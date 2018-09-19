#' Modified Federov algorithm for MNL models.
#' 
#' The algorithm swaps every profile of an initial design with candidate 
#' profiles. By doing this it tries to minimize the D(B)-error, based on a
#' multinomial logit model.
#' 
#' Each iteration will loop through all profiles from the initial design,
#' evaluating the change in D(B)-error for every profile from \code{cand.set}. 
#' The algorithm stops when an iteration occured without replacing a profile or 
#' when \code{max.iter} is reached.
#' 
#' By specifying a numeric vector in \code{par.draws}, the D-error will be 
#' calculated and the design will be optimised locally. By specifying a matrix, 
#' in which each row is a draw from a multivariate distribution, the DB-error 
#' will be calculated, and the design will be optimised globally. The number of 
#' columns should equal the number of parameter in \code{alt.cte} + the number
#' of parameters in \code{cand.set}. This is also the order in which they should
#' be sorted (first \code{alt.cte} parameters).
#' 
#' The DB-error is calculated by taking the mean over D-errors. It could be that
#' for some draws the design results in an infinite D-error. The percentage of 
#' draws for which this was true for the final design can be found in the 
#' output \code{inf.error}.
#' 
#' Alternative specific constants can be specified in \code{alt.cte}. The lenght
#' of this binary vector should equal \code{n.alts}, were \code{0} indicates the
#' absence of an alternative specific constant and \code{1} the opposite.
#' 
#' @param cand.set A numeric matrix in which each row is a possible profile. The
#'   \code{\link{Profiles}} function can be used to generate this.
#' @param n.sets Numeric value indicating the number of choice sets.
#' @param n.alts Numeric value indicating the number of alternatives per choice 
#'   set.
#' @param alt.cte A binary vector indicating for each alternative if an 
#'   alternative specific constant is desired.
#' @param par.draws A numeric vector containing the parameter values, or a
#'   numeric matrix in which each row is a draw from the multivariate prior
#'   parameter distribution.
#' @param start.des A matrix in which each row is a profile. The number of rows 
#'   equals \code{n.sets * n.alts}, and the number of columns equals the number 
#'   of columns of \code{cand.set}. If not specified a random start design will
#'   be generated.
#' @param max.iter A numeric value indicating the maximum number allowed 
#'   iterations.
#' @return \item{design}{A numeric matrix wich contains an efficient design.} 
#' \item{error}{Numeric value indicating the D(B)-error of the design.} 
#' \item{inf.error}{Numeric value indicating the percentage of draws for which
#' the D-error was \code{Inf}.} \item{prob.diff}{Numeric value indicating the
#' difference between the alternative with the highest and the one with the
#' lowest probability for each choice set. If a sample matrix was provided this
#' is based on the average over all draws.}
#' @examples
#' # D-efficient design
#' # 3 Attributes, 2 are dummy coded and 1 continuous (= 3 parameters).
#' cs <- Profiles(lvls = c(2, 3, 2), coding = c("D", "C", "D"), c.lvls = list(c(2, 4, 6)))
#' ps <- c(0.8, 0.2, -0.3) # Prior parameter vector
#' Modfed(cand.set = cs, n.sets = 8, n.alts = 2, alt.cte = c(0, 0), par.draws = ps)
#' 
#' # DB-efficient design. 
#' # 3 Attributes with 2, 3 and 2 levels, all effect coded (= 4 parameters).
#' cs <- Profiles(lvls = c(2, 3, 2), coding = c("E", "E", "E")) 
#' m <- c(0.8, 0.2, -0.3, -0.2, 0.7) # Prior mean (total = 5 parameters).
#' v <- diag(length(m)) # Prior variance.
#' set.seed(123) 
#' ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v) # 10 draws.
#' Modfed(cand.set = cs, n.sets = 8, n.alts = 2, alt.cte = c(1, 0), par.draws = ps)
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{federov}{mnldes} 
#' @export
Modfed <- function(cand.set, n.sets, n.alts,  alt.cte, par.draws, start.des = NULL, max.iter = Inf) {
  # Handling par.draws.
  if (!(is.matrix(par.draws))) {
    par.draws <- matrix(par.draws, nrow = 1)
  }
  # Errors alternative specific constants. 
  if (length(alt.cte) != n.alts) {
    stop("n.alts does not match the alt.cte vector")
  }
  if (!all(alt.cte %in% c(0,1))){
    stop("alt.cte should only contain 0s or 1s.")
  }
  # Error identifying model.
  if (n.sets < ncol(par.draws)) {
    stop("Model is unidentified. Increase the number of choice sets or decrease parameters to estimate.")
  }
  # Errors start design. 
  if (!is.null(start.des)) {
    if (ncol(start.des) != ncol(cand.set)) {
      stop("number of colums start design is different from number of columns candidate set.")
    }
    if (nrow(start.des) != (n.alts * n.sets)) {
      stop("number of rows start design is different from number of sets times number of alternatives.")
    }
  }
  # Create alternative specific design.
  cte.des <- Altspec(alt.cte = alt.cte, n.sets = n.sets)
  # Error handling cte.des
  if (ncol(cand.set) + ncol(cte.des) != ncol(par.draws)) {
    stop("The number of parameters in par.draws does not match the number of parameters for alt.cte + the number of paramters in cand.set.")
  }
  # Random start design.
  db.start <- NA
  while (is.na(db.start)) {
    if (!is.null(start.des)) {
      des <- start.des
    } else {
      r <- round(stats::runif((n.sets * n.alts), 1, nrow(cand.set)))
      des <- data.matrix(cand.set[r, ])
    }
    # Combine with alt.spec design.
    des <- cbind(cte.des, des)
    # Starting values. 
    d.start <- apply(par.draws, 1, Derr, des = des,  n.alts = n.alts)
    db.start <- mean(d.start, na.rm = TRUE)
    if (!is.null(start.des) && is.na(db.start)) {
      stop("the provided start design results in an unvalid db-error.")
    }
  }
  converge <- FALSE
  change <- FALSE
  it <- 1
  n.samples <- nrow(par.draws)
  n.cte <- ncol(cte.des)
  n.par <- ncol(des)
  # start algorithm.
  while (!converge & it <= max.iter) {
    it <- it + 1
    # show progress iteration.
    if (interactive()) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(des), style = 3)
    }
    # save design before iteration.
    iter.des <- des
    # For every row in the design.
    for (r in 1:nrow(des)) {
      # Switch with everey row in candidate set. 
      for (c in 1:nrow(cand.set)) {
        des[r, (n.cte + 1) : n.par ] <- cand.set[c, ]
        # Calculate D-errors.
        d.errors <- apply(par.draws, 1, Derr, des = des,  n.alts = n.alts)
        # DB-error. 
        db <- mean(d.errors, na.rm = TRUE)
        # Change if lower db error.
        if (!is.na(db) && !is.na(db.start)) {
          if (db < db.start) {
            best.row <- as.numeric(des[r, ])
            db.start <- db
            change <- TRUE
          }
        }
        # Show progress iteration.
        if (interactive()) {
          utils::setTxtProgressBar(pb, r)
        }
      }
      # Replace with best profile if change.
      if (change) {
        des[r, ] <- best.row
      } else {
        des[r, ] <- iter.des[r, ]
      }
      # Initialize variables again. 
      change <- FALSE
      na.percentage <- 0
    }
    if (interactive()) {
      close(pb)
    } 
    converge <- isTRUE(all.equal(des, iter.des)) # Convergence if no profile is swapped this iteration.
  }
  # calculate percentage NA values.
  d.errors <- apply(par.draws, 1, Derr, des = des,  n.alts = n.alts)
  if (any(is.na(d.errors))) {
    na.percentage <- scales::percent(sum(is.na(d.errors)) / n.samples)
  } 
  # Utility balance.
  ub <- apply(par.draws, 1, Utbal, des = des,  n.alts = n.alts)
  ub <- .rowMeans(ub, m = n.sets, n = n.samples, na.rm = FALSE)
  # Rownames design. 
  des.names <- Rcnames(n.sets = n.sets, n.alts = n.alts, n.cte = n.cte, alt.cte = alt.cte)
  rownames(des) <- des.names[[1]]
  # Colnames alternative specific constants. 
  if (n.cte != 0 && !is.null(colnames(des))) {
    colnames(des)[1:n.cte] <- des.names[[2]]
  }
  # Return design, D(B)error, percentage NA's, utility balance. 
  return(list("design" = des, "error" =  db.start, "inf.error" = na.percentage, "prob.diff" = ub))
}
