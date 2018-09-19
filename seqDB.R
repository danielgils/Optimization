#' Sequential modified federov algorithm for MNL model.
#' 
#' Selects the choice set that minimizes the DB-error when added to an initial
#' design, given (updated) parameter values.
#' 
#' This algorithm is ideally used in an adaptive context. The algorithm will 
#' select the next DB-efficient choice set given parameter values and an initial
#' design. In an adaptive context these parameter values are updated after each 
#' observed response.
#' 
#' The initial design \code{des} can be generated with \code{\link{Modfed}}. If 
#' alternative specific constants are included in the initial design, the 
#' algorithm will use the same for selecting the new choice set. Columns of 
#' \code{des} which contain ".cte" in their name are recognized as alternative 
#' specific columns.
#' 
#' The list of potential choice sets are created using 
#' \code{\link[gtools]{combinations}}. If \code{reduce} is \code{TRUE}, 
#' \code{repeats.allowed = FALSE} and vice versa. If no alternative constants
#' are used \code{reduce} should always be \code{TRUE}. When alternative
#' specific constants are used \code{reduce} can be \code{TRUE} so that the
#' algorithm will be faster, but the combinations of constants and profiles will
#' not be evaluated exhaustively.
#' 
#' The \code{weights} can be used when the \code{par.draws} have weights. This
#' is for example the case when parameter values are updated using 
#' \code{\link{ImpsampMNL}}.
#' @inheritParams Modfed
#' @param par.draws A matrix in which each row is a sample from the 
#'   multivariate parameter distribution. See also \code{\link{ImpsampMNL}}.
#' @param des A design matrix in which each row is a profile. Can be generated
#'   with \code{\link{Modfed}}
#' @param prior.covar Covariance matrix of the prior distribution.
#' @param reduce Logical value indicating whether the candidate set should be
#'   reduced or not.
#' @param weights A vector containing the weights of the draws. Default is
#'   \code{NULL}, See also \code{\link{ImpsampMNL}}.
#' @return \item{set}{A matrix representing a DB efficient choice set.} 
#' \item{db.error}{A numeric value indicating the DB-error of the whole design.}
#' @examples 
#' # DB efficient choice set, given a design and parameter draws. 
#' # Candidate profiles 
#' cs <- Profiles(lvls = c(3, 3), coding = c("E", "E"))
#' m <- c(0.3, 0.2, -0.3, -0.2) # Prior mean (total = 5 parameters).
#' pc <- diag(length(m)) # Prior variance
#' set.seed(123)
#' ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = pc) # 10 draws.
#' ac <- c(0, 0) # No alternative specific constants. 
#' # Initial design.
#' des <- Modfed(cand.set = cs, n.sets = 6, n.alts = 2, alt.cte = ac, par.draws = ps)$design
#' # Efficient choice set to add. 
#' SeqDB(des = des, cand.set = cs, n.alts = 2, par.draws = ps, prior.covar = pc)
#' 
#' # DB efficient choice set, given a design and parameter draws. 
#' # Candidate profiles 
#' cs <- Profiles(lvls = c(3, 3), coding = c("C", "E"), c.lvls = list(c(5,3,1)))
#' m <- c(0.7, 0.3, -0.3, -0.2) # Prior mean (4 parameters).
#' pc <- diag(length(m)) # Prior variance
#' set.seed(123)
#' ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = pc) # 10 draws.
#' ac <- c(1, 0) # Alternative specific constant. 
#' # Initial design.
#' des <- Modfed(cand.set = cs, n.sets = 6, n.alts = 2, alt.cte = ac, par.draws = ps)$design
#' # Efficient choice set to add. 
#' SeqDB(des = des, cand.set = cs, n.alts = 2, par.draws = ps, prior.covar = pc)
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{ju}{mnldes} 
#' @export
SeqDB <- function(des, cand.set, n.alts, par.draws, prior.covar, reduce = TRUE, weights = NULL) {
  # Initialize.
  n.sets <- nrow(des) / n.alts
  cte.des <- NULL
  # If no weights, equal weights.
  if (is.null(weights)) {
    weights <- rep(1, nrow(par.draws))
  }
  # Detect alternative specific constants
  des.f <- as.data.frame(des)
  alt.cte <- dplyr::select(des.f, dplyr::contains(".cte"))
  if (ncol(alt.cte) > 0) {
    cte.des <- alt.cte[1:n.alts, ]
  }
  # Handling par.draws.
  if (!(is.matrix(par.draws))) {
    par.draws <- matrix(par.draws, nrow = 1)
  }
  # Error par.draws
  if (ncol(des) != ncol(par.draws)) {
    stop("Numbers of parameters in par.draws does not match the number of parameters in the design.")
  }
  # Error identifying model.
  if (n.sets < ncol(par.draws)) {
    stop("Model is unidentified. Increase the number of choice sets or decrease parameters to estimate.")
  }
  # Starting and initializing values.
  i.cov <- solve(prior.covar)
  d.start <- apply(par.draws, 1, Derr, des = des,  n.alts = n.alts)
  db.start <- mean(d.start, na.rm = TRUE)
  full.comb <- gtools::combinations(n = nrow(cand.set), r = n.alts, repeats.allowed = !reduce)
  n.par <- ncol(par.draws)
  # For each potential set, select best. 
  db.errors <- apply(full.comb, 1, DBerrS, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights)
  comb.nr <- as.numeric(full.comb[which.min(db.errors), ])
  set <- cand.set[comb.nr, ]
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- cbind(cte.des, set)
  }
  row.names(set) <- NULL
  db <- min(db.errors)
  #return best set and db error design.
  return(list(set = set, db.error = db))
}