#' Sequential Kullback-Leibler based algorithm for the MNL model.
#' 
#' Selects the choice set that maximizes the Kullback-Leibler divergence between
#' prior parameter values and the expected posterior, assuming an MNL model.
#' 
#' The algorithm selects the choice set that maximizes the Kullback-Leibler 
#' divergence between prior and expected posterior. Otherwisely framed the 
#' algorithm selects the choice set that maximizes the expected information 
#' gain.
#' @inheritParams SeqDB
#' @param alt.cte A binary vector indicating for each alternative if an
#'   alternative specific constant is desired.
#' @param reduce Logical value indicating whether the candidate set should be 
#'   reduced or not.
#' @return \item{set}{Numeric matrix containing the choice set that maximizes the expected KL divergence.}
#' \item{kl}{Numeric value which is the Kullback leibler divergence.}
#' @importFrom Rdpack reprompt
#' @references 
#' \insertRef{crabbe}{mnldes}
#' @examples 
#' # KL efficient choice set, given parameter draws. 
#' # Candidate profiles 
#' cs <- Profiles(lvls = c(3, 3), coding = c("E", "E"))
#' m <- c(0.3, 0.2, -0.3, -0.2) # Prior mean (4 parameters).
#' pc <- diag(length(m)) # Prior variance
#' set.seed(123)
#' ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = pc) # 10 draws.
#' ac <- c(0, 0) # No alternative specific constants. 
#' # Efficient choice set to add. 
#' SeqKL(cand.set = cs, n.alts = 2, alt.cte = ac, par.draws = ps, weights = NULL)
#' 
#' # KL efficient choice set, given parameter draws. 
#' # Candidate profiles 
#' cs <- Profiles(lvls = c(3, 3), coding = c("C", "E"), c.lvls = list(c(5,3,1)))
#' m <- c(0.7, 0.3, -0.3, -0.2) # Prior mean (4 parameters).
#' pc <- diag(length(m)) # Prior variance
#' set.seed(123)
#' ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = pc) # 10 draws.
#' ac <- c(1, 0) # Alternative specific constant. 
#' # Efficient choice set to add. 
#' SeqKL(cand.set = cs, n.alts = 2, alt.cte = ac, par.draws = ps, weights = NULL)
#' @export
SeqKL <- function(cand.set, n.alts, alt.cte, par.draws, weights, reduce = TRUE) {
  # Handling par.draws.
  if (!(is.matrix(par.draws))) {
    par.draws <- matrix(par.draws, nrow = 1)
  }
  # Error alternative specific constants. 
  if (length(alt.cte) != n.alts) {
    stop("n.alts does not match the alt.cte vector")
  }
  # Create alternative specific design. 
  cte.des <- Altspec(alt.cte = alt.cte, n.sets = 1)
  # Error handling cte.des
  if (ncol(cand.set) + ncol(cte.des) != ncol(par.draws)) {
    stop("dimension of par.draws does not match the dimension of alt.cte + cand.set.")
  }
  # All choice sets.
  full.comb <- gtools::combinations(n = nrow(cand.set), r = n.alts, repeats.allowed = !reduce)
  # If no weights, equal weights.
  if (is.null(weights)) {
    weights <- rep(1, nrow(par.draws))
  }
  # Calculate KL for each set. 
  kl.infos <- apply(full.comb, 1, KLs, par.draws, cte.des, cand.set, weights)
  # Select maximum.
  comb.nr <- as.numeric(full.comb[which.max(kl.infos), ])
  set <- cand.set[comb.nr, ]
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- cbind(cte.des, set)
  }
  row.names(set) <- NULL
  # return.
  return(list(set = set, kl = max(kl.infos)))
}


