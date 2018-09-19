#' Response generation
#' 
#' Function to generate responses given parameter values and a design matrix, 
#' assuming a MNL model.
#' @param par Numeric vector containing parameter values.
#' @inheritParams SeqDB
#' @param bin Indicates whether the returned value should be a binary vector or 
#'   a discrete value which denotes the chosen alternative.
#' @return Numeric vector indicating the chosen alternatives.
#' @examples 
#' # 3 Attributes, 2 are dummy coded and 1 continuous.
#' cs <- Profiles(lvls = c(2, 3, 2), coding = c("D", "C", "D"), c.lvls = list(c(2,4,6)))
#' p <- c(0.8, 0.2, -0.3) # parameter vector
#' # Generate design
#' des <- Modfed(cand.set = cs, n.sets = 8, n.alts = 2, alt.cte = c(0,0), par.draws = p)$des
#' # Generate responses
#' y <- RespondMNL(par = p, des = des, n.alts = 2)
#' @export
RespondMNL <- function(par, des, n.alts, bin = TRUE) {
  # Error par is not vector
  if (!is.vector(par)) {
    stop('par should be a vector.')
  }
  # Error n.alts 
  if ((nrow(des) %% n.alts) != 0) {
    stop('number of rows in des is not a multiple of n.alts.')
  }
  # Error par
  if (ncol(des) != length(par)) {
    stop("length of par vector does not match the number of parameters in the design.")
  }
  # Probability
  group <- rep(seq(1, nrow(des) / n.alts, 1), each = n.alts)
  u <- des %*% diag(par)
  u <- .rowSums(u, m = nrow(des), n = length(par))
  p <- exp(u) / rep(rowsum(exp(u), group), each = n.alts)
  # Choice
  n.sets <- nrow(des) / n.alts
  draws <- (0:(n.sets-1)) + (stats::runif(n.sets))
  choice <- findInterval(x = draws, vec = c(0, cumsum(p)))
  Y <- rep(0, length(p))
  Y[choice] <- 1
  # Return
  ifelse(bin, return(Y), return(choice))
}