# Likelihood function
#
# @param par Numeric vector with parametervalues.
# @param des A design matrix in which each row is a profile.
# @param n.alts The number of alternatives in each choice set
# @param y A binary response vector.
# @return the likelihood
Lik <- function(par, des, n.alts, y) {
  # utility
  des <- as.matrix(des)
  u <- t(t(des) * par)
  u <- .rowSums(u, m = nrow(des), n = length(par))
  # probability
  expu <- exp(u)
  p <- expu / rep(rowsum(expu, rep(seq(1, nrow(des) / n.alts, 1), each = n.alts)), each = n.alts)
  # likelihood
  L <- prod(p^y)
  return(L)
}