# Density multivariate t-distribution
#
# @param par Numeric vector with parametervalues.
# @param g.mean vector containing the mean of the multivariate t-distribution.
# @param g.covar covariance matrix of the multivariate t-distribution.
# @return density
Gdens <- function(par, g.mean, g.covar) {
  df <- length(g.mean)
  n <- length(par)
  dif <- g.mean - par
  invcov <- solve(g.covar)
  differ <- as.numeric(t(dif) %*% invcov %*% dif)
  iMVSTd <- 1 / (det(g.covar)^(0.5)) * (1 + ((1 / df) * differ))^(-(df + length(par)) / 2)
}