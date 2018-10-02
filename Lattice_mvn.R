# Lattice multivariate normal distribution.
# 
# Generates a grid of points coming from a multivariate normal distribution.
# @param mean Numeric vector indicating the multivariate mean.
# @param cvar A matrix which specifies the covariance matrix.
# @param m Numeric value. Number of draws = b^m.
# @param b Numeric value indicating the base (default = 2).
# @return Matrix of lattice points drawn from a multivariate normal
#   distribution. Each row is a draw.
Lattice_mvn <- function(mean, cvar, m, b=2) {
  dim <- length(mean)
  l <- Lat(K = dim, b, m)
  mean <- t(mean)
  X <- matrix(NA, nrow(l), dim)
  A <- chol(cvar)
  for (i in 1:nrow(l)) {
    Z <- l[i, ]
    r <- mean + (Z %*% t(A))
    X[i, ] <- t(r)
  }
  return(X)
}
