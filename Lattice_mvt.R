# Lattice multivariate t-distribution.
# 
# Generates a grid of points coming from a multivariate t-distribution.
# @param mean Numeric vector indicating the multivariate mean.
# @param cvar A matrix which specifies the covariance matrix.
# @param df Numeric value indicating the degrees of freedom for the multivariate
#   t-distribution.
# @param m Numeric value. Number of draws = b^m.
# @param b Numeric value indicating the base (default = 2).
# @return Matrix of lattice points drawn from a multivariate t-distribution.
#   Each row is a draw.
Lattice_mvt <- function (mean, cvar, df, m, b=2) {
  # Dimension
  dim <- length(mean)
  # Generate lattice from standard normal
  lattice <- Lat(K = dim, b, m)
  mean <- t(mean)
  X <- matrix(NA, nrow(lattice), dim)
  A <- chol(cvar)
  # Transform to multivariate t distribution
  for (i in 1:nrow(lattice)) {
    inv <- stats::rgamma(1, df / 2)
    invw <- inv / (df / 2)
    W <- 1 / invw
    Z <- lattice[i, ]
    r <- mean + sqrt(W) * (Z %*% t(A))
    X[i, ] <- t(r)
  }
  return (X)
}
