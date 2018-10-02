# Lattice multivariate standard normal distribution.
# 
# Generates a grid of points coming from a multivariate standard normal
# distribution.
# @param K Numeric value indicating the dimensionality of the grid.
# @param b Numeric value indicating the base.
# @param m Numeric value. Number of draws = b^m.
# @return Matrix of lattice points drawn from a multivariate standard normal
#   distribution. Each row is a draw.
Lat <- function(K, b, m) {
  base <- function(num){
    a1 <- c1 <- rep(0, m)
    a1[m] <- c1[m] <- num %% b
    for(j in seq(m - 1, 1, by = -1)){
      tem <- (num - c1[j + 1]) / (b^(m - j))
      a1[j] <- tem %% b
      c1[j] <- c1[j + 1] + a1[j] * (b^(m - j))
    }
    return(a1)
  }
  a <- 1571
  N <- b^m   #number of lattice points
  u <- stats::runif(K)
  av <- rep(1, K)
  for (i in 2:K) {
    av[i] <- (a * av[i - 1]) %% N
  }
  e <- matrix(0, N, K)
  seq <- rep(0, m)
  kk <- -m
  for (k in 1:m) {
    seq[k] <- b^kk
    kk <- kk + 1
  }
  for (i in 1:N) {
    ei <- c(crossprod(seq, base(i - 1))) * av + u
    e[i, ] <- ei - floor(ei)
  }
  latt <- matrix(0, N, K)
  for (i in 1:K) {
    for (j in 1:N) {
      latt[j, i] <- 1 - abs(2 * e[j, i] - 1)
    }
  }
  latt <- stats::qnorm(latt)
  return(latt)
}