# Hessian
#
# @param par Numeric vector with parametervalues.
# @param des A design matrix in which each row is a profile.
# @param covar The covariance matrix.
# @param n.alts The number of alternatives in each choice set.
# @return the hessian matrix
Hessian <- function(par, des, covar, n.alts) {
  # utility
  des <- as.matrix(des)
  u <- des %*% diag(par)
  u <- .rowSums(u, m = nrow(des), n = length(par))
  # probability 
  expu <- exp(u)
  p <- expu / rep(rowsum(expu, rep(seq(1, nrow(des) / n.alts, 1), each = n.alts)), each = n.alts)
  # information matrix
  info <- crossprod(des * p, des) - crossprod(rowsum(des * p, rep(seq(1, nrow(des) / n.alts, 1), each = n.alts)))
  # hessian 
  hess <- (-info - solve(covar))
  return(hess)
}