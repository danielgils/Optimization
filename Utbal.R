# Utility balance 
Utbal <- function(par, des, n.alts) { 
  group <- rep(seq(1, nrow(des) / n.alts, 1), each = n.alts)
  u <- des %*% diag(par)
  u <- .rowSums(u, m = nrow(des), n = length(par))
  p <- exp(u) / rep(rowsum(exp(u), group), each = n.alts)
  ub <- by(p, group, function(x) {max(x) - min(x)}, simplify = TRUE)
  # return
  return(ub)
}