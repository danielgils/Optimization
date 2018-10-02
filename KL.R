# Kullback-Leibler divergence for a set
# 
# Calculates the KL-divergence for a choice set given parameter values.
# @inheritParams DerrS
# @param weights A vector containing the weights of the draws. Default is
#   \code{NULL}
KL <- function (set, par.draws, weights){
  # Probabilities.
  num2 <- tcrossprod(set, par.draws)
  mmat2 <- as.matrix(t(apply(num2, 2, max)))
  numm2 <- exp(sweep(num2, 2, mmat2, FUN = "-"))
  nummax <- exp(sweep(num2, 2, mmat2, FUN = "-"))
  denom <- colSums(numm2)
  ps <- sweep(nummax, 2, denom, FUN = "/")
  lgp <- log(ps)
  wprob <- sweep(ps, 2, weights, FUN="*")
  twp <- rowSums(wprob)
  lgwp <- sweep(lgp, 2, weights, FUN="*")
  tlwp <- rowSums(lgwp)
  #kullback Leibler information
  klinfo <- sum(twp * (log(twp) - tlwp))
  return (as.numeric(klinfo))
}