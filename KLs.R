# KL information
# 
# Calculates the Kullback-Leibler divergence for all posible choice sets, given
# @inheritParams SeqDB
# @param full.comb A matrix in which each row is a possible combination of
#   profiles.
# @return Numeric value indicating the Kullback-Leibler divergence.
KLs <- function(full.comb, par.draws, cte.des, cand.set, weights) {
  #take set
  set <- as.matrix(cand.set[as.numeric(full.comb), ])
  #Alternative specific constants 
  set <- cbind(cte.des, set)
  #calculate for all sets the KLinfo.
  kl <- KL(set, par.draws, weights)
  return(kl)
}