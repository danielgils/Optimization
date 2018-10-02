# log Posterior
# 
# Calculates the logposterior with a normal prior density par Numeric vector
# with parametervalues.
# @param des A design matrix in which each row is a profile.
# @param y A binary response vector.
# @param n.alts The number of alternatives in each choice set.
# @param prior.mean vector containing the prior mean.
# @param prior.covar matrix containing the prior covariance.
# @param lower Numeric vector. Vector of lower truncation points, the default
#   is \code{rep(-Inf, length(prior.mean))}.
# @param upper Numeric vector. Vector of upper truncation points, the default
#   is \code{rep(Inf, length(prior.mean))}. 
# @return the logposterior probability
LogPost <- function(par, prior.mean, prior.covar, lower, upper,  des,  n.alts, y) {
  #calcultate utility alternatives
  u <- t(t(des) * par)
  u <- .rowSums(u, m = nrow(des), n = length(par))
  #calculate probability alternatives
  expu <- exp(u)
  p <- expu / rep(rowsum(expu, rep(seq(1, nrow(des)/n.alts, 1), each = n.alts)), each = n.alts)
  #loglikelihood
  ll <- sum(y * log(p))
  #logprior
  lprior <- tmvtnorm::dtmvnorm(par, prior.mean, prior.covar, lower, upper, log = TRUE)
  #logposterior 
  return(lprior + ll)
}