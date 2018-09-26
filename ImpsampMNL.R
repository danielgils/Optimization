ImpsampMNL <- function (prior.mean, prior.covar, des, n.alts, y, m, b = 2){
  if (length(prior.mean) != ncol(prior.covar)) {
    stop("different number of parameters in prior mean and prior covarance matrix.")
  }
  if (nrow(des) != length(y)) {
    stop("response vector length differs from the expected based on design")
  }
  if (det(prior.covar) == 0) {
    stop("prior covariance matrix is not invertible")
  }
  kPrior <- (2 * pi)^(-length(prior.mean)/2) * (det(prior.covar))^(-0.5)
  maxest <- maxLik::maxNR(LogPost, start = prior.mean, prior.mean = prior.mean, 
                          prior.covar = prior.covar, des = des, y = y, n.alts = n.alts)$estimate
  hes <- Hessian(par = maxest, des = des, covar = prior.covar, 
                 n.alts = n.alts)
  g.covar <- -solve(hes)
  g.draws <- Lattice_mvt(mean = maxest, cvar = g.covar, df = length(maxest), 
                         m = m)
  prior <- likh <- dens.g <- weights <- numeric(nrow(g.draws))
  spvc <- solve(prior.covar)
  for (r in 1:nrow(g.draws)) {
    prior[r] <- kPrior * exp(-0.5 * (g.draws[r, ] - prior.mean) %*% 
                               spvc %*% as.matrix(g.draws[r, ] - prior.mean))
    likh[r] <- Lik(par = g.draws[r, ], des = des, y = y, 
                   n.alts = n.alts)
    dens.g[r] <- Gdens(par = g.draws[r, ], g.mean = maxest, 
                       g.covar = g.covar)
  }
  w <- likh * prior/dens.g
  w <- w/sum(w)
  return(list(sample = g.draws, weights = w, max = maxest, 
              covar = g.covar))
}