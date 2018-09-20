SeqDB3 <- function(des, cand.set, n.alts, par.draws, prior.covar, reduce = TRUE, weights = NULL) {
  # Initialize.
  n.sets <- nrow(des) / n.alts
  cte.des <- NULL
  # If no weights, equal weights.
  if (is.null(weights)) {
    weights <- rep(1, nrow(par.draws))
  }
  # Detect alternative specific constants
  des.f <- as.data.frame(des)
  alt.cte <- dplyr::select(des.f, dplyr::contains(".cte"))
  if (ncol(alt.cte) > 0) {
    cte.des <- alt.cte[1:n.alts, ]
  }
  # Handling par.draws.
  if (!(is.matrix(par.draws))) {
    par.draws <- matrix(par.draws, nrow = 1)
  }
  # Error par.draws
  if (ncol(des) != ncol(par.draws)) {
    stop("Numbers of parameters in par.draws does not match the number of parameters in the design.")
  }
  # Error identifying model.
  if (n.sets < ncol(par.draws)) {
    stop("Model is unidentified. Increase the number of choice sets or decrease parameters to estimate.")
  }
  # Starting and initializing values.
  i.cov <- solve(prior.covar)
  d.start <- apply(par.draws, 1, Derr2, des = des,  n.alts = n.alts)
  db.start <- mean(d.start, na.rm = TRUE)
  full.comb <- gtools::combinations(n = nrow(cand.set), r = n.alts, repeats.allowed = !reduce)
  n.par <- ncol(par.draws)
  # For each potential set, select best. 
  db.errors <- apply(full.comb, 1, DBerrS3, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights)
  comb.nr <- as.numeric(full.comb[which.min(db.errors), ])
  set <- cand.set[comb.nr, ]
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- cbind(cte.des, set)
  }
  row.names(set) <- NULL
  db <- min(db.errors)
  #return best set and db error design.
  return(list(set = set, db.error = db))
}