# Sequential DB function for shiny
# 
# Small changes in alt.cte argument in comparison with the \code{SeqDB}
# function. This way the function can be easily used in the SurveyApp function
SeqDBApp <- function(des, cand.set, n.alts, par.draws, prior.covar, alt.cte, reduce = TRUE, w = NULL) {
  # Initialize.
  n.sets <- nrow(des) / n.alts
  # If no w, equal w.
  if (is.null(w)) {
    w <- rep(1, nrow(par.draws))
  }
  # Create alternative specific design if necessay.
  if (!all(alt.cte == 0)) {
    cte.set <- Altspec(alt.cte = alt.cte, n.sets = 1)
    cte.des <- Altspec(alt.cte = alt.cte, n.sets = n.sets)
    kcte <- ncol(cte.des)
    fdes <- cbind(cte.des, des)
  } else {
    cte.set <- NULL
    kcte <- 0
    fdes <- des
  }
  # Error handling cte.des
  if (ncol(cand.set) + kcte != ncol(par.draws)) {
    stop("dimension of par.draws does not match the dimension of alt.cte + cand.set.")
  }
  # Handling par.draws.
  if (!(is.matrix(par.draws))) {
    par.draws <- matrix(par.draws, nrow = 1)
  }
  # Error identifying model.
  if (n.sets < ncol(par.draws)) {
    stop("Model is unidentified. Increase the number of choice sets or decrease parameters to estimate.")
  }
  # Error par.draws
  if (ncol(fdes) != ncol(par.draws)) {
    stop("Numbers of parameters in par.draws does not match the number of parameters in the design.")
  }
  # Starting and initializing values.
  i.cov <- solve(prior.covar)
  d.start <- apply(par.draws, 1, Derr, des = fdes,  n.alts = n.alts)
  db.start <- mean(d.start, na.rm = TRUE)
  full.comb <- gtools::combinations(n = nrow(cand.set), r = n.alts, repeats.allowed = !reduce)
  n.par <- ncol(par.draws)
  # For each potential set, select best. 
  db.errors <- apply(full.comb, 1, DBerrS, cand.set, par.draws, fdes, n.alts, cte.set, i.cov, n.par, w)
  comb.nr <- as.numeric(full.comb[which.min(db.errors), ])
  set <- cand.set[comb.nr, ]
  row.names(set) <- NULL
  db <- min(db.errors)
  #return best set and db error design.
  return(list(set = set, db.error = db))
}