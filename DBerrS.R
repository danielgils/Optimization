# Sequential DB-error
# 
# Function to calculate DB-errors for potential choice sets in combination with 
# an initial design.
# @inheritParams Modfed
# @inheritParams DerrS
# @param full.comb A matrix with on each row a possible combination of profiles.
# @param cte.des A matrix which represent the alternative specific constants. If
#   there are none it value is \code{NULL}.
# @return The DB errors of the designs in which each design is a combination 
#   with of the initial design with a potential choice set.
DBerrS <- function(full.comb, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights) {
  # Take set.
  set <- as.matrix(cand.set[as.numeric(full.comb), ])
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- as.matrix(cbind(cte.des, set))
  }
  # For each draw calculate D-error.
  d.errors <- apply(par.draws, 1, DerrS, set, des, n.alts, i.cov, n.par)
  w.d.errors <- d.errors * weights
  # DB-error. 
  db.error <- mean(w.d.errors, na.rm = TRUE)
  return(db.error)
}

# Function using only Info_des_cpp
DBerrS2 <- function(full.comb, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights) {
  # Take set.
  set <- as.matrix(cand.set[as.numeric(full.comb), ])
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- as.matrix(cbind(cte.des, set))
  }
  # For each draw calculate D-error.
  d.errors <- apply(par.draws, 1, DerrS2, set, des, n.alts, i.cov, n.par)
  w.d.errors <- d.errors * weights
  # DB-error. 
  db.error <- mean(w.d.errors, na.rm = TRUE)
  return(db.error)
}

#Function using Info_des_cpp and DerrS_cpp
DBerrS3 <- function(full.comb, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights) {
  # Take set.
  set <- as.matrix(cand.set[as.numeric(full.comb), ])
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- as.matrix(cbind(cte.des, set))
  }
  # For each draw calculate D-error.
  d.errors <- apply(par.draws, 1, DerrS_cpp, set, des, n.alts, i.cov, n.par)
  w.d.errors <- d.errors * weights
  # DB-error. 
  db.error <- mean(w.d.errors, na.rm = TRUE)
  return(db.error)
}

# Function using Info_des_Cpp and det_cpp
DBerrS4 <- function(full.comb, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights) {
  # Take set.
  set <- as.matrix(cand.set[as.numeric(full.comb), ])
  # Add alternative specific constants if necessary
  if (!is.null(cte.des)) {
    set <- as.matrix(cbind(cte.des, set))
  }
  # For each draw calculate D-error.
  d.errors <- apply(par.draws, 1, DerrS3, set, des, n.alts, i.cov, n.par)
  w.d.errors <- d.errors * weights
  # DB-error. 
  db.error <- mean(w.d.errors, na.rm = TRUE)
  return(db.error)
}