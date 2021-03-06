# Sequential D-error
# 
# Function to calculate D-errors if set would be part of design.
# @inheritParams Modfed
# @param set A choice set in which each row is a profile.
# @param des A design matrix in which each row is a profile.
# @param i.cov Inverse of covariance matrix.
# @param n.par Number of parameters.
DerrS <- function(par.draws, set, des, n.alts, i.cov, n.par) {
  des.f <- rbind(des, set)  # Append of optimal design with new alternatives
  info.d <- InfoDes(par = par.draws, des = des.f, n.alts = n.alts) 
  d.error <- det(info.d + i.cov)^(-1 / n.par)
  return(d.error)
}

# Function using Info_des_cpp
DerrS2 <- function(par.draws, set, des, n.alts, i.cov, n.par) {
  des.f <- rbind(des, set) 
  info.d <- InfoDes_cpp(par = par.draws, des = des.f, n_alts = n.alts) 
  d.error <- det(info.d + i.cov)^(-1 / n.par)
  return(d.error)
}

# Function using Info_des_cpp and det_cpp
DerrS3 <- function(par.draws, set, des, n.alts, i.cov, n.par) {
  des.f <- rbind(des, set) 
  info.d <- InfoDes_cpp(par = par.draws, des = des.f, n_alts = n.alts) 
  #d.error <- det(info.d + i.cov)^(-1 / n.par)
  d.error <- det_cpp(info.d + i.cov)^(-1 / n.par)
  return(d.error)
}