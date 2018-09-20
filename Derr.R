# D-error
# 
# Function to calculate d error given a design, and parameter values.
# @param par Vector containing parameter values.
# @param des A design matrix in which each row is a profile.
# @param n.alts Numeric value indicating the number of alternatives per choice
#   set.
# @return D-error.
Derr <- function(par, des, n.alts) {
  info.des <- InfoDes(par, des, n.alts)  # Calculates Fisher-Information matrix
  detinfo <- det(info.des)  # Calculates the determinant of the information matrix
  ifelse((detinfo <= 0), return(NA), return(detinfo^(-1 / length(par)))) # Return the d-error
}

Derr2 <- function(par, des, n.alts) {
  info.des <- InfoDes_cpp(par, des, n.alts)  # Calculates Fisher-Information matrix
  detinfo <- det(info.des) # Calculates the determinant of the information matrix
  ifelse((detinfo <= 0), return(NA), return(detinfo^(-1 / length(par)))) # Return the d-error
}