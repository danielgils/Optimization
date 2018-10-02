#' ---
#' title: "Profilling seqDB function"
#' author: "Daniel Gil"
#' date: "`Sep 2018"
#' output: github_document
#' ---
#' 

# Remove all elements
rm(list = ls())


# Load packages
library(microbenchmark)
library(profvis)
library(Rcpp)
library(RcppArmadillo)
# library(inline)
#----
source("seqDB.R")
source("Derr.R")
source("InfoDes.R")
source("DBerrS.R")
source("DerrS.R")


#----
#' Example from seqDB help
# DB efficient choice set, given a design and parameter draws. 
# Candidate profiles 
cs <- idefix::Profiles(lvls = c(3, 3), coding = c("E", "E"))
m <- c(0.3, 0.2, -0.3, -0.2) # Prior mean (total = 5 parameters).
pc <- diag(length(m)) # Prior variance
set.seed(123)
ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = pc) # 10 draws.
ac <- c(0, 0) # No alternative specific constants. 
# Initial design.
des <- idefix::Modfed(cand.set = cs, n.sets = 6, n.alts = 2, alt.cte = ac, par.draws = ps)$design
# Efficient choice set to add. 
SeqDB(des = des, cand.set = cs, n.alts = 2, par.draws = ps, prior.covar = pc)

# Profiling
profvis(SeqDB(des = des, cand.set = cs, n.alts = 2, par.draws = ps, prior.covar = pc))


#----
#' Example from paper JJSIdefix
#' #' Chapter 3
#'   General optimal design for MIXL. Is the same as in the previous example
# Compute all possible profiles/treatments given the factors and number of 
# levels
set.seed(123)
cs <- idefix::Profiles(lvls = c(4, 3, 2), coding = c("E", "E", "E"))
# Sepcify prior for each respondent
m <- c(0.5, 0.5, 1, -0.3, -0.7, 0.7)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v)

# Generate DB optimal design: 8 choice sets with 2 alternatives each
init.des <- idefix::Modfed(cand.set = cs, n.sets = 8, n.alts = 2,
                   alt.cte = c(0, 0), par.draws = ps)$design
init.des

#'   Simulate choice data for the initial design
#'   True individual preference parameter
truePREF <- c(0.8, 1, 1.2, -0.4, -0.8, 1.3)

#'   Simulate choices on the logit model
#'   In this case, for the first five choice sets the second alternative is 
#'   chosen, whereas for the last three the first alternative is chosen.
set.seed(123)
y.sim <- idefix::RespondMNL(par = truePREF, des = init.des, n.alts = 2)
y.sim 

#'   Updating prior distribution
set.seed(123)
draws <- idefix::ImpsampMNL(prior.mean = m, prior.covar = v,
                    des = init.des, n.alts = 2, y = y.sim, m = 6)
draws

#'   Selecting optimal choice
#'   minimizing DB-error
dr <- draws$sample
w <- draws$weights
set <- SeqDB(des = init.des, cand.set = cs, n.alts = 2,
             par.draws = dr, prior.covar = v, weights = w)
set

#' Profiling seqDB function
profvis(SeqDB(des = init.des, cand.set = cs, n.alts = 2,
              par.draws = dr, prior.covar = v, weights = w))

#----
#   Step by step
# SeqDB <- function(des, cand.set, n.alts, par.draws, prior.covar, reduce = TRUE, weights = NULL) {

# Paramaters.
des <- init.des;des # Optimal design (matrix)
cand.set <- cs;cs # All possible treatments (matrix)
n.alts <- 2 # Number of alternatives for each set (numeric)
par.draws <- dr # Draws from the posterior (matrix)
prior.covar <-  v;prior.covar # Prior covariance matrix (Multinormal) (matrix)
weights <- w;weights # Weights from importance sampling algorithm (numeric)

#----
# Initialize.
n.sets <- nrow(des) / n.alts # Number of sets
cte.des <- NULL # 

# If no weights, equal weights.
if (is.null(weights)) {
  weights <- rep(1, nrow(par.draws)) # Each draw has weight = 1
}

# Detect alternative specific constants
des.f <- as.data.frame(des) # matrix to data.frame to use dplyr functions
alt.cte <- dplyr::select(des.f, dplyr::contains(".cte")) #  select variables 
                                                        #that have that string
if (ncol(alt.cte) > 0) {
  cte.des <- alt.cte[1:n.alts, ]  # Why until n.alts?? Ans: It seems that takes the constant defined in the Modfed function. All choice sets have the same constant
}

# Handling par.draws.
if (!(is.matrix(par.draws))) {
  par.draws <- matrix(par.draws, nrow = 1) #  Transform draws that are not matrices to vector. Why? Ans: It seems that is done to give the next error
}

# Error par.draws
if (ncol(des) != ncol(par.draws)) {
  stop("Numbers of parameters in par.draws does not match the number of parameters in the design.")
}

# Error identifying model.
if (n.sets < ncol(par.draws)) { # Dont know why this error yet. Check
  stop("Model is unidentified. Increase the number of choice sets or decrease parameters to estimate.")
}

#----
# Starting and initializing values.
i.cov <- solve(prior.covar) # Inverse of prior covariance matrix
d.start <- apply(par.draws, 1, Derr, des = des,  n.alts = n.alts)

#---- 
# Infodes
group <- rep(seq(1, nrow(des) / n.alts, 1), each = n.alts) #Vector to 

# probability of each alternative in each set
u <- des %*% diag(par.draws[1,])  # 
u <- .rowSums(u, m = nrow(des), n = length(par.draws[1,]))  
p <- exp(u) / rep(rowsum(exp(u), group), each = n.alts)
# information matrix
info.des <- crossprod(des * p, des) - crossprod(rowsum( des * p, group))
# crossprod(des * p, des)==t(des * p)%*%des
# crossprod(rowsum( des * p, group))==t(rowsum( des * p, group))%*%rowsum( des * p, group)
#---- 

#----
# Building InfoDes
sourceCpp("InfoDes_cpp.cpp")
# Rep_seq_cpp: Function to calculate the first line
rep(seq(1, 50 / 5, 1), each = 5)
InfoDes_cpp(par.draws[1,],matrix(0,50,4),5)
InfoDes_cpp(par.draws[1,],matrix(0,50,4),5)==rep(seq(1, 50 / 5, 1), each = 5) 

microbenchmark(InfoDes_cpp = InfoDes_cpp(par.draws[1,],matrix(0,50,4),5),
               InfoDes = rep(seq(1, 50 / 5, 1), each = 5) )

# Diagonal (1:)
sourceCpp("InfoDes_cpp.cpp")
diag(par.draws[1,])
InfoDes_cpp(par.draws[1,],des,2)
diag(par.draws[1,])==InfoDes_cpp(par.draws[1,],des,2)

# Matrix multiplication and row sum (2:)
# I have no idea why the code multiply and rowsum at the same time
sourceCpp("InfoDes_cpp.cpp")
.rowSums(u, m = nrow(des), n = length(par.draws[1,])) 
InfoDes_cpp(par.draws[1,],des,2)
# class(.rowSums(u, m = nrow(des), n = length(par.draws[1,]))  )
# class(InfoDes_cpp(par.draws[1,],des,2))
# rbind(.rowSums(u, m = nrow(des), n = length(par.draws[1,])) ,
# InfoDes_cpp(par.draws[1,],des,2))

# Exp function (3:)
sourceCpp("InfoDes_cpp.cpp")
u <- des %*% diag(par.draws[1,])  # 
u_exp = exp(.rowSums(u, m = nrow(des), n = length(par.draws[1,])) )
InfoDes_cpp(par.draws[1,],des,2)
# rbind(u_exp ,
# InfoDes_cpp(par.draws[1,],des,2))

# Denominator
# Sum for each alternative
sourceCpp("InfoDes_cpp.cpp")
rowsum(u_exp, group)
InfoDes_cpp(par.draws[1,],des,2)
# cbind(rowsum(u_exp, group),
# InfoDes_cpp(par.draws[1,],des,2))

# Repeat the sum for each alternative
sourceCpp("InfoDes_cpp.cpp")
rep(rowsum(u_exp, group), each = n.alts)
InfoDes_cpp(par.draws[1,],des,2)
# cbind(rep(rowsum(u_exp, group), each = n.alts),
      # InfoDes_cpp(par.draws[1,],des,2))

# Probability
sourceCpp("InfoDes_cpp.cpp")
u <- des %*% diag(par.draws[1,])  # 
u <- .rowSums(u, m = nrow(des), n = length(par.draws[1,]))  
p <- exp(u) / rep(rowsum(exp(u), group), each = n.alts);p
InfoDes_cpp(par.draws[1,],des,2)
cbind(p,InfoDes_cpp(par.draws[1,],des,2))

# information matrix
# Crossproduct 1: crossprod(des * p, des)==t(des * p)%*%des
sourceCpp("InfoDes_cpp.cpp")
# des * p
des * p
InfoDes_cpp(par.draws[1,],des,2)
# rbind(apply(des * p,1,sum),
#       apply(InfoDes_cpp(par.draws[1,],des,2),1,sum))

# crossprod(des * p, des) 
sourceCpp("InfoDes_cpp.cpp")
crossprod(des * p, des) 
InfoDes_cpp(par.draws[1,],des,2)
# rbind(apply(crossprod(des * p, des) ,1,sum),
#        apply(InfoDes_cpp(par.draws[1,],des,2),1,sum))
# des;dim(des)
# p;dim(p)
# des * p;dim(des * p);class(des * p)
# des[2,1]*p[2] == (des * p)[2,1]


# Crossproduct 2: crossprod(rowsum( des * p, group))==t(rowsum( des * p, group))%*%rowsum( des * p, group)
sourceCpp("InfoDes_cpp.cpp")
rowsum( des * p, group) 
InfoDes_cpp(par.draws[1,],des,2)
#rbind(apply(rowsum( des * p, group)  ,1,sum),
#      apply(InfoDes_cpp(par.draws[1,],des,2),1,sum))

#crossprod(rowsum( des * p, group))
sourceCpp("InfoDes_cpp.cpp")
crossprod(rowsum( des * p, group))
InfoDes_cpp(par.draws[1,],des,2)
#rbind(apply(crossprod(rowsum( des * p, group)),1,sum),
#      apply(InfoDes_cpp(par.draws[1,],des,2),1,sum))

#Infodes
sourceCpp("InfoDes_cpp.cpp")
crossprod(des * p, des) - crossprod(rowsum( des * p, group))
InfoDes_cpp(par.draws[1,],des,2)
rbind(apply(crossprod(des * p, des) - crossprod(rowsum( des * p, group)),1,sum),
      apply(InfoDes_cpp(par.draws[1,],des,2),1,sum))

microbenchmark(R = InfoDes(par.draws[1,],des,2),
               cpp = InfoDes_cpp(par.draws[1,],des,2))

#----
# Changing functions and comparing again (microbenchmark)
source("seqDB.R")
source("seqDB2.R")
source("Derr.R")
source("InfoDes.R")
source("DBerrS.R")
source("DerrS.R")
sourceCpp("InfoDes_cpp.cpp")

set.seed(123)
cs <- idefix::Profiles(lvls = c(4, 3, 2), coding = c("E", "E", "E"))
# Sepcify prior for each respondent
m <- c(0.5, 0.5, 1, -0.3, -0.7, 0.7)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v)

# Generate DB optimal design: 8 choice sets with 2 alternatives each
init.des <- idefix::Modfed(cand.set = cs, n.sets = 8, n.alts = 2,
                           alt.cte = c(0, 0), par.draws = ps)$design
#init.des

#'   Simulate choice data for the initial design
#'   True individual preference parameter
truePREF <- c(0.8, 1, 1.2, -0.4, -0.8, 1.3)

#'   Simulate choices on the logit model
#'   In this case, for the first five choice sets the second alternative is 
#'   chosen, whereas for the last three the first alternative is chosen.
set.seed(123)
y.sim <- idefix::RespondMNL(par = truePREF, des = init.des, n.alts = 2)
#y.sim 

#'   Updating prior distribution
set.seed(123)
draws <- idefix::ImpsampMNL(prior.mean = m, prior.covar = v,
                            des = init.des, n.alts = 2, y = y.sim, m = 6)
#draws

#'   Selecting optimal choice
#'   minimizing DB-error
dr <- draws$sample
w <- draws$weights
set <- SeqDB(des = init.des, cand.set = cs, n.alts = 2,
             par.draws = dr, prior.covar = v, weights = w)
set
set2 <- SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
             par.draws = dr, prior.covar = v, weights = w)
set2

a <- microbenchmark(R=SeqDB(des = init.des, cand.set = cs, n.alts = 2,
                     par.draws = dr, prior.covar = v, weights = w),
               Cpp=SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
                      par.draws = dr, prior.covar = v, weights = w));a
autoplot.microbenchmark(a)
#----

#----
# Profiling seqDB2 function
source("seqDB.R")
source("seqDB2.R")
source("Derr.R")
source("InfoDes.R")
source("DBerrS.R")
source("DerrS.R")
sourceCpp("InfoDes_cpp.cpp")

set.seed(123)
cs <- idefix::Profiles(lvls = c(4, 3, 2), coding = c("E", "E", "E"))
# Sepcify prior for each respondent
m <- c(0.5, 0.5, 1, -0.3, -0.7, 0.7)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v)

# Generate DB optimal design: 8 choice sets with 2 alternatives each
init.des <- idefix::Modfed(cand.set = cs, n.sets = 8, n.alts = 2,
                           alt.cte = c(0, 0), par.draws = ps)$design
#init.des

#'   Simulate choice data for the initial design
#'   True individual preference parameter
truePREF <- c(0.8, 1, 1.2, -0.4, -0.8, 1.3)

#'   Simulate choices on the logit model
#'   In this case, for the first five choice sets the second alternative is 
#'   chosen, whereas for the last three the first alternative is chosen.
set.seed(123)
y.sim <- idefix::RespondMNL(par = truePREF, des = init.des, n.alts = 2)
#y.sim 

#'   Updating prior distribution
set.seed(123)
draws <- idefix::ImpsampMNL(prior.mean = m, prior.covar = v,
                            des = init.des, n.alts = 2, y = y.sim, m = 6)
#draws

#'   Selecting optimal choice
#'   minimizing DB-error
dr <- draws$sample
w <- draws$weights
# set <- SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
#              par.draws = dr, prior.covar = v, weights = w)
# set

profvis(SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w))

# DerrS is the function to be improved
# Paramaters.
des <- init.des;des # Optimal design (matrix)
cand.set <- cs;cs # All possible treatments (matrix)
n.alts <- 2 # Number of alternatives for each set (numeric)
par.draws <- dr # Draws from the posterior (matrix)
prior.covar <-  v;prior.covar # Prior covariance matrix (Multinormal) (matrix)
weights <- w;weights # Weights from importance sampling algorithm (numeric)
reduce <- TRUE

n.sets <- nrow(des) / n.alts # Number of sets
cte.des <- NULL # 
# Detect alternative specific constants
des.f <- as.data.frame(des)
alt.cte <- dplyr::select(des.f, dplyr::contains(".cte"))
if (ncol(alt.cte) > 0) {
  cte.des <- alt.cte[1:n.alts, ]
}


# Starting and initializing values.
i.cov <- solve(prior.covar) # Inverse of prior covariance matrix
d.start <- apply(par.draws, 1, Derr2, des = des,  n.alts = n.alts) #  Calculate the D-error for each alternative
db.start <- mean(d.start, na.rm = TRUE)  # Calculates the mean D-error
full.comb <- gtools::combinations(n = nrow(cand.set), r = n.alts, repeats.allowed = !reduce) # Calculates all possible combinations without repetition
n.par <- ncol(par.draws) # Number of parameters

# For each potential set, select best. 
# db.errors <- apply(full.comb, 1, DBerrS2, cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights)

#   DBerrS2 Function
set <- as.matrix(cand.set[as.numeric(full.comb[1,]), ]) # matrix with only the alternatives chosen from the matrix of all combinations (full.comb).
# Add alternative specific constants if necessary
if (!is.null(cte.des)) {
  set <- as.matrix(cbind(cte.des, set))
}
# For each draw calculate D-error.
# d.errors <- apply(par.draws, 1, DerrS2, set, des, n.alts, i.cov, n.par)

#   DerrS2 Function
des.f <- rbind(des, set)  # Append of optimal design with new alternatives
info.d <- InfoDes(par = par.draws[1,], des = des.f, n.alts = n.alts)  # Information matrix for each row
d.error <- det(info.d + i.cov)^(-1 / n.par) # Calculate sequential d-error

# des.f append with new alternatives
sourceCpp("DerrS_cpp.cpp")
rbind(des, set) ==
DerrS_cpp(par.draws[1,], set, des, n.alts, i.cov, n.par)
# microbenchmark(R = rbind(des, set),
#               cpp= DerrS_cpp(par.draws[1,], set, des, n.alts, i.cov, n.par))

# Call function InfoDes
sourceCpp("DerrS_cpp.cpp")
InfoDes(par = par.draws[1,], des = des.f, n.alts = n.alts)  # Information 
InfoDes_cpp(par = par.draws[1,], des = des.f, n_alts = n.alts)  # Information
DerrS_cpp(par.draws[1,], set, des, n.alts, i.cov, n.par)
# microbenchmark(R= InfoDes(par = par.draws[1,], des = des.f, n.alts = n.alts),
#                Info_des_cpp = InfoDes_cpp(par = par.draws[1,], des = des.f, n_alts = n.alts),
#                Derrs_cpp =DerrS_cpp(par.draws[1,], set, des, n.alts, i.cov, n.par))

# Calculate determinant
sourceCpp("DerrS_cpp.cpp")
det(info.d + i.cov)^(-1 / n.par) # Calculate sequential d-error

# The determinant in cpp is faster
# det_cpp(info.d + i.cov)^(-1 / n.par) # Calculate sequential d-error
# a = microbenchmark(R=det(info.d + i.cov)^(-1 / n.par),
#                 cpp=det_cpp(info.d + i.cov)^(-1 / n.par));a
# autoplot.microbenchmark(a)

# comparison between functions
DerrS(par.draws[1,], set, des, n.alts, i.cov, n.par)
DerrS2(par.draws[1,], set, des, n.alts, i.cov, n.par)
DerrS_cpp(par.draws[1,], set, des, n.alts, i.cov, n.par)

a <- microbenchmark(r = DerrS(par.draws[1,], set, des, n.alts, i.cov, n.par),
               Info_des = DerrS2(par.draws[1,], set, des, n.alts, i.cov, n.par),
               cpp = DerrS_cpp(par.draws[1,], set, des, n.alts, i.cov, n.par));a
autoplot.microbenchmark(a)
boxplot(a)

DBerrS(full.comb,cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights)
DBerrS2(full.comb,cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights)
DBerrS3(full.comb,cand.set, par.draws, des, n.alts, cte.des, i.cov, n.par, weights)
b <- microbenchmark(r = DBerrS(full.comb,cand.set, par.draws, des, n.alts, cte.des,
                          i.cov, n.par, weights),
               Infodes = DBerrS2(full.comb,cand.set, par.draws, des, n.alts, 
                                 cte.des, i.cov, n.par, weights),
               cpp = DBerrS3(full.comb,cand.set, par.draws, des, n.alts, 
                             cte.des, i.cov, n.par, weights));b
autoplot.microbenchmark(b)
boxplot(b)
#----
# Remove all elements
rm(list = ls())
# Changing functions and comparing again (microbenchmark)
source("seqDB.R")
source("seqDB2.R")
source("seqDB3.R")
source("seqDB4.R")
source("seqDB5.R")
source("Derr.R")
source("InfoDes.R")
source("DBerrS.R")
source("DerrS.R")
sourceCpp("InfoDes_cpp.cpp")
sourceCpp("DerrS_cpp.cpp")

set.seed(123)
cs <- idefix::Profiles(lvls = c(4, 3, 2), coding = c("E", "E", "E"))
# Sepcify prior for each respondent
m <- c(0.5, 0.5, 1, -0.3, -0.7, 0.7)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v)

# Generate DB optimal design: 8 choice sets with 2 alternatives each
init.des <- idefix::Modfed(cand.set = cs, n.sets = 8, n.alts = 2,
                           alt.cte = c(0, 0), par.draws = ps)$design
#init.des

#'   Simulate choice data for the initial design
#'   True individual preference parameter
truePREF <- c(0.8, 1, 1.2, -0.4, -0.8, 1.3)

#'   Simulate choices on the logit model
#'   In this case, for the first five choice sets the second alternative is 
#'   chosen, whereas for the last three the first alternative is chosen.
set.seed(123)
y.sim <- idefix::RespondMNL(par = truePREF, des = init.des, n.alts = 2)
#y.sim 

#'   Updating prior distribution
set.seed(123)
draws <- idefix::ImpsampMNL(prior.mean = m, prior.covar = v,
                            des = init.des, n.alts = 2, y = y.sim, m = 6)
#draws

#'   Selecting optimal choice
#'   minimizing DB-error
dr <- draws$sample
w <- draws$weights
set <- SeqDB(des = init.des, cand.set = cs, n.alts = 2,
             par.draws = dr, prior.covar = v, weights = w)
set2 <- SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set3 <- SeqDB3(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set4 <- SeqDB4(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set5 <- SeqDB5(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set;set2;set3;set4;set5

a = microbenchmark(R = SeqDB(des = init.des, cand.set = cs, n.alts = 2,
                       par.draws = dr, prior.covar = v, weights = w),
               Infodes_Cpp = SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
                          par.draws = dr, prior.covar = v, weights = w),
               DerrS_cpp = SeqDB3(des = init.des, cand.set = cs, n.alts = 2,
                      par.draws = dr, prior.covar = v, weights = w),
               det_cpp = SeqDB4(des = init.des, cand.set = cs, n.alts = 2,
                      par.draws = dr, prior.covar = v, weights = w),
               det_cpp_2 = SeqDB5(des = init.des, cand.set = cs, n.alts = 2,
                                par.draws = dr, prior.covar = v, weights = w));a
autoplot.microbenchmark(a)
# it's still better seqDB2: the rbind is faster in R that in cpp
# using rbind and det_cpp is faster
# To see source code of rbind: library(pryr);show_c_source(.Primitive(rbind(x)));

profvis(SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w))
# library(microbenchmark)
# microbenchmark(apply(par.draws, 1, Derr, des = des,  n.alts = n.alts),
# apply(par.draws, 1, Derr_dan1, des = des,  n.alts = n.alts))


#----------------------------------------------------------------------------
#' Profiling the whole example code
#' profvis({set.seed(123)
#'   cs <- idefix::Profiles(lvls = c(4, 3, 2), coding = c("E", "E", "E"))
#'   m <- c(0.5, 0.5, 1, -0.3, -0.7, 0.7)
#'   v <- diag(length(m))
#'   ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v)
#'   init.des <- idefix::Modfed(cand.set = cs, n.sets = 8, n.alts = 2,
#'                              alt.cte = c(0, 0), par.draws = ps)$design
#'   #init.des
#'   
#'   #'   Simulate choice data
#'   #'   True individual preference parameter
#'   truePREF <- c(0.8, 1, 1.2, -0.4, -0.8, 1.3)
#'   
#'   #'   Simulate choices on the logit model
#'   set.seed(123)
#'   y.sim <- idefix::RespondMNL(par = truePREF, des = init.des, n.alts = 2)
#'   #y.sim
#'   
#'   #'   Updating prior distribution
#'   set.seed(123)
#'   draws <- idefix::ImpsampMNL(prior.mean = m, prior.covar = v,
#'                               des = init.des, n.alts = 2, y = y.sim, m = 6)
#'   #draws
#'   
#'   #'   Selecting optimal choice
#'   #'   minimizing DB-error
#'   dr <- draws$sample
#'   w <- draws$weights
#'   set <- SeqDB(des = init.des, cand.set = cs, n.alts = 2,
#'                par.draws = dr, prior.covar = v, weights = w)
#'   #set
#'   })


