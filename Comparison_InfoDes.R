#' ---
#' title: "Pruebas de comparacion entre funciones"
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

# Load functions
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
set2 <- SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set;set2

# profvis({SeqDB(des = init.des, cand.set = cs, n.alts = 2,
#                par.draws = dr, prior.covar = v, weights = w)})
# profvis({SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
#                par.draws = dr, prior.covar = v, weights = w)})
