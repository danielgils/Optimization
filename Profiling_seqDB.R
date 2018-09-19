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
library(profvis)

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
db.start <- mean(d.start, na.rm = TRUE)
full.comb <- gtools::combinations(n = nrow(cand.set), r = n.alts, repeats.allowed = !reduce)
n.par <- ncol(par.draws)






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


