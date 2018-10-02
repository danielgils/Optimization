#' ---
#' title: "Optimizing seqDB function"
#' author: "Daniel Gil"
#' date: "Sep 2018"
#' ---
#' 
#' ###**Summary**
#' Multiple options were used to optimize *seqDB* function. After all, the faster function reduces the time in orders of magnitude ().
#' 
#' ###**Description process**
#' According to Wickham in his book "Advanced R", optimizing code to make it faster is an iterative procedure: <br>
#' 1. Find the biggest bottleneck (the slowest part of your code).<br>
#' 2. Try to eliminate it (you may not succeed but that’s ok).<br>
#' 3. Repeat until your code is “fast enough.”<br>
#' This report describes the process conducted to optimize the *seqDB* function. <br>
#' <br>
#' 
#' #### Considerations
#' - To be able to create multiple versions of a function without losing the location of each file, I re-organized the files that comes from the package. Now each function has its own file.
#' - I choose the example used in Chapter 3 of the paper "Generating Optimal Designs for Discrete Choice Experiments in R: The idefix Package" to measure the improvements in the optimization process.
#' - The following packages are used here: <br>
#'   **profvis**: to find bottlenecks in *seqDB* function<br>
#'   **microbenchmark**: to compare processing time of each implementation<br>
#'   **Rcpp**: to implement code in C++.<br>
#'   **RcppArmadillo**: to use built-in armadillo functions in C++.<br>
#' 
#' #### 1. Find the biggest bottleneck <br>
#' The first step is to load all functions from the package that are used in *SeqDB* function. Because I reorganized everything, I load each function:
source("seqDB.R")
source("Derr.R")
source("InfoDes.R")
source("DBerrS.R")
source("DerrS.R")

#' Then load packages
library(microbenchmark)
library(profvis)
library(Rcpp)
library(RcppArmadillo)

#' Run the code as in the paper.
set.seed(123)
cs <- idefix::Profiles(lvls = c(4, 3, 2), coding = c("E", "E", "E"))

# Specify prior for each respondent
m <- c(0.5, 0.5, 1, -0.3, -0.7, 0.7)
v <- diag(length(m))
ps <- MASS::mvrnorm(n = 10, mu = m, Sigma = v)

# Generate DB optimal design: 8 choice sets with 2 alternatives each
init.des <- idefix::Modfed(cand.set = cs, n.sets = 8, n.alts = 2,
                           alt.cte = c(0, 0), par.draws = ps)$design
#init.des

#   Simulate choice data for the initial design
#   True individual preference parameter
truePREF <- c(0.8, 1, 1.2, -0.4, -0.8, 1.3)

#   Simulate choices on the logit model
#   In this case, for the first five choice sets the second alternative is 
#   chosen, whereas for the last three the first alternative is chosen.
set.seed(123)
y.sim <- idefix::RespondMNL(par = truePREF, des = init.des, n.alts = 2)
#y.sim 

#   Updating prior distribution
set.seed(123)
draws <- idefix::ImpsampMNL(prior.mean = m, prior.covar = v,
                            des = init.des, n.alts = 2, y = y.sim, m = 6)
#draws

#   Selecting optimal choice
#   minimizing DB-error
dr <- draws$sample
w <- draws$weights
set <- SeqDB(des = init.des, cand.set = cs, n.alts = 2,
             par.draws = dr, prior.covar = v, weights = w)
set

#' Profiling seqDB function to find bottlenecks
profvis(SeqDB(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w))

#' Multiple things can be noticed: <br>
#' - The profvis result shows that in the *seqDB* function the bottleneck is located in an apply function. This apply calls the *DBerrS* function.<br>
#' - In *DBerrS* function, the bottleneck is located in an apply function that calls the *DerrS* function.
#' - In *DerrS* function, the bottleneck is located in the line that calls the function *InfoDes*.<br>
#' - In *InfoDes* function, there are multiple lines that takes time to process: a rep(seq()) line. The line where the probability is computed. And the line where the information matrix is computed.<br>
#' For these reasons, the biggest bottlneck is in the *InfoDes* function.
#' 
#' #### 2. Implementation in C++ with Rcpp package.
#' Before implementing the code in C++, the parameters of the function *seqDB* as well as the variables created before the first apply within the function are created in the global enviroment (like an attach).

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

#' Then the implementation is coded and sourced in R to make the comparison. To do this, each line of the *InfoDes* function was coded in C++ and tested. <br>
#' Having done this, a copy of the *seqDB* function is done, called *seqDB2*, with the following changes: <br>
#' - Calls the function *Derr2* instead of *Derr*, which calls the function *Infodes_cpp* instead of *InfoDes*. <br>
#' - Calls the function *DBerr2* instead of *DBerr*, which calls the function *DerrS2* which calls *Infodes_cpp* instead of *InfoDes*. <br>
#' The comparison of the results and time to execute is presented:
#' 
```{Rcpp}

```

sourceCpp("InfoDes_cpp.cpp")
source("seqDB2.R")
set <- SeqDB(des = init.des, cand.set = cs, n.alts = 2,
             par.draws = dr, prior.covar = v, weights = w)
set2 <- SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set;set2
# 
# a <- microbenchmark(seqDB = SeqDB(des = init.des, cand.set = cs, n.alts = 2,
#                             par.draws = dr, prior.covar = v, weights = w),
#                     seqDB2 = SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
#                                par.draws = dr, prior.covar = v, weights = w));a
# autoplot.microbenchmark(a)
#   
