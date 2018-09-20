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
library(shiny)

# Load functions
source("seqDB.R")
source("seqDB2.R")
source("seqDB3.R")
source("Derr.R")
source("InfoDes.R")
source("DBerrS.R")
source("DerrS.R")
sourceCpp("InfoDes_cpp.cpp")
sourceCpp("DerrS_cpp.cpp")

#----
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
set3 <- SeqDB3(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)
set;set2;set3

profvis({SeqDB(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)})
profvis({SeqDB2(des = init.des, cand.set = cs, n.alts = 2,
               par.draws = dr, prior.covar = v, weights = w)})
profvis({SeqDB3(des = init.des, cand.set = cs, n.alts = 2,
                par.draws = dr, prior.covar = v, weights = w)})


#----
#   Example from paper
#'   Discrete choice experiment without any adaptive sets.
# data("example_design")
# xdes <- example_design
# xdes
# getwd()
n.sets <- 8
alternatives <- c("Alternative A", "Alternative B")
attributes <- c("Price", "Time", "Comfort")
labels <- vector(mode = "list", length(attributes))
labels[[1]] <- c("$10", "$5", "$1")
labels[[2]] <- c("20 min", "12 min", "3 min")
labels[[3]] <- c("bad", "average", "good")

code <- c("D", "D", "D")

b.text <- "Please choose the alternative you prefer"
i.text <- "Welcome, here are some instructions ... good luck!"
e.text <- "Thanks for taking the survey"

# SurveyApp (des = xdes, n.total = n.sets, alts = alternatives,
#            atts = attributes, lvl.names = labels, coding = code,
#            buttons.text = b.text, intro.text = i.text, end.text = e.text,
#            data.dir = NULL)


#   Discrete choice experiment containing adaptive sets.
n.sets <- 12
p.mean <- c(0.3, 0.7, 0.3, 0.7, 0.3, 0.7)
p.var <- diag(length(p.mean))

levels <- c(3, 3, 3)
code <- c("D", "D", "D")
cand <- idefix::Profiles(lvls = levels, coding = code)

dataDir = "C:/Users/danie/Documents/Daniel Gil/KULeuven/Stage 2/Thesis/Scripts/Output_test"

SurveyApp (des = xdes, n.total = n.sets, alts = alternatives,
           atts = attributes, lvl.names = labels, coding = code,
           buttons.text = b.text, intro.text = i.text,
           end.text = e.text, data.dir = dataDir, crit= "KL",
           prior.mean = p.mean, prior.covar = p.var,
           cand.set = cand, m = 6)

#   Without initial design
SurveyApp (des = NULL, n.total = n.sets, alts = alternatives,
           atts = attributes, lvl.names = labels, coding = code,
           buttons.text = b.text, intro.text = i.text,
           end.text = e.text, data.dir = dataDir, crit = "KL",
           prior.mean = p.mean, prior.covar = p.var,
           cand.set = cand, m = 6)

runApp(SurveyApp (des = xdes, n.total = n.sets,
                  alts = alternatives, atts = attributes, lvl.names = labels,
                  coding = code, buttons.text = b.text, intro.text = i.text,
                  end.text = e.text, data.dir = tempdir()))
