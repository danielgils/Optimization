#' ---
#' title: "Profilling seqDBApp function"
#' author: "Daniel Gil"
#' date: "`Sep 2018"
#' output: github_document
#' ---
#' 

# Remove all elements
rm(list = ls())

# Load packages
library(shiny)
# library(microbenchmark)
# library(profvis)
# library(Rcpp)
# library(RcppArmadillo)

#----
source("SurveyApp.R")
source("Decode.R")
source("Profiles.R")
source("Charbin.R")
source("ImpsampMNL.R")
source("Lattice_mvt.R")


#----
#'   Real surveys with idefix
#'   Discrete choice experiment without any adaptive sets.
# data("example_design")
load("C:/Users/danie/Documents/Daniel Gil/KULeuven/Stage 2/Thesis/Scripts/idefix/data/example_design.RData")
xdes <- example_design
xdes

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


SurveyApp(des = xdes, n.total = n.sets, alts = alternatives,
           atts = attributes, lvl.names = labels, coding = code,
           buttons.text = b.text, intro.text = i.text, end.text = e.text,
           data.dir = NULL)

#----
#   Discrete choice experiment containing adaptive sets.
n.sets <- 12
p.mean <- c(0.3, 0.7, 0.3, 0.7, 0.3, 0.7)
p.var <- diag(length(p.mean))

levels <- c(3, 3, 3)
code <- c("D", "D", "D")
cand <- Profiles(lvls = levels, coding = code)

dataDir = "C:/Users/danie/Documents/Daniel Gil/KULeuven/Stage 2/Thesis/Scripts/Output_test"

SurveyApp(des = xdes, n.total = n.sets, alts = alternatives,
           atts = attributes, lvl.names = labels, coding = code,
           buttons.text = b.text, intro.text = i.text,
           end.text = e.text, data.dir = dataDir, crit= "KL",
           prior.mean = p.mean, prior.covar = p.var,
           cand.set = cand, m = 6)
debug(ImpsampMNL)

#----
#   Without initial design
# Hay dos versiones de ImpsampMNL, tener cuidado
# Me pide log_post: Creo que es mejor trabajar con el nuevo programa y pedir ejemplos a Frits

SurveyApp (des = NULL, n.total = n.sets, alts = alternatives,
           atts = attributes, lvl.names = labels, coding = code,
           buttons.text = b.text, intro.text = i.text,
           end.text = e.text, data.dir = dataDir, crit = "DB",
           prior.mean = p.mean, prior.covar = p.var,
           cand.set = cand, m = 6)

runApp(SurveyApp (des = xdes, n.total = n.sets,
                  alts = alternatives, atts = attributes, lvl.names = labels,
                  coding = code, buttons.text = b.text, intro.text = i.text,
                  end.text = e.text, data.dir = tempdir()))

# dataDir <- getwd()
data <- LoadData(data.dir = dataDir, type = "num")
data
help(package="idefix")
