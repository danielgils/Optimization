#'Shiny application to generate a discrete choice survey.
#'
#'This function starts a shiny application which puts choice sets on screen and 
#'saves the responses. The complete choice design can be provided in advance, or
#'can be generated sequentially adaptively, or can be a combination of both.
#'
#'A pregenerated design can be specified in \code{des}. This should be a matrix 
#'in which each row is a profile. This can be generated with \code{Modfed}, but 
#'is not necesarry.
#'
#'If \code{n.total} = \code{nrow(des)} / \code{length(alts)}, the specified
#'design will be put on screen, one set after the other, and the responses will
#'be saved. If \code{n.total} > (\code{nrow(des)} / \code{length(alts)}), first
#'the specified design will be shown and afterwards the remaining sets will be 
#'generated adaptively. If \code{des} = \code{NULL}, \code{n.total} sets will be
#'generated adaptively.
#'
#'Whenever adaptive sets will be generated, \code{crit}, \code{prior.mean}, 
#'\code{prior.covar}, \code{cand.set} and \code{m}, should be specified.
#'
#'The names specified in \code{alts} will be used to label the choice 
#'alternatives. The names specified in \code{atts} will be used to name the 
#'attributes in the choice sets. The values of \code{lvl.names} will be used to 
#'create the values in the choice sets. See \code{\link{Decode}} for more 
#'details. The number of draws sampeled from the posterior preference 
#'distribution in the importance sampling algorithm used for adaptive sets can 
#'be specified with \code{m}, where the number is 2^\code{m}.
#'
#'The text specified in \code{buttons.text} will be displayed above the buttons 
#'to indicate the preferred choice (for example: "indicate your preferred 
#'choice"). The text specified in \code{intro.text} will be displayed before the
#'choice sets. This will generally be a description of the survey and some 
#'instructions. The text specified in \code{end.text} will be displayed after 
#'the survey. This will generally be a thanking note and some further 
#'instructions.
#'
#'
#'@param alts A character vector containing the names of the alternatives.
#'@param atts A character vector containing the names of the attributes.
#'@param n.total A numeric value indicating the total number of choice sets.
#'@param buttons.text A string containing the text presented together with the 
#'  option buttons.
#'@param intro.text A string containing the text presented before the choice 
#'  survey.
#'@param end.text A string containing the text presented after the choice 
#'  survey.
#'@param data.dir A character string with the directory denoting where the data
#'  needs to be written. The default is NULL
#'@param crit A string containing eihter KL or DB indicating the adaptive
#'  criterion to be used.
#'@inheritParams Decode
#'@inheritParams Modfed
#'@inheritParams Profiles
#'@inheritParams SeqKL
#'@inheritParams ImpsampMNL
#'@importFrom Rdpack reprompt
#'@references \insertRef{crabbe}{mnldes}
#'@return After completing the survey, two text files can be found in 
#'  \code{data.dir}. The file with "num" in the filename is a matrix with the 
#'  numeric choice data. The coded design matrix ("par"), presented during the 
#'  survey, together with the observed responses ("resp") can be found here. 
#'  Rownames indicate the setnumbers. The file with "char" in the filename is a 
#'  matrix with character choice data. The labeled design matrix ("par"), 
#'  presented during the survey, together with the observed responses ("resp") 
#'  can be found here. See \code{\link{LoadData}} to load the data.
#' @examples 
#' \donttest{
#'#### Present choice design without adaptive sets (n.total = sets in des)
#'# NOTE that the data will be saved in the current working directory. 
#'# example design 
#'data("example_design") # pregenerated design
#'xdes <- example_design
#'### settings of the design 
#'code <- c("D", "D", "D")
#'n.sets <- 8
#'# settings of the survey
#'alternatives <- c("Alternative A", "Alternative B")
#'attributes <- c("Price", "Time", "Comfort")
#'labels <- vector(mode="list", length(attributes))
#'labels[[1]] <- c("$10", "$5", "$1")
#'labels[[2]] <- c("20 min", "12 min", "3 min")
#'labels[[3]] <- c("bad", "average", "good")
#'i.text <- "Welcome, here are some instructions ... good luck!"
#'b.text <- "Please choose the alternative you prefer"
#'e.text <- "Thanks for taking the survey"
#'dataDir <- getwd()
#'# Display the survey 
#'SurveyApp (des = xdes, n.total = n.sets, alts = alternatives, 
#'           atts = attributes, lvl.names = labels, coding = code, 
#'           buttons.text = b.text, intro.text = i.text, end.text = e.text,
#'           data.dir = dataDir)
#'# Data 
#'data_num <- LoadData(data.dir = dataDir, type  = "num")
#'data_char <- LoadData(data.dir = dataDir, type = "char")
#'
#'#### Present choice design with adaptive sets (n.total > sets in des)
#'# NOTE that the data will be saved in the current working directory. 
#'# example design 
#'data("example_design") # pregenerated design
#'xdes <- example_design
#'### settings of the design 
#'code <- c("D", "D", "D")
#'n.sets <- 12
#'# settings of the survey
#'alternatives <- c("Alternative A", "Alternative B")
#'attributes <- c("Price", "Time", "Comfort")
#'labels <- vector(mode="list", length(attributes))
#'labels[[1]] <- c("$10", "$5", "$1")
#'labels[[2]] <- c("20 min", "12 min", "3 min")
#'labels[[3]] <- c("bad", "average", "good")
#'i.text <- "Welcome, here are some instructions ... good luck!"
#'b.text <- "Please choose the alternative you prefer"
#'e.text <- "Thanks for taking the survey"
#'# setting for adaptive sets 
#'levels <- c(3, 3, 3)
#'cand <- Profiles(lvls = levels, coding = code)
#'p.mean <- c(0.3, 0.7, 0.3, 0.7, 0.3, 0.7)
#'p.var <- diag(length(p.mean))
#'dataDir <- getwd()
#'# Display the survey 
#'SurveyApp (des = NULL, n.total = n.sets, alts = alternatives, atts =
#'attributes, lvl.names = labels, coding = code, buttons.text = b.text,
#'intro.text = i.text, end.text = e.text, data.dir = dataDir, crit= "KL",
#'prior.mean = p.mean, prior.covar = p.var, cand.set = cand, m = 6)
#'# Data 
#'data_num <- LoadData(data.dir = dataDir, type = "num")
#'data_char <- LoadData(data.dir = dataDir, type = "char")
#'
#'#### Choice design with only adaptive sets (des=NULL)
#'# NOTE that the data will be saved in the current working directory. 
#'# setting for adaptive sets 
#'levels <- c(3, 3, 3)
#'p.mean <- c(0.3, 0.7, 0.3, 0.7, 0.3, 0.7)
#'p.var <- diag(length(p.mean)) 
#'code <- c("D", "D", "D")
#'cand <- Profiles(lvls = levels, coding = code)
#'n.sets <- 12
#'# settings of the survey
#'alternatives <- c("Alternative A", "Alternative B")
#'attributes <- c("Price", "Time", "Comfort")
#'labels <- vector(mode="list", length(attributes))
#'labels[[1]] <- c("$10", "$5", "$1")
#'labels[[2]] <- c("20 min", "12 min", "3 min")
#'labels[[3]] <- c("bad", "average", "good")
#'i.text <- "Welcome, here are some instructions ... good luck!"
#'b.text <- "Please choose the alternative you prefer"
#'e.text <- "Thanks for taking the survey"
#'dataDir <- getwd()
#'# Display the survey 
#'SurveyApp (des = NULL, n.total = n.sets, alts = alternatives, 
#'           atts = attributes, lvl.names = labels, coding = code, 
#'           buttons.text = b.text, intro.text = i.text, end.text = e.text, data.dir = dataDir, 
#'           crit= "KL", prior.mean = p.mean, prior.covar = p.var, cand.set = cand, m = 6)
#'# Data 
#'data_num <- LoadData(data.dir = dataDir, type = "num")
#'data_char <- LoadData(data.dir = dataDir, type = "char")
#'}
#'@import shiny
#'@export
SurveyApp <- function(des = NULL, n.total, alts, atts, lvl.names, coding, 
                      buttons.text, intro.text, end.text, data.dir = NULL,
                      c.lvls = NULL, crit = NULL, alt.cte = NULL, prior.mean = NULL,
                      prior.covar = NULL, cand.set = NULL, m = NULL) {
  # Initialize 
  sdata <- vector(mode = "list")
  surveyData <- vector(mode = "list")
  y.bin <- vector("numeric")
  resp  <- vector("character")
  n.atts <- length(atts)
  n.alts <- length(alts)
  choice.sets <- matrix(data = NA, nrow = n.total * n.alts, ncol = n.atts)
  buttons <- NULL
  sn <- 0
  
  if (is.null(alt.cte)) {
    alt.cte <- rep(0, n.alts)
    cte.des <- NULL
  } else {
    # Error 
    if (!all(alt.cte %in% c(0,1))){
      stop("alt.cte should only contain 0s or 1s.")
    }
  }
  if (is.null(des)) {
    n.init <- 0
    fulldes <- matrix(data = NA, nrow = (n.alts * n.total), ncol = ncol(cand.set))
  } else {
    n.init <- nrow(des) / n.alts 
    bs <- seq(1, (nrow(des) - n.alts + 1), n.alts)
    es <- c((bs - 1), nrow(des))[-1] 
    if (sum(alt.cte) > 0) {
      cte.des <- Altspec(alt.cte = alt.cte, n.sets = (nrow(des) / n.alts))
      colnames(cte.des) <- paste(paste("alt", which(alt.cte == 1), sep = ""), ".cte", sep = "")
    }
    colnames(des) <- paste("par", 1 : ncol(des), sep = ".")
    fulldes <- cbind(cte.des, des)
    # Error handling
    if (length(bs) != n.init) {
      stop("The number of design rows does not match the number of alternatives times the number of sets.")
    }
  }
  # Error handling
  if (!is.null(data.dir)) {
    if (!dir.exists(data.dir)) {
      stop("Directory data.dir does not exist")
    }
  }
  if (n.total > n.init) {
    if (any(c(is.null(prior.mean), is.null(prior.covar), is.null(cand.set), is.null(m), is.null(crit)))) {
      stop("When n.total is larger than the number of sets in argument des, arguments crit, prior.mean, prior.covar, cand.set and m should be specified.")
    }
    if (length(prior.mean) != ncol(cand.set) + sum(alt.cte)) {
      stop("Number of parameters in prior.mean does not match with cand.set + alt.cte")
    }
  } else {
    if (!is.null(prior.mean)) {
      warning("prior.mean will be ignored, since there are no adaptive sets.")
    } 
    if (!is.null(prior.covar)) {
      warning("prior.covar will be ignored, since there are no adaptive sets.")
    }
    if (!is.null(cand.set)) {
      warning("cand.set will be ignored, since there are no adaptive sets.")
    }
    if (sum(alt.cte) > 0) {
      warning("alt.cte will be ignored, since there are no adaptive sets.")
    }
    if (!is.null(m)) {
      warning("m will be ignored, since there are no adaptive sets.")
    }
  }
  if (crit =="DB" && is.null(des)) {
    stop("In order to use the DB criterion, an initial design has to be provided.")
  }
  
  shinyApp(
    ### User interface
    ui <- fluidPage(
      # Put setnr on screen
      column(8, align = 'center', textOutput("set.nr")),
      # Put design on screen
      column(8, align = 'center', tableOutput("choice.set")),
      # Put answer options on screen
      column(8, align = 'center', uiOutput('buttons')), 
      # put introtext on screen
      column(8, align = 'center', textOutput('intro')),
      # Put action button on screen
      column(8, align = "center", actionButton("OK", "OK")),
      # put end text on screen
      column(8, align = 'center', textOutput('end'))
    ),
    ### Server
    server <- function(input, output) {
      # Count set number
      observeEvent(input$OK, {
        sn <<- sn + 1
      })
      # Set selection function
      Select <- function () {
        if (sn <= n.total) {
          # for initial sets 
          if (sn <= n.init) {
            set <- des[bs[sn] : es[sn], ]
          } else {
            ## sample drawing for adaptive sets
            # if First set
            if (sn == 1) {
              # sample draws from prior
              s <- MASS::mvrnorm(n = 2 ^ m, mu = prior.mean, Sigma  = prior.covar)
              w <- rep(1, nrow(s)) / nrow(s)
              # From second set
            } else {
              # Sample draws from updated posterior
              sam <- ImpsampMNL(prior.mean = prior.mean, prior.covar = prior.covar, des = fulldes, n.alts = n.alts, y = y.bin, m = m)
              s <- sam$sample
              w <- sam$weights
            }
            
            ## Selecting set
            if (crit == "KL") {
              # Select new set based on KL info
              set <- SeqKL(cand.set = cand.set, n.alts = n.alts, par.draws = s, alt.cte = alt.cte, weights = w)$set
              #delete alt.cte if necessary
              if (sum(alt.cte) > 0) {
                set <- set[ , -(1 : (sum(alt.cte)))]
              }
            } else if (crit == "DB") {
              # Select new set based on DB 
              setobj <- SeqDBApp(des = des, cand.set = cand.set, n.alts = n.alts, par.draws = s, prior.covar = prior.covar, alt.cte = alt.cte, w = w)
              set <- setobj$set
              db  <- setobj$db
            } else {
              stop("Argument crit should eihter be KL or DB.")
            }
            
            ## Design storage
            if (sn == 1) { 
              rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn , sep = ""), "alt", sep = "."))
              colnames(set) <- paste("par", 1:ncol(set), sep = ".")
              des <<- set
              # with alt.cte
              altset <- Altspec(alt.cte, n.sets = 1)
              if (sum(alt.cte) > 0) {
                colnames(altset) <- paste(paste("alt", which(alt.cte == 1), sep = ""), ".cte", sep = "")
              }
              fullset <- cbind(altset, set)
              fulldes <<- fullset
            } else {
              rownames(set) <- rownames(set, do.NULL = FALSE, prefix = paste(paste("set", sn , sep = ""), "alt", sep = "."))
              colnames(set) <- paste("par", 1:ncol(set), sep = ".")
              des <<- rbind(des, set)
              # with alt.cte
              altset <- Altspec(alt.cte, n.sets = 1)
              if (sum(alt.cte) > 0) {
                colnames(altset) <- paste(paste("alt", which(alt.cte == 1), sep = ""), ".cte", sep = "")
              }
              fullset <- cbind(altset, set)
              fulldes <<- rbind(fulldes, fullset)
            }
          }
          # Transform coded set to attribute level character set.
          choice.set <- Decode(set = set, lvl.names = lvl.names, coding = coding, c.lvls = c.lvls)
          choice.set <- t(choice.set[ , 1:n.atts])
          # Fill in attribute names and alternatives names
          colnames(choice.set) <- alts
          rownames(choice.set) <- atts
          # Store uncoded choice set
          if (sn == 1) {
            choice.sets <<- choice.set
          } else {
            choice.sets <<- rbind(choice.sets, choice.set)
          }
          #return design 
          return(choice.set)
        }
      }
      #When action button is clicked
      observeEvent(input$OK, {
        # survey phase 
        if (sn <= n.total ) {
          # Plot new choice set
          output$choice.set <-  renderTable(Select(), rownames = TRUE)
        }
        # Store responses and design
        if (sn > 1 && sn <= (n.total +1)) {
          resp  <<- c(resp, input$survey)
          y.bin <<- Charbin(resp = resp, alts = alts, n.alts = n.alts)
          sdata[["bin.responses"]] <- y.bin
          sdata[["responses"]] <- resp
          sdata[["desing"]] <- fulldes
          sdata[["survey"]] <- choice.sets
          surveyData <<- sdata 
        } 
        # end phase 
        if (sn > n.total) {
          #Don't show choice set
          output$choice.set <-  renderTable(NULL)
        }
      })
      #Output response options after first action button click
      output$buttons <- renderUI({
        # radiobuttons
        if (input$OK > 0 && input$OK <= n.total) {
          return(list(radioButtons("survey", buttons.text,
                                   alts , inline = T, selected = "None")))
        }
      })
      # set nr
      observeEvent(input$OK, {
        if (sn < n.total) {
          output$set.nr <- renderText(paste(c("choice set:", sn, "/", n.total)))
        } else {output$set.nr <- renderText(NULL)}
      })
      # Introtext
      output$intro <- renderText(intro.text)
      observeEvent(input$OK, {
        output$intro <- renderText(NULL)
      })
      # End of survey
      observeEvent(input$OK, {
        # Display end text 
        if (input$OK > n.total) {
          # Display end text 
          output$end <- renderText(end.text)
        }
        # Quit application 
        if (input$OK > (n.total + 1)) {
          # Write data to file
          if (!is.null(data.dir)){
            saveData(data = surveyData, data.dir = data.dir, n.atts = n.atts)
          }
          # Stop application 
          stopApp()
        }
      })
    }
  )
}