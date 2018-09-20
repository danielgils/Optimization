#' Coded choice set to character choice set.
#' 
#' Transforms a coded choice set into a choice set containing character attribute
#' levels, ready to be used in a survey.
#' 
#' In \code{lvl.names}, the number of character vectors in the list should equal
#' the number of attributes in de choice set. The number of elements in each 
#' character vector should equal the number of levels for that attribute.
#' 
#' Valid arguments for \code{coding} are \code{C}, \code{D} and \code{E}. When
#' using \code{C} the attribute will be treated as continuous and no coding will
#' be applied. All possible levels should then be specified in \code{c.lvls}. If
#' \code{D} (dummy coding) is used \code{\link{contr.treatment}} will be applied
#' to that attribute. The first attribute wil be used as reference level.  For
#' \code{E} (effect coding) \code{\link{contr.sum}} is applied, in this case the
#' last attributelevel is used as reference level.
#' 
#' @param set A numeric matrix which represents a choice set. Each row is a
#'   profile.
#' @param lvl.names A list containing character vectors with the values of each
#'   level of each attribute.
#' @param coding A character vector denoting the type of coding used for each
#'   attribute. See also \code{\link{Profiles}}.
#' @param alt.cte A binary vector indicating for each alternative if an 
#'   alternative specific constant is present. The default is \code{NULL}. 
#' @inheritParams Profiles
#' @inheritParams Modfed
#' @return A character matrix which represents the choice set.
#' @examples 
#' \donttest{
#' # Example without continuous attributes.
#' l <- c(3, 4, 2) # 3 Attributes.
#' c <- c("D", "E", "D") # Coding.
#' # All profiles.
#' p <- Profiles(lvls = l, coding = c)
#' cs <- p[c(4, 8), ] # Choice set 
#' # Levels as they should appear in survey. 
#' al <- list(
#'  c("$50", "$75", "$100"), # Levels attribute 1.
#'  c("2 min", "15 min", "30 min", "50 min"), # Levels attribute 2.
#'  c("bad", "good") # Levels attribute 3.
#' ) 
#' # Decode
#' Decode(set = cs, lvl.names = al, coding = c, alt.cte = c(0, 0)) 
#'
#' # Example with continuous attribute.
#' l <- c(3, 4, 2) # 3 Attributes.
#' c <- c("D", "C", "D") # Coding.
#' cl <- list(c(50, 75, 80, 100))
#' # All profiles.
#' p <- Profiles(lvls = l, coding = c, c.lvls = cl)
#' cs <- p[c(4, 8), ] # Set. 
#' a <- c(1, 0) # Alternative specific constant. 
#' cs <- cbind(a, cs) # set with alt.cte
#' # Levels as they should appear in survey. 
#' al <- list(
#'   c("$50", "$75", "$100"), # Levels attribute 1.
#'   c("50 min", "75 min", "80 min", "100 min"), # Levels attribute 2.
#'   c("bad", "good") # Levels attribute 3.
#' ) 
#' # Decode
#' Decode(set = cs, lvl.names = al, coding = c, alt.cte = c(1, 0), c.lvls = cl) 
#' }
Decode <- function(set, lvl.names, coding, alt.cte = NULL, c.lvls = NULL) {
  
  if(!is.null(alt.cte)) {
    contins <- which(alt.cte == 1)
    if( !length(contins) == 0) {
      set <- set[, -length(contins)]
    }
  }
  
  n.alts <- nrow(set) # Number of alternatives.
  n.att <- length(lvl.names) # Number of attributes.
  conts <- which(coding == "C") # Continuous levels. 
  # Create vector where each element denotes the number of levels for each attribute.
  lvls <- numeric(n.att) 
  for (i in 1:n.att) { 
    lvls[i] <- length(lvl.names[[i]])
  }
  # Generate all possible profiles coded and uncoded
  dc <- Profiles(lvls = lvls, coding = coding, c.lvls = c.lvls)
  # Create uncoded grid. 
  d <- as.data.frame(expand.grid(lvl.names))
  # Create new matrix for choice set with attribute level names 
  m <- matrix(data = NA, nrow = n.alts, ncol = n.att)
  # Error handling
  if (ncol(set) != ncol(dc)) {
    stop("Number of columns of the set does not match expected number based on the other arguments.")
  }
  # For each alternative look for matching profile  
  for (i in 1:n.alts) {
    # if coded choice set, look for match in coded version first, then take uncoded equivalent.
    lev.num <- d[as.numeric(which(apply(dc, 1, function(x) all(x == set[i, ])))), ]
    lev.num <- as.numeric(lev.num)
    # Error handling
    if (any(is.na(lev.num))) { 
      stop('The set does not match with the type of coding provided')
    }
    # For each attribute fill in the attribute level name
    for (c in 1:n.att) {
      m[i, c] <- lvl.names[[c]][lev.num[c]]
    }
  }
  return(m)
}
