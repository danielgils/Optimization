#' Profiles generation.
#' 
#' Function to generate all possible combinations of attribute levels (i.e. all 
#' possible profiles).
#' 
#' Valid arguments for \code{coding} are \code{C}, \code{D} and \code{E}. When
#' using \code{C} the attribute will be treated as continuous and no coding will
#' be applied. All possible levels should then be specified in \code{c.lvls}. If
#' \code{D} (dummy coding) is used \code{\link{contr.treatment}} will be applied
#' to that attribute. For \code{E} (effect coding) \code{\link{contr.sum}} will
#' be applied.
#' 
#' @param lvls  A numeric vector which contains for each attribute, the number
#'   of levels.
#' @param coding Type op coding that needs to be used for each attribute.
#' @param c.lvls A list containing numeric vectors with the attributelevels for
#'   each continuous attribute. The default is \code{NULL}.
#' @return A numeric matrix which contains all possible profiles.
#' @examples 
#' # Without continuous attributes
#' at.lvls <- c(3,4,2) # 3 Attributes with respectively 3, 4 and 2 levels. 
#' c.type <- rep("E", length(at.lvls)) # All Effect coded.
#' Profiles(lvls = at.lvls, coding = c.type) # Generate profiles.
#' 
#' # With continuous attributes 
#' at.lvls <- c(3,4,2) # 3 attributes with respectively 3, 4 and 2 levels. 
#' # First attribute is dummy coded, second and third are continuous. 
#' c.type <- c("D", "C", "C") 
#' # Levels for continuous attributes, in the same order. 
#' con.lvls <- list(c(4,6,8,10), c(7,9))
#' Profiles(lvls = at.lvls, coding = c.type, c.lvls = con.lvls)
#' @export
Profiles <- function(lvls, coding, c.lvls = NULL) {
  # Continuous attributes. 
  contins <-  which(coding == "C")
  n.contins <-  length(contins)
  # error continuous levels 
  if (!is.null(c.lvls) && !is.list(c.lvls)) { 
    stop('c.lvls should be a list.')
  }
  # Error correct coding types.
  codings.types <- c("E", "D", "C")
  if (!all(coding %in% codings.types) || (length(coding) != length(lvls))) {
    stop("coding argument is incorrect.")
  } 
  # Error lvls vector.
  if (length(lvls) < 2 || (!(is.numeric(lvls)))){
    stop("lvls argument is incorrect.")
  }
  # Error continuous specified and NULL.
  if (length(contins) > 0 && is.null(c.lvls)) {
    stop("there are no levels provided for the continuous attributes")
  }
  # Error continuous levels specification. 
  if (!is.null(c.lvls)) {
    if (length(c.lvls) != n.contins) {
      stop("length of c.lvls does not match number of specified continuous attributes in coding")
    }
    # Error c.lvls same number of levels. 
    if (!isTRUE(all.equal(lvls[contins], lengths(c.lvls)))) {
      stop("the number of continuous attribute levels provided in c.lvls does not match the expected based on lvls")
    }
  }
  # Change into correct coding. 
  coding <- dplyr::recode(coding, D = "contr.treatment", E = "contr.sum")
  # Create all combinations of attribute levels.
  levels.list <- lapply(X = as.list(lvls), function(x) (1:x))
  # Replace continuous.
  levels.list[contins] <- c.lvls
  # Create grid. 
  dgrid <- as.data.frame(expand.grid(levels.list))
  # Apply coding to non continuous. 
  cn <- names(dgrid)
  if (!is.null(c.lvls)) {
    cn <- cn[-contins]
  }
  # Create factors.
  dgrid[, cn] <- apply(dgrid[, cn, drop = FALSE], 2, factor)
  # coding 
  con <- as.list(stats::setNames(coding, names(dgrid)))
  con[which(con == "C")] <- NULL
  cgrid <- as.data.frame(stats::model.matrix(~., dgrid, contrasts = con))
  # Delete intercept.
  cgrid <- cgrid[, -1]
  # Return profiles.
  return(as.matrix(cgrid))
}
