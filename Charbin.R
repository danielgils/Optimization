#' Character vector to binary vector.
#' 
#' Transforms a character vector with responses into a binary vector. Each
#' alternative in each choice set wil be either 0 or 1. If the
#' alternative was not chosen 0, and 1 if it was. The function can be used for example in a
#' shiny application to transform the response vector received from
#' \code{\link[shiny]{radioButtons}} into a numeric vector that can be used for
#' estimation.
#' 
#' The \code{n.alts} denotes the number of alternatives a respondent could
#' choose from, without counting a possible no choice option.
#' 
#' If \code{no.choice} is \code{TRUE} the first alternative specified in 
#' \code{alts} will be treated as a no choice option. If the no choice option 
#' was chosen all alternatives are zero for that choice set.
#' @param resp String vector containing input responses
#' @param alts String vector containing all possible alternatives. The order
#'   should be the same as the order of the design matrix.
#' @param n.alts The number of alternatives per choice set.
#' @param no.choice Logical value indicating whether a no.choice option is 
#'   provided or not. The default = \code{FALSE}.
#' @return A binary response vector with length equal to \code{length(resp) *
#'   length(n.alts)}.
#' @examples 
#' \donttest{
#' # Observed Responses 
#' resp <- c("alt1", "alt3", "alt2", "no.choice", "alt1") 
#' # All possible alternatives 
#' alts <- c("no.choice", "alt1", "alt2", "alt3")
#' # 3 alternatives + no.choice 
#' Charbin(resp = resp, alts = alts, n.alts = 3, no.choice = TRUE)
#' }
Charbin <- function (resp, alts, n.alts, no.choice = FALSE) {
  # Error resp not in altsions
  if (!all(resp %in% alts)) {
    stop("1 or more responses do not match the possible response options.")
  }
  # Error altsions
  if (length(alts) != (n.alts + no.choice)) {
    stop("Number of response options is not correct")
  }
  map <- match(resp, alts)
  l <- list()
  for(i in 1:length(map)){
    l[[i]] <- rep(0, n.alts)
    if (no.choice) {
      l[[i]][map[i] - 1] <- 1
    } else {
      l[[i]][map[i]] <- 1
    }
  }
  v <- unlist(l)
  return(v)
}