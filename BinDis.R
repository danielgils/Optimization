# Binary to discrete choice matrix.
# 
# Transforms a numeric matrix with binary choice data for each respondent 
# (columns), to a matrix with discrete values representing the chosen 
# alternatives.
# @param y NUmeric matrix containing the binary choice data. Each column is a 
#   different ID.
# @param n.alts Numeric value indicating the number of alternatives per choice 
#   set.
# @param no.choice Logical value indicating whether a no choice response could 
#   be observed. This would be a \code{0} for each alternative.
# @return A matrix with discrete values, indicating the chosen alternatives per
#   ID.
# @examples  
# # Binary response data, 2 participants
# y <- matrix(data = c(0,1,1,0,0,0,0,1), ncol = 2, byrow = FALSE)
# # no choice = TRUE 
# BinDis(y = y, n.alts = 2, no.choice = TRUE)
BinDis <- function(y, n.alts, no.choice) {
  # y matrix.
  if (!is.matrix(y)) {
    stop('y should be a matrix.')
  }
  # Error no.choice
  if(!is.logical(no.choice)) {
    stop('no.choice should be logical.')
  }
  # Create choice sets.
  f <- function(x) {
    split(x, ceiling(seq_along(x) / n.alts))
  }
  cs <- apply(y, 2, f)
  # Error n.alts 
  for (i in 1:ncol(y)){
    if((length(unique(lengths(cs[[i]]))) != 1L)){
      stop('length of Y vector does match expected length based on nr of alternatives.')
    }
  }
  # Index 1.
  Ones <- function(x) {
    xx <- (x == 1)
    ione <- which(xx, arr.ind = TRUE)
    if(length(ione) > 1) {
      stop('Multiple alternatives are chosen per choice set. The response data or the number of alternatives is probably incorrect.')
    }
    # When no choice was chosen
    if (length(ione) == 0) {
      if (!no.choice) {
        stop('no choice responses detected while no.choice = FALSE.')
      }
      ione <- 0
    }
    return(ione)
  }
  yy <- list()
  for(c in 1:ncol(y)){
    yy[[c]] <- lapply(cs[[c]], Ones)
  }
  # Rbind.
  ynom <- lapply(yy, rbind)
  y.nom <- matrix(unlist(ynom), ncol = ncol(y), byrow = FALSE)
  return(y.nom)
}