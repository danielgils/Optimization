# Create row and column names for designs 
Rcnames <- function(n.sets, n.alts, n.cte, alt.cte) {
  # rownames
  r.s <- rep(1:n.sets, each = n.alts)
  r.a <- rep(1:n.alts, n.sets)
  r.names <- paste(paste("set", r.s, sep = ""), paste("alt", r.a, sep = ""), sep = ".")
  # colnames alternative specific constants 
  cte.names <- paste(paste("alt", which(alt.cte == 1), sep = ""), ".cte", sep = "") 
  # return
  return(list(r.names, cte.names))
}