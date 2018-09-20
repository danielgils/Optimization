#Function to save the data gathered by shiny app
saveData <- function(data, data.dir, n.atts) {
  # Data manipulation 
  d <- as.data.frame(cbind(data$desing, resp = data$bin.responses))
  unc_resp <- rep(data$responses, each = n.atts) 
  unc_setnr <- rep(1:length(data$responses), each = n.atts)
  unc_d <- cbind(set = unc_setnr, data$survey, resp = unc_resp) 
  # Create unique file names
  numname <- sprintf("%s_num_data.txt", as.integer(Sys.time()))
  charname <- sprintf("%s_char_data.txt", as.integer(Sys.time()))
  # Write files to data.dir
  utils::write.table(
    x = d,
    file = file.path(data.dir, numname), 
    row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
  )
  utils::write.table(
    x = unc_d,
    file = file.path(data.dir, charname), 
    row.names = TRUE, quote = FALSE, sep = "\t", col.names = NA
  )
}