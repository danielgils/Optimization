#' Load numeric choice data from directory
#' 
#' Reads all individual choice data files from a directory and concatenates 
#' those files into a single data file. Files containing either "num" or "char"
#' will be read, with num indicating numeric data and char indicating character
#' data. for more information see output of \code{\link{SurveyApp}}.
#' 
#' @param data.dir A character string containing the directory to read from.
#' @param type Character vector containing either num or char. 
#' @return A data frame containg the full design and all the responses of the 
#'   combined data files that where found. Different files are indicated by an
#'   ID variable.
#' @export
LoadData <- function(data.dir, type) {
  # ErrorS
  if(!type %in% c("num", "char")){
    stop("type must be either num or char")
  }
  if (!dir.exists(data.dir)) {
    stop("Directory data.dir does not exist")
  }
  error <- character(0)
  if(identical(list.files(data.dir, full.names = TRUE, pattern = type), error)){
    stop('No files of the specified type in data.dir')
  } 
  # Read all files into list
  files <- list.files(data.dir, full.names = TRUE, pattern = type)
  data <- lapply(files, utils::read.table, stringsAsFactors = FALSE, sep = '\t', header = T) 
  # matrix
  id_rows <- sapply(data, nrow)
  id <- rep(1:length(id_rows), id_rows)
  data <- lapply(data, as.data.frame)
  # Bind together
  data <- do.call(rbind, data)
  data <- cbind("ID" = id, data)
  return(data)
}