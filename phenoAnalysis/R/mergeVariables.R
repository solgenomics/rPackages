#' merges variables from 2 or more data files.
#'
#' merges variables from 2 or more data files.
#' @param dataFiles a vector of data files
#' @param ... args to pass to 'full_join' function from dplyr.
#' @return a dataframe of the combined variables.
#' @export
#' @seealso \code{\link[dplyr]{full_join}} which this function uses

mergeVariables <- function(dataFiles=NULL, ...) {

  if (is.null(dataFiles)) {
    stop('Expected a vector of data files, but found none.')
  }

  if (length(dataFiles) == 1) {
    warning('There is only one data file. Therefore, returning variables from that file only.')
  }

  cnt <- 0
  combinedData <- c()
  for (i in dataFiles) {

      cnt <- cnt + 1
      newData <- fread(i, na.strings = c('na', '-', 999))

      if(cnt == 1) {
        combinedData <- newData
      } else {
        combinedData <- full_join(newData, combinedData, ...)
      }
  }
      combinedData <- data.frame(combinedData)
      if (length(combinedData$V1) > 1) {
          combinedData <- column_to_rownames(combinedData, 'V1')
      }

      return(combinedData)
}
