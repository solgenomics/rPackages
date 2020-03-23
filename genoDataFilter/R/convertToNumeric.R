#' convert to numeric
#'
#' A function for converting an entire data.frame or data.table into numeric type.
#'
#' @param x A data.frame or data.table of the dataset
#
#' @return a data.frame or data.table of the dataset with all columns converted to numeric.
#'
#' @export
#'
#'
convertToNumeric <- function(gData) {

  if (length(grep( "data.frame", class(gData))) < 1) {
    stop('The dataset is not a data.frame or data.table')
  }

  origDType <- class(gData)[1]

  if (class(gData)[1] == 'data.frame') {
    gData <- data.table(gData, keep.rownames = TRUE)
  }

  cols <- colnames(gData)
  toNumeric <- function (x) {
    ifelse(sapply(x, is.numeric), x, as.numeric(x))
  }

  gData[, (cols) := lapply(.SD, toNumeric)]

  if (origDType == 'data.frame') {
    gData <- as.data.frame(gData)
  }

  return(gData)

}
