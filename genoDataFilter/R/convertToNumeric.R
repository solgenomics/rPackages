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
  rows <- c()

  if (class(gData)[1] == 'data.frame') {
    rows <- rownames(gData)
    gData <- data.table(gData, keep.rownames = TRUE)
  }

  toNumeric <- function (x) {
    ifelse(sapply(x, is.numeric), x, as.numeric(x))
  }

  cols <- colnames(gData)
  gData[, (cols) := lapply(.SD, toNumeric)]

  if (origDType == 'data.frame') {
    gData <- as.data.frame(gData)
    rownames(gData) <- rows
    gData[, 'rn']   <- NULL
  }

  return(gData)

}
