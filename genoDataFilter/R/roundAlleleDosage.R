#' rounds decimals
#'
#' A function for rounding a dataset to a desired decimal place
#'
#' @param gData A data.frame or data.table of the dataset
#
#' @param digits an integer, decimal places to round. Defaults to 0.
#
#' @return a data.frame or data.table of the genotype dataset with rounded values.
#'
#' @export
#'
#'
roundAlleleDosage <- function(gData=gData, digits=0) {

  if (length(grep( "data.frame", class(gData))) < 1) {
    stop('The genotype dataset is not a data.frame or data.table')
  }

  origDType <- class(gData)[1]

  if (origDType == 'data.frame') {
    gData <- data.table(gData, keep.rownames = TRUE)
  }

  cols <- colnames(gData)
  rnd <- function (x) {
    ifelse(sapply(x, is.numeric), round(x, digits), x)
  }

  gData[, (cols) := lapply(.SD, rnd)]

  if (origDType == 'data.frame') {
    gData <- data.frame(gData)
    gData <- column_to_rownames(gData, 'rn')

  }

  return(gData)

}
