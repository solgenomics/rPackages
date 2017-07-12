#' rounds decimals
#'
#' A function for rounding decimals in genotype columns to create [0, 1, 2].
#'
#' @param x A data.frame or data.table of the genotype dataset
#
#' @return a data.frame or data.table of the genotype dataset with rounded allele dosages.
#'
#' @export
#'
#'
roundAlleleDosage <- function(gData) {

  if (length(grep( "data.frame", class(gData))) < 1) {
    stop('The genotype dataset is not a data.frame or data.table')
  }

  origDType <- class(gData)[1]

  if (class(gData)[1] == 'data.frame') {
    gData <- data.table(gData, keep.rownames = TRUE)
  }

  cols <- colnames(gData)
  rnd <- function (x) {
    ifelse(is.numeric(x), round(x, 0), x)
  }

  gData[, (cols) := lapply(.SD, rnd), .SDcols=cols]

  if (origDType == 'data.frame') {
    gData <- as.data.frame(gData)
  }

  return(gData)

}
