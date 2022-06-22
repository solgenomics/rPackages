#' Reads phenotype data from a file.
#'
#'  reads phenotype data from a file..
#'
#' @param phenoFile a character vector of phenotype file.
#'
#' @return a data frame of the data from the given file.
#' @export
#'

getDataFrame <- function(phenoFile) {

  phenoData <- c()
  if (is.null(phenoFile)) {
    stop('No data file given.')
  } else {
    phenoData <- fread(phenoFile,
                       sep= "\t",
                       stringsAsFactors = FALSE,
                       na.strings = c("NA", "", "--", "-", "."))

    phenoData <- data.frame(phenoData)
    if (is.null(phenoData)) {
      stop("The file is empty.")
      q("no", 1, FALSE)
    }
  }

  return(phenoData)

}
