#' removes non-numeric columns from a data frame
#'
#' @param phenoData a data frame of the phenotype dataset
#' @param traits a vector of trait columns to check if they are numeric or not. By default,
#'        all columns checked.
#' @return a data frame of the phenotype dataset with without the non-numeric variables.
#' @export
#'
getNumericCols <- function(phenoData=NULL, traits=NULL) {

  if (is.null(phenoData)) {
    stop('You have not provided a data frame of a dataset or it is an empty data frame.')
  }

  # if (is.null(traits)) {
  #   stop('You need to provide a vector of the traits/columns to screen for numeric data types.')
  # }
  phenoData <- data.frame(phenoData)
  if (is.null(traits)) {
    traits <- colnames(phenoData)
  }

  phenoData <- phenoData %>%
               select(traits) %>%
               select_if(is.numeric)

  return (phenoData)

}
