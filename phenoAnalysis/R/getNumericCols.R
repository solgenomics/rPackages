#' removes non-numeric columns from a data frame
#'
#' @param phenoData a data frame of the phenotype dataset
#' @param exception a vector of columns to skip checking if they are numeric.
#'                By default, it skips the 'germplasmName' column.
#' @return a data frame of the phenotype dataset with without the non-numeric variables.
#' @export
#'
getNumericCols <- function(phenoData=NULL, exception=c('germplasmName')) {

  if (is.null(phenoData)) {
    stop('You have not provided a data frame of a dataset or it is an empty data frame.')
  }

  # if (is.null(traits)) {
  #   stop('You need to provide a vector of the traits/columns to screen for numeric data types.')
  # }

  phenoData <- data.frame(phenoData)

  phenoData <- phenoData %>%
               select(matches(exception), names(select_if(., ~is.numeric(.x))))

  return (phenoData)

}
