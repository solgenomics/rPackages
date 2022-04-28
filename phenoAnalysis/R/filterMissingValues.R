#' Filters out columns and rows by percentage of missing value
#'
#' @param phenoData a data frame of the phenotype dataset
#' @param rowMissingFilter a decimal missing values cut off to apply to rows. By default,
#'        rows with missing values in all columns are removed.
#' @param colMissingFilter a decimal missing values cut off to apply to columns. By default,
#'        columns with missing values in all rows are removed.
#' @param traits a vector of trait columns to filter. By default,
#'        filters are applied to all columns.
#' @return a data frame of the phenotype dataset without the columns and rows
#' with missing values above the given percentages.
#' @export
#'
filterMissingValues <- function(phenoData=NULL,
                          rowMissingFilter=1,
                          colMissingFilter=1,
                          traits=NULL) {

  if (is.null(phenoData)) {
    stop('You have not provided a data frame of a dataset or it is an empty data frame.')
  }

  if (rowMissingFilter < 0 | rowMissingFilter > 1) {
    stop('Row missing values filter cut off must be between 0 and 1.')
  }

  if (colMissingFilter < 0 | colMissingFilter > 1) {
    stop('Column missing values filter cut off must be between 0 and 1.')
  }

  phenoData <- data.frame(phenoData)
  if (is.null(traits)) {
    traits <- colnames(phenoData)
  }

  if (colMissingFilter > 0) {
    phenoData <- phenoData %>%
                 select_if(function(x) sum(is.na(x)) < colMissingFilter * nrow(phenoData))
  }

  if (rowMissingFilter > 0) {
     phenoData <- phenoData %>%
                 filter_at(traits, all_vars(sum(is.na(.)) < rowMissingFilter * ncol(phenoData)))
  }

  return (phenoData)
}
