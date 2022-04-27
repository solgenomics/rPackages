#' groups by given factor and calculates summary data
#'
#' Groups by given factor and calculates summary data
#'
#' @param phenoData a data frame of the phenotype dataset
#' @param traits a character vector of traits summary to calculate for.
#'               If not given, summary will be calculated for all traits in the data set.
#' @param groupBy a vector of the columns to group by, defaults to germplasmName.
#' @param summaryStat a vector of the summary statistics to calculate, defaults to mean.
#' @return a data frame of the phenotype dataset with summarized by the grouping factors.
#' @export
#'
summarizeTraits <- function(phenoData=NULL,
                            traits=NULL,
                            groupBy=c('germplasmName'),
                            summaryStat=c('mean')) {

  if (is.null(phenoData)) {
  stop('No dataset given. ')
  }

  allCols <- names(phenoData)
  if (!is.null(traits)){
    traitCols <- traits
  } else {
    traitCols <- allCols[! allCols %in% groupBy]
  }

  phenoData   <- phenoData[which(rowSums(is.na(phenoData)) != length(traitCols) ), ]

  summaryData <- phenoData %>%
                 group_by_(.dots=groupBy) %>%
                 summarise_at(traitCols, summaryStat, na.rm=TRUE) %>%
                 select(which(colSums(is.na(.)) == 0))

  return (summaryData)
}
