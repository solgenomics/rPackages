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

  phenoData <- filterMissingValues(phenoData, traits=traits)
  phenoData <- getNumericCols(phenoData)

  allCols <- names(phenoData)
  traitsCols <- c()
  if (!is.null(traits)){
    traitsCols <- traits
  } else {
    traitsCols <- allCols[! allCols %in% groupBy]
  }

  traitsCols <- intersect(traitsCols, allCols)

  if (!any(grepl('germplasmName', colnames(phenoData)))) {

    phenoData <- rownames_to_column(phenoData, 'germplasmName')

    if (is.null(groupBy)) {
      groupBy <- c('germplasmName')
    }
  }


  summaryData <- phenoData %>%
                 group_by(across(all_of(groupBy))) %>%
                 summarise_at(traitsCols, summaryStat, na.rm=TRUE) %>%
                 select(where(~!all(is.na(.x))))

  summaryData[summaryData == 'NaN'] <- NA

  return (summaryData)
}
