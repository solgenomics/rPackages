#' groups by given factor and calculates summary data
#'
#' Groups by given factor and calculates summary data
#'
#' @param phenoData a data frame of the phenotype dataset
#' @param groupBy a vector of the columns to group by, defaults to germplasmName.
#' @param summaryStat a vector of the summary statistics to calculate, defaults to mean.
#' @return a data frame of the phenotype dataset with summarized by the grouping factors.
#' @export
#'
summarizeTraits <- function(phenoData=NULL, groupBy=c('germplasmName'), summaryStat=c('mean')) {

  if (is.null(phenoData)) {
  stop('No dataset given. ')
  }

  allCols <- names(phenoData)
  traitCols <- allCols[! allCols %in% groupBy]
  naTraits <- c()

  # for (i in traitCols) {
  #   if (class(phenoData[, i]) != 'numeric') {
  #       phenoData[, i] <- as.numeric(as.character(phenoData[, i]))
  #   }
  #
  #   if (all(is.nan(phenoData[, i]))) {
  #       phenoData[, i] <- sapply(phenoData[, i], function(x) ifelse(is.numeric(x), x, NA))
  #   }
  #
  #   if (sum(is.na(phenoData[,i])) > (0.5 * nrow(phenoData))) {
  #       phenoData$i <- NULL
  #       naTraits <- c(naTraits, i)
  #       message('dropped trait ', i, ' no of missing values: ', sum(is.na(phenoData[,i])))
  #   }
  # }

  summaryData <- phenoData %>%
                 group_by_(.dots=groupBy) %>%
                 summarise_at(traitCols, summaryStat, na.rm=TRUE) %>%
                 data.frame

}
