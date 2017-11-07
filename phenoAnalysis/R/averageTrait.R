#' calculates average trait values for genotypes
#'
#' calculates average trait values for genotypes
#' @param traitData a data frame of the data set
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @return a dataframe of genotypes mean trait values.
#' @export
#'

averageTrait <- function(traitData, traitName) {
  warning('This is just arthemetic average of the trait.')

  traitData <- traitData[, c('germplasmName', traitName)]

  if (sum(is.na(traitData)) > 0) {
    traitData <- na.omit(traitData)
  }

  traitData   <- traitData[order(row.names(traitData)), ]
  traitData   <- data.frame(traitData)

  calMean <- paste0('mean(', traitName, ', na.rm=TRUE)')
  aveCol  <- traitName

  traitAverage <- traitData %>%
    group_by(germplasmName) %>%
    summarise_(.dots = setNames(calMean, aveCol))

  traitAverage <- data.frame(traitAverage)
  colnames(traitAverage)[1] <- 'genotypes'

  return(traitAverage)

}
