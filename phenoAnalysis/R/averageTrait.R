#' calculates average trait values for genotypes
#'
#' calculates average trait values for genotypes
#' @param traitData a data frame of the data set
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @return a dataframe of genotypes mean trait values.
#' @param meansVariable name of the variable averages to calculate for, the default is germplasmName.
#' @export
#'

averageTrait <- function(traitData,
                         traitName,
                         meansVariable='germplasmName') {

  warning('This is just arthemetic average of the trait.')

  traitData <- traitData[, c(meansVariable, traitName)]
  traitData[, traitName] <- as.numeric(as.character(traitData[, traitName]))

  if (sum(is.na(traitData)) > 0) {
    traitData <- na.omit(traitData)
  }

  traitData   <- traitData[order(row.names(traitData)), ]
  traitData   <- data.frame(traitData)


  #calMean <- paste0('mean(', traitName, ', na.rm=TRUE)')
  #aveCol  <- traitName

  traitAverage <- traitData %>%
    group_by(across(all_of(meansVariable))) %>%
    summarise(traitName = mean({{traitName}}))

  traitAverage <- data.frame(traitAverage)
  names(traitAverage)[2] <- paste0(traitName, '_Arithmetic_mean')

  return(traitAverage)

}
