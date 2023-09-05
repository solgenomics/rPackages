#' calculates average trait values for genotypes
#'
#' calculates average trait values for genotypes
#' @param traitData a data frame of the data set
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @return a dataframe of genotypes mean trait values.
#' @param meansVariable name of the variable averages to calculate for, the default is germplasmName.
#' @param averageType character vector of type(s) of averages (mean or median) to calculate, the default is mean.
#' @export
#'

averageTrait <- function(traitData,
                         traitName,
                         meansVariable='germplasmName',
                         averageType=c('mean')) {

  warning('This is just arthemetic average of the trait.')

  traitAverageHeader <- c()
  if(averageType == 'mean') {
    traitAverageHeader <- paste0(traitName, '_Arithmetic_mean')
  } else if (averageType == 'median') {
    traitAverageHeader<- paste0(traitName, '_median')
  } else {
    stop(paste0("Error occured calculating trait average. Can't calculate ", averageType, "."))
  }

  traitData <- traitData[, c(meansVariable, traitName)]
  traitData[, traitName] <- as.numeric(as.character(traitData[, traitName]))

  if (sum(is.na(traitData)) > 0) {
    traitData <- na.omit(traitData)
  }

  traitData   <- traitData[order(row.names(traitData)), ]
  traitData   <- data.frame(traitData)

  traitAverage <- traitData %>%
    group_by(across(all_of(meansVariable))) %>%
    summarise_at(traitName, averageType, na.rm = TRUE)

  traitAverage <- data.frame(traitAverage)
  names(traitAverage)[2] <- traitAverageHeader

  return(traitAverage)

}
