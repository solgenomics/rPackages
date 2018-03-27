#' structures trait data for further analysis
#'
#' Given a dataset with the BrAPI style column naming, it creates a trait dataset for further analysis.
#'
#' @param trialData a data frame of the trial(s) dataset.
#' @param traitName a character vector of the trait of interest.
#' @return a data frame of the trait dataset with columns 'germplasmName', 'locationName', 'studyYear', 'blockNumber', 'replicate', trait Name
#'
#' @export
#'

structureTraitData <- function (trialData, traitName=NULL) {

  if (is.null(traitName)) stop('Trait argument is missing.')
  if (is.null(trialData)) stop('Trial data argument is missing.')

  trialData$replicate   <- as.factor(trialData$replicate)
  trialData$blockNumber <- as.factor(trialData$blockNumber)
  trialData$studyYear   <- as.factor(trialData$studyYear)

  colList <- c('germplasmName', 'locationName', 'studyYear',
               'blockNumber', 'replicate', traitName)

  traitData <- trialData[, colList]

  if (class(traitData[, traitName]) != 'numeric') {
    traitData[, traitName] <- as.numeric(as.character(traitData[, traitName]))
  }

  traitData <- sortTraitData(traitData)
  traitData <- cleanEmptyBlocks(traitData, traitName)
  traitData <- cleanEmptyReps(traitData, traitName)

  return(traitData)
}



