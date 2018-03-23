#' sorts trait data by design factors for further analysis
#'
#' Given a dataset with the BrAPI style column naming, sorts trait data by design factors for further analysiss.
#'
#' @param traitData a data frame of the trial dataset.
#' @return a data frame of the dataset sorted by block number, replicate or both.
#' @export
#'
#'
sortTraitData <- function(traitData) {
  if (is.null(traitData)) stop('No Trait data to sort.')

  if (length(unique(traitData$blockNumber)) > 1) {

    if (length(unique(traitData$replicate)) > 1 ) {
      traitData <- arrange(traitData, blockNumber, replicate)
    } else {
      traitData <- arrange(traitData, blockNumber)
    }

  } else if  (length(unique(traitData$blockNumber)) <= 1) {

     if (length(unique(traitData$replicate)) > 1) {
      traitData <- arrange(traitData, replicate)
     }

  }

  return (traitData)
}
