#' Removes rows with completely empty trait values for a replication
#'
#' Given a dataset with the BrAPI style column naming, removes rows with completely empty trait values for a replication
#'
#' @param traitData a data frame of the trial dataset.
#' @param traitName the name of the trait
#' @return a data frame of the dataset excluding the rows from the replication compeletly with out values for the trait.
#' @export
#'
cleanEmptyReps <- function(traitData, traitName=NULL) {

  if (is.null(traitData)) stop('No Trait data to check.')
  if (is.null(traitName)) stop('Please specify the trait name for which
                               you want to clean empty blocks or replicates.')

  if (length(unique(traitData$replicate)) > 1) {

    for (rp in unique(traitData$replicate)) {

      traitValues <- subset(traitData, replicate == rp, select=traitName)

      if (nrow(traitValues) == sum(is.na(traitValues))) {
        traitData <- traitData[traitData$replicate != rp, ]
      }
    }

  }

  return(traitData)
}
