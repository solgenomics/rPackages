#' Removes rows with completely empty trait values for a block
#'
#' Given a dataset with the BrAPI style column naming, removes rows with completely empty trait values for a block
#'
#' @param traitData a data frame of the trial dataset.
#' @param traitName the name of the trait
#' @return a data frame of the dataset excluding the rows from the block compeletly with out values for the trait.
#' @export
#'
cleanEmptyBlocks <- function(traitData, traitName=NULL) {

  if (is.null(traitData)) stop('No Trait data to check.')
  if (is.null(traitName)) stop('Please specify the trait name for which
                               you want to clean empty blocks or replicates.')

  if (length(unique(traitData$blockNumber)) > 1) {

    for (blk in unique(traitData$blockNumber)) {

      traitValues <- subset(traitData, blockNumber == blk, select=traitName)

      if (nrow(traitValues) == sum(is.na(traitValues))) {
        traitData <- traitData[traitData$blockNumber != blk, ]
      }
    }

  }

  return(traitData)
}

