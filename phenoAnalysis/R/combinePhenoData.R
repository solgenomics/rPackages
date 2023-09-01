#' combine phenotype data
#'
#' combines phenotype data from multiple files.
#'
#' @param phenoFiles A vector of the phenotype data files. Phenotype data file name must have a numeric trial id. e.g. file: phenotype_data_141.txt.
#' @param metaDataFile A character vector of meta data file, containing the non-trait cols in the phenotype file.
#' @param keepMetaCols character vector of meta data cols to keep. By default it keeps germplasmName, studyDbId, locationName, studyYear, replicate, blockNumber.
#' @param keepAllTraits a logical (TRUE or FALSE). By default combines only common traits.
#' @return a data.frame of the combined phenotype data.
#' @export
#' @seealso \code{\link[phenoAnalysis]{cleanMetaCols}} which this function uses
#'

combinePhenoData <- function(phenoFiles = NULL,
                             metaDataFile = NULL,
                             keepMetaCols = NULL,
                             keepAllTraits = FALSE) {

  combinedPhenoData <- c()
  cnt               <- 0
  popIds            <- c()
  metaData          <- c()
  commonTraits      <- c()


  if (is.null(keepMetaCols)) {
    keepMetaCols <- c('germplasmName', 'studyDbId', 'locationName',
                      'studyYear', 'replicate', 'blockNumber')
  }

  if(!is.null(metaDataFile)) {
    metaData <- scan(metaDataFile, what="character")
  }

  for (phenoFile in phenoFiles) {
      cnt <- cnt + 1

      phenoData <- getDataFrame(phenoFile)

      trialId <- unique(phenoData$studyDbId)

      phenoData <- cleanMetaCols(metaData = metaData,
                               phenoData= phenoData,
                               keepMetaCols = keepMetaCols)

     allCols    <- names(phenoData)
     traitNames <- allCols[! allCols %in% metaData]

      if (cnt == 1 ) {
        combinedPhenoData <- phenoData
        commonTraits <- traitNames
      } else {

        if (!is.null(phenoData)) {
          if (!isTRUE(keepAllTraits)) {
              commonTraits <- intersect(commonTraits, traitNames)
              selectCols   <- c(keepMetaCols, commonTraits)

              phenoData <- phenoData %>%
                           select(across(all_of(selectCols))) %>%
                           data.frame

              combinedPhenoData <- combinedPhenoData %>%
                                  select(across(all_of(selectCols))) %>%
                                  data.frame

          }

         combinedPhenoData <- bind_rows(combinedPhenoData, phenoData)

       } else {
            message('dataset ', trialId, ' is empty.')
      }
     }
  }

  return(data.frame(combinedPhenoData))

}
