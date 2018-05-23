#' combine genotype data
#'
#' combines genotype data from multiple populations.
#'
#' @param allGenoFiles A vector of the genotype data files. Genotype data file name must have a numeric trial id. e.g. file: genotype_data_141.txt.
#
#' @return a data.frame of the combined genotype data, with an additional column ('trial') indicating population origin of the individual.
#' @import stringr
#'         dplyr
#' @export
#'
#'

combineGenoData <- function(allGenoFiles = NULL) {

  combinedGenoPops <- c()
  cnt              <- 0
  popIds           <- c()

  for (popGenoFile in allGenoFiles) {

    uniqGenoNames <- c()
    cnt <- cnt + 1

    genoData <- fread(popGenoFile,
                     na.strings = c("NA", " ", "--", "-"))

    genoData <- data.frame(genoData)
    message('cnt of genotypes in dataset: ', length(rownames(genoData)))
    genoData <- genoData[!duplicated(genoData[,'V1']), ]
    message('cnt of unique genotypes in dataset: ', length(rownames(genoData)))
    rownames(genoData) <- genoData[, 1]
    genoData[, 1] <- NULL

    popGenoFile <- basename(popGenoFile)
    popId       <- str_extract(popGenoFile, "\\d+")
    popIds      <- c(popIds, popId)
    trialGenos <- paste0(rownames(genoData), '_', popId)
    rownames(genoData) <- trialGenos
    genoData$trial <- popId
    genoData       <- genoData %>% select(trial, everything())

    if (cnt == 1 ) {
        print('no need to combine, yet')
        message('cnt of genotypes first dataset: ', length(rownames(genoData)))
        combinedGenoPops <- genoData

     } else {
        print('combining genotype datasets...')

        uniqGenoNames <- unique(rownames(combinedGenoPops))

        message('cnt of genotypes in new dataset ', popId, ': ',  length(rownames(genoData)) )

        genoData <- genoData[!(rownames(genoData) %in% uniqGenoNames),]

        message('cnt of unique genotypes from new dataset ', popId, ': ', length(rownames(genoData)))

      if (!is.null(genoData)) {
          combinedGenoPops <- rbind(combinedGenoPops, genoData)
       } else {
          message('dataset ', popId, ' has no unique genotypes.')
      }
    }
  }

  return(combinedGenoPops)
  #combinedGenoPops <- combinedGenoPops[order(rownames(combinedGenoPops)), ]

}
