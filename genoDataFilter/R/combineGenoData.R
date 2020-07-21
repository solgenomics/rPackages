#' combine genotype data
#'
#' combines genotype data from multiple populations.
#'
#' @param allGenoFiles A vector of the genotype data files. Genotype data file name must have a numeric trial id. e.g. file: genotype_data_141.txt.
#
#' @return a data.frame of the combined genotype data, with an additional column ('trial') indicating population origin of the individual.
#' @export
#'
#'

combineGenoData <- function(allGenoFiles = NULL) {

  combinedGenoPops <- c()
  cnt              <- 0
  popIds           <- c()
  comboGenoMetaData     <- c()

  for (popGenoFile in allGenoFiles) {

    uniqGenoNames <- c()
    cnt <- cnt + 1

    genoData <- fread(popGenoFile,
                     na.strings = c("NA", " ", "--", "-"))

    genoData <- data.frame(genoData)
    #message('cnt of genotypes in dataset: ', length(rownames(genoData)))
    genoData <- genoData[!duplicated(genoData[,'V1']), ]
    #message('cnt of unique genotypes in dataset: ', length(rownames(genoData)))
    rownames(genoData) <- genoData[, 1]
    genoData[, 1] <- NULL

    popGenoFile <- basename(popGenoFile)
    popId       <- str_extract(popGenoFile, "\\d+")
    popIds      <- c(popIds, popId)

    genoData$trial <- popId
    genoData <- rownames_to_column(genoData, var='genotypes')
    genoData <- genoData %>% select(genotypes, trial, everything())

    if (cnt == 1 ) {
        combinedGenoPops <- genoData
        genoMetaData <- genoData %>%  select(genotypes, trial)
        comboGenoMetaData <- genoMetaData
     } else {

       genoMetaData <- subset(genoData, select=c('genotypes', 'trial'))

       comboGenoMetaData <- full_join(comboGenoMetaData, genoMetaData, by="genotypes")

        uniqGenoNames <- unique(rownames(combinedGenoPops))

        #message('cnt of genotypes in new dataset ', popId, ': ',  length(rownames(genoData)) )

        genoData <- genoData[!(rownames(genoData) %in% uniqGenoNames),]

        #message('cnt of unique genotypes from new dataset ', popId, ': ', length(rownames(genoData)))

      if (!is.null(genoData)) {
          combinedGenoPops <- rbind(combinedGenoPops, genoData)
       } else {
         # message('dataset ', popId, ' has no unique genotypes.')
      }
    }
  }

  concatenate <- function (dt) {
    conVars <- grep('trial', names(dt), value = TRUE)

    tr_gr <- apply(dt[, conVars], 1, function (x) {
        x = na.omit(x)
        x = paste(x, collapse = '-')}
        )
    return(tr_gr)
  }

  comboGenoMetaData <- comboGenoMetaData %>%
                          mutate_at(vars(starts_with('trial')), as.numeric)

  comboGenoMetaData <- comboGenoMetaData %>%
                          mutate(tr_gr = concatenate(.))

  comboGenoMetaData <- comboGenoMetaData %>%
                          select(genotypes, tr_gr)

  combinedGenoPops <- full_join(comboGenoMetaData, combinedGenoPops, by = 'genotypes')
  combinedGenoPops <- combinedGenoPops %>%
                         select(genotypes, tr_gr, everything()) %>%
                         select(-trial) %>%
                         rename(trial = tr_gr)

  #combinedGenoPops <- combinedGenoPops[order(rownames(combinedGenoPops)), ]
  return(combinedGenoPops)

}
