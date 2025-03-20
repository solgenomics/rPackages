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

    cnt <- cnt + 1

    genoData <- fread(popGenoFile,
                     na.strings = c("NA", " ", "--", "-"))

    genoData <- data.frame(genoData)
    genoData <- genoData[!duplicated(genoData[,'V1']), ]
    genoData <- rename(genoData, genotypes='V1')

    popGenoFile <- basename(popGenoFile)
    popId <- str_extract(popGenoFile, "list_\\d+|dataset_\\d+|\\d+")
    popIds      <- c(popIds, popId)

    genoData$trial <- popId
    genoData <- genoData %>% select(genotypes, trial, everything())

    if (cnt == 1 ) {
        combinedGenoPops <- genoData
        genoMetaData <- genoData %>%  select(genotypes, trial)
        comboGenoMetaData <- genoMetaData
     } else {

       genoMetaData <- genoData %>% select(genotypes, trial)
       comboGenoMetaData <- full_join(comboGenoMetaData, genoMetaData, by="genotypes")

       genoData <- genoData[!(genoData$genotypes %in% unique(combinedGenoPops$genotypes)),]

       if (!is.null(genoData)) {
          combinedGenoPops <- rbind(combinedGenoPops, genoData)
       } else {
         message('dataset ', popId, ' has no unique genotypes.')
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
                          mutate_at(vars(starts_with('trial')), as.character)

  comboGenoMetaData <- comboGenoMetaData %>%
                          mutate(tr_gr = concatenate(.))

  comboGenoMetaData <- comboGenoMetaData %>%
                          select(genotypes, tr_gr)

  combinedGenoPops <- full_join(comboGenoMetaData, combinedGenoPops, by = 'genotypes')
  combinedGenoPops <- combinedGenoPops %>%
                         select(genotypes, tr_gr, everything()) %>%
                         select(-trial) %>%
                         rename(trial = tr_gr)

  combinedGenoPops <- column_to_rownames(combinedGenoPops, var='genotypes')

  #combinedGenoPops <- combinedGenoPops[order(rownames(combinedGenoPops)), ]
  return(combinedGenoPops)

}
