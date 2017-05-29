#' calculates genotype adjusted means
#'
#' calculates the genotype adjusted means for a trait.
#'
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, genotypes variable is considered fixed effects.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

getAdjMeans <- function (trialData, traitName=NULL, genotypeEffectType='fixed') {

  genotypeEffectType <<- genotypeEffectType
  traitData          <- structureTraitData(trialData, traitName=traitName)
  adjMeans           <- c()

  experimentalDesign <- trialData[2, 'studyDesign']
  if (is.na(experimentalDesign) == TRUE) experimentalDesign <- c('No Design')

  if (experimentalDesign == 'RCBD'
      &&  length(unique(traitData$blockNumber)) > 1) {

     if (genotypeEffectType == 'fixed') {
      adjMeans <- fixedRCBD(traitData, traitName)
    } else {
      adjMeans <- randomRCBD(traitData, traitName)
    }

  } else if (experimentalDesign == 'Augmented'
                &&  length(unique(traitData$blockNumber)) > 1) {

    if (genotypeEffectType == 'fixed') {
      adjMeans <- fixedAugmentedRCBD(traitData, traitName)
    } else {
      adjMeans <- randomAugmentedRCBD(traitData, traitName)
    }

  }  else if ((experimentalDesign == 'CRD')
             &&  length(unique(traitData$replicate)) > 1) {

    if (genotypeEffectType == 'fixed') {
      adjMeans <- fixedCRD(traitData, traitName)
    } else {
      adjMeans <- randomCRD(traitData, traitName)
    }

  } else if (experimentalDesign == 'Alpha') {

    if (genotypeEffectType == 'fixed') {
      adjMeans <- fixedAlpha(traitData, traitName)
    } else {
      adjMeans <- randomAlpha(traitData, traitName)
    }

  } else {

    adjMeans <- averageMean(traitData, traitName)

  }
}

#' structures trait data
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

  colList <- c('germplasmName', 'locationName', 'studyYear', 'blockNumber', 'replicate', traitName)

  traitData <- trialData[, colList]

  if (class(traitData[, traitName]) != 'numeric') {
    traitData[, traitName] <- as.numeric(as.character(traitData[, traitName]))
  }

  return(traitData)
}

fixedAugmentedRCBD <- function (traitData, traitName) {

  adjMeans <- fixedRCBD(traitData, traitName)

  return(adjMeans)

}

randomAugmentedRCBD <- function (traitData, traitName) {

  adjMeans <- randomRCBD(traitData, traitName)

  return(adjMeans)

}


averageMean <- function(traitData, traitName) {
  message('averaging')

  traitData <- traitData[, c('germplasmName', traitName)]

  if (sum(is.na(traitData)) > 0) {
    traitData <- na.omit(traitData)
  }

  traitData   <- traitData[order(row.names(traitData)), ]
  traitData   <- data.frame(traitData)

  calMean <- paste0('mean(', traitName, ', na.rm=TRUE)')
  aveCol  <- traitName

  traitAverage <- traitData %>%
                         group_by(germplasmName) %>%
                         summarise_(.dots = setNames(calMean, aveCol))

  traitAverage <- data.frame(traitAverage)
  colnames(traitAverage)[1] <- 'genotypes'

  return(traitAverage)

}


fixedCRD <- function(traitData, traitName) {

  colnames(traitData)[1] <- "genotypes"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ genotypes + (1|replicate),
                    traitData,
                    na.action = na.omit))

  if (class(modelOut) != "try-error") {
    genoMeans <- structureGenoMeans(modelOut, traitName)
    return(genoMeans)
  } else {
    stop('Encountered fitting the model.')
  }

}

randomCRD <- function(traitData, traitName) {

  print('doing random effects')
  colnames(traitData)[1] <- "genotypes"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|genotypes) + (1|replicate),
                    traitData,
                    na.action = na.omit))

  if (class(modelOut) != "try-error") {
    genoMeans <- structureGenoMeans(modelOut, traitName)
    return(genoMeans)
  } else {
    stop('Encountered fitting the model.')
  }

}

fixedAlpha <- function(traitData, traitName) {

  colnames(traitData)[1] <- "genotypes"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ genotypes + (1|replicate/blockNumber),
                    traitData,
                    na.action = na.omit))

  if (class(modelOut) != "try-error") {
    genoMeans <- structureGenoMeans(modelOut, traitName)
    return(genoMeans)
  } else {
    stop('Encountered fitting the model.')
  }

}


randomAlpha <- function(traitData, traitName) {

  colnames(traitData)[1] <- "genotypes"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|genotypes) + (1|replicate/blockNumber),
                    traitData,
                    na.action = na.omit))

  if (class(modelOut) != "try-error") {
    genoMeans <- structureGenoMeans(modelOut, traitName)
    return(genoMeans)
  } else {
    stop('Encountered fitting the model.')
  }

}

fixedRCBD <- function (traitData, traitName) {

    colnames(traitData)[1] <- "genotypes"
    colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

    modelOut <- try(lmer(trait ~ genotypes + (1|blockNumber),
                      traitData,
                      na.action = na.omit))

    if (class(modelOut) != "try-error") {
      genoMeans <- structureGenoMeans(modelOut, traitName)
      return(genoMeans)
    } else {
      stop('Encountered fitting the model.')
    }

}


randomRCBD <- function (traitData, traitName) {

  colnames(traitData)[1] <- "genotypes"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|genotypes) + (1|blockNumber),
                    traitData,
                    na.action = na.omit))

  if (class(modelOut) != "try-error") {
    genoMeans <- structureGenoMeans(modelOut, traitName)
    return(genoMeans)
  } else {
    stop('Encountered fitting the model.')
  }

}


structureGenoMeans <- function(modelOut, traitName) {

  if (genotypeEffectType == 'fixed') {
    genolsmean     <- lsmeans(modelOut, 'genotypes')
    genolsmeanSumm <- summary(genolsmean)
    genoMeans      <- genolsmeanSumm[c('genotypes', 'lsmean')]

    genoMeans$lsmean    <- round(genoMeans$lsmean, 2)
    names(genoMeans)[2] <- traitName
    genoMeans <- data.frame(genoMeans)

    return(genoMeans)
  } else {

    genoBlups <- ranef(modelOut)$genotypes
    names(genoBlups) <- traitName
    genoBlups$genotypes <- rownames(genoBlups)
    genoBlups <- genoBlups[, c(2, 1)]

    sumFormula <- paste0(traitName, ' + ', fixef(modelOut))
    colMeansName  <- traitName
    genoMeans <- genoBlups %>% mutate_(.dots = setNames(sumFormula, colMeansName))

    roundMeans <- paste0('round(', traitName, ' ,2)')
    genoMeans  <- genoMeans %>% mutate_(.dots = setNames(roundMeans, colMeansName))
  }

}


