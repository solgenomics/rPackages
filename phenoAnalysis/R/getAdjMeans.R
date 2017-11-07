#' calculates the genotype adjusted means for a trait.
#'
#' calculates the genotype adjusted means for a trait.
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, genotypes variable is considered fixed effects.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

getAdjMeans <- function (trialData, traitName=NULL, genotypeEffectType='fixed') {

  if (is.null(traitName)) stop('Trait name is missing.')

  tr <- grep(traitName, colnames(trialData))
  if (length(tr) == 0) {
    stop('Can not find ', traitName, ' in the data set.')
  }

  genotypeEffectType <<- genotypeEffectType
  traitData          <- structureTraitData(trialData, traitName=traitName)
  adjMeans           <- c()


  genotypeEffectType <<- genotypeEffectType
  traitData          <- structureTraitData(trialData, traitName=traitName)
  anovaTable         <- c()

  experimentalDesign <- trialData[2, 'studyDesign']
  if (is.na(experimentalDesign) == TRUE) experimentalDesign <- c('No Design')

  if (experimentalDesign == 'RCBD' &&  length(unique(traitData$blockNumber)) > 1) {

     if (genotypeEffectType == 'fixed') {
    modelOut <- fixedRCBD(traitData, traitName)
    adjMeans <- structureGenoMeans(modelOut, traitName)
  } else {
    modelOut <- randomRCBD(traitData, traitName)
    adjMeans <- structureGenoMeans(modelOut, traitName)
    }
  } else if (experimentalDesign == 'Augmented'
             &&  length(unique(traitData$blockNumber)) > 1) {

    if (genotypeEffectType == 'fixed') {
      modelOut <- fixedAugmentedRCBD(traitData, traitName)
      adjMeans <- structureGenoMeans(modelOut, traitName)
    } else {
      modelOut <- randomAugmentedRCBD(traitData, traitName)
      adjMeans <- structureGenoMeans(modelOut, traitName)
    }

  }  else if ((experimentalDesign == 'CRD')
              &&  length(unique(traitData$replicate)) > 1) {

    if (genotypeEffectType == 'fixed') {
      modelOut <- fixedCRD(traitData, traitName)
      adjMeans <- structureGenoMeans(modelOut, traitName)
    } else {
      modelOut <- randomCRD(traitData, traitName)
      adjMeans <- structureGenoMeans(modelOut, traitName)
    }

  } else if (experimentalDesign == 'Alpha') {

    if (genotypeEffectType == 'fixed') {
      modelOut <- fixedAlpha(traitData, traitName)
      adjMeans <- structureGenoMeans(modelOut, traitName)
    } else {
      modelOut <- randomAlpha(traitData, traitName)
      adjMeans <- structureGenoMeans(modelOut, traitName)
    }
  } else {

    adjMeans <- averageTrait(traitData, traitName)

  }
}



