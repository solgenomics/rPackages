#' fits model and returns the model output summary
#'
#' fits model and returns the model output summary
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, genotypes variable is considered fixed effects.
#' @return a summary of the model output.
#' @export
#'

runAnova <- function (trialData, traitName=NULL, genotypeEffectType='fixed') {

  if (is.null(traitName)) stop('Trait name is missing.')

  tr <- grep(traitName, colnames(trialData))
  if (length(tr) == 0) {
    stop('Can not find ', traitName, ' in the data set.')
  }

  genotypeEffectType <<- genotypeEffectType
  traitData          <- structureTraitData(trialData, traitName=traitName)
  modelOut           <- c()

  studyDesign <- trialData[2, 'studyDesign']
  if (is.na(studyDesign) == TRUE) studyDesign <- c('No Design')

  if (studyDesign == 'RCBD'&&  length(unique(traitData$blockNumber)) > 1) {
     if (genotypeEffectType == 'fixed') {
      modelOut   <- fixedRCBD(traitData, traitName)
    } else {
      modelOut   <- randomRCBD(traitData, traitName)
    }
  } else if (studyDesign == 'Augmented' &&  length(unique(traitData$blockNumber)) > 1) {

    if (genotypeEffectType == 'fixed') {
      modelOut   <- fixedAugmentedRCBD(traitData, traitName)
    } else {
      modelOut   <- randomAugmentedRCBD(traitData, traitName)
    }

  }  else if (studyDesign == 'CRD' &&  length(unique(traitData$replicate)) > 1) {

    if (genotypeEffectType == 'fixed') {
      modelOut   <- fixedCRD(traitData, traitName)
    } else {
      modelOut   <- randomCRD(traitData, traitName)
    }

  } else if (studyDesign == 'Alpha') {

    if (genotypeEffectType == 'fixed') {
      modelOut  <- fixedAlpha(traitData, traitName)
    } else {
      modelOut   <- randomAlpha(traitData, traitName)
    }

  } else {
    modelOut <- c("Can't perform ANOVA on this trait. Because, the trial has no proper or supported experimental design for the trait.")
  }

  return(modelOut)
}

