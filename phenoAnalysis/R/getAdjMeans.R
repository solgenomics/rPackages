#' calculates the genotype adjusted means for a trait.
#'
#' calculates the genotype adjusted means for a trait.
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention. This argument is required if the modelOut argument is null.
#' @param modelOut a lmerModLmerTest or merModLmerTest class anova model output. This argument is required if the trialData argument is null.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, genotypes variable is considered fixed effects.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

getAdjMeans <- function (trialData=NULL, modelOut=NULL, traitName=NULL, genotypeEffectType='fixed') {

  if (is.null(traitName)) stop('Trait name is missing.')

  if (is.null(trialData) & is.null(modelOut)) {
    stop('You need to provide either trial data or model output.')
  }

  if (!is.null(modelOut)) {
    if (class(modelOut)[1] != 'lmerModLmerTest' & class(modelOut)[1] != 'merModLmerTest') {
      stop('The modelOut argument is not a class type of lmerModLmerTest or merModLmerTest.')
    }
  }

  if (!is.null(trialData) & is.null(modelOut)) {
    tr <- grep(traitName, colnames(trialData))

    if (length(tr) == 0) {
       stop('Can not find ', traitName, ' in the data set.')
      }

     #genotypeEffectType <<- genotypeEffectType
      traitData          <- structureTraitData(trialData, traitName=traitName)
      adjMeans           <- c()

    experimentalDesign <- trialData[2, 'studyDesign']
    if (is.na(experimentalDesign) == TRUE) experimentalDesign <- c('No Design')

    if (experimentalDesign == 'RCBD'
        &&  length(unique(traitData$blockNumber)) > 1) {

        if (genotypeEffectType == 'fixed') {
          modelOut <- fixedRCBD(traitData, traitName)
        } else {
          modelOut <- randomRCBD(traitData, traitName)
        }

    } else if (experimentalDesign == 'Augmented'
             &&  length(unique(traitData$blockNumber)) > 1) {

      if (genotypeEffectType == 'fixed') {
          modelOut <- fixedAugmentedRCBD(traitData, traitName)
      } else {
          modelOut <- randomAugmentedRCBD(traitData, traitName)
      }

    }  else if ((experimentalDesign == 'CRD')
              &&  length(unique(traitData$replicate)) > 1) {

      if (genotypeEffectType == 'fixed') {
          modelOut <- fixedCRD(traitData, traitName)
      } else {
          modelOut <- randomCRD(traitData, traitName)
      }

    } else if (experimentalDesign == 'Alpha') {

      if (genotypeEffectType == 'fixed') {
         modelOut <- fixedAlpha(traitData, traitName)
      } else {
        modelOut <- randomAlpha(traitData, traitName)
     }

    }
  }

  if (class(modelOut)[1] == 'lmerModLmerTest' || class(modelOut)[1] == 'merModLmerTest') {
    adjMeans <- structureAdjMeans(modelOut, traitName)
  } else {
    adjMeans <- averageTrait(traitData, traitName)
  }

  return (adjMeans)
}



