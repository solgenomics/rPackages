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

getAdjMeans <- function (trialData=NULL, traitName=NULL, modelOut=NULL, genotypeEffectType='fixed') {

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
      modelOut <- runAnova(trialData, traitName=traitName, genotypeEffectType=genotypeEffectType)
  }

  if (class(modelOut)[1] == 'lmerModLmerTest' || class(modelOut)[1] == 'merModLmerTest') {
    adjMeans <- structureAdjMeans(modelOut, traitName, genotypeEffectType=genotypeEffectType)
  } else {
    adjMeans <- averageTrait(trialData, traitName)
  }

  return (adjMeans)
}



