#' calculates the genotype adjusted means for a trait.
#'
#' calculates the genotype adjusted means for a trait.
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention. This argument is required if the modelOut argument is null.
#' @param modelOut a lmerModLmerTest or merModLmerTest class anova model output. This argument is required if the trialData argument is null.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, germplasmName variable is considered fixed effects.
#' @param adjMeansVariable name of the variable adjusted means to calculate for, the default is germplasmName.
#' @param calcAverages logical. Defaults to FALSE. If TRUE, returns the trait averages, if modeling fails and can't calculate BLUEs or BLUPs.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

getAdjMeans <- function (trialData=NULL,
                         traitName=NULL,
                         modelOut=NULL,
                         genotypeEffectType='fixed',
                         adjMeansVariable='germplasmName',
                         calcAverages=FALSE) {

  if (is.null(traitName)) stop('Trait name is missing.')

  if (is.null(trialData) & is.null(modelOut)) {
    stop('You need to provide either a trial data or a model (from lmer).')
  }

  if (!is.null(trialData) & is.null(modelOut)) {
      modelOut <- runAnova(trialData,
                           traitName=traitName,
                           genotypeEffectType=genotypeEffectType)
  }

  if (class(modelOut)[1] == 'lmerModLmerTest' || class(modelOut)[1] == 'merModLmerTest') {
    adjMeans <- structureAdjMeans(modelOut,
                                  traitName,
                                  genotypeEffectType=genotypeEffectType,
                                  adjMeansVariable=adjMeansVariable)

    return (adjMeans)
  } else if (calcAverages && !is.null(trialData)) {
    adjMeans <- averageTrait(trialData,
                             traitName,
                             meansVariable=adjMeansVariable)

    return (adjMeans)
  } else if (calcAverages && is.null(trialData)) {
    return ('ERROR: You need to provide the trial data argument to calculate averages.')
  } else {
    return (modelOut)
  }

}



