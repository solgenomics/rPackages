#' calculates the genotype adjusted means for a trait.
#'
#' calculates the genotype adjusted means for a trait.
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention. This argument is required if the modelOut argument is null.
#' @param modelOut a lmerModLmerTest or merModLmerTest class anova model output. This argument is required if the trialData argument is null.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, germplasmName variable is considered fixed effects.
#' @param adjMeansVariable name of the variable adjusted means to calculate for, the default is germplasmName.
#' @param calcAverages logical. Defaults to FALSE. If TRUE, returns the trait averages, if modeling fails and can't calculate BLUEs or BLUPs.
#' @param logReturn logical indicating whether to return a log of the adjusted means calculation. Default is FALSE.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

getAdjMeans <- function (trialData=NULL,
                         traitName=NULL,
                         modelOut=NULL,
                         genotypeEffectType='fixed',
                         adjMeansVariable='germplasmName',
                         calcAverages=FALSE,
                         logReturn=FALSE) {

  if (is.null(traitName)) stop('Trait name is missing.')

  if (is.null(trialData) & is.null(modelOut)) {
    stop('You need to provide either a trial data or a model object (from lmer).')
  }


  log <- c()
  if (!is.null(trialData) & is.null(modelOut)) {
      anovaOut <- runAnova(trialData,
                           traitName=traitName,
                           genotypeEffectType=genotypeEffectType,
                           logReturn=logReturn)
      if (logReturn) {
        modelOut <- anovaOut$modelOut
        log <- anovaOut$log
      } else {
        modelOut <- anovaOut
      }
  }

  adjMeans <- c()
  if (class(modelOut)[1] == 'lmerModLmerTest' || class(modelOut)[1] == 'merModLmerTest') {
    adjMeans <- structureAdjMeans(modelOut,
                                  traitName,
                                  genotypeEffectType=genotypeEffectType,
                                  adjMeansVariable=adjMeansVariable)

  } else if (calcAverages) {

    if (is.null(trialData)){
      stop('You need to provide the trial data argument to calculate averages.')
    }

    adjMeans <- averageTrait(trialData,
                             traitName,
                             meansVariable=adjMeansVariable)

    trialName <- trialData[2, 'studyName']
    log <- paste0("Trial ", trialName, " does not have a proper experimental design. Therefore ANOVA was not run on the trial to calculate the trait adjusted means.", "\n")
    log <- append(log, paste0("The ", traitName, " values for the trial are arithmetic averages.", "\n\n"))

  }

  if (logReturn) {
    return (list("adjMeans" = adjMeans, "log" = log))
  } else {

    return(adjMeans)
  }

}



