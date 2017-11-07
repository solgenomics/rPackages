#' fits model and generates anova table
#'
#' fits model and generates anova table
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, genotypes variable is considered fixed effects.
#' @param ... arguments to be passed to \code{\link[stargazer]{stargazer}}.
#' @seealso see \code{\link[stargazer]{stargazer}} to modify the table output.
#' @return a stargazer formatted anova table of chosen type, html type by default.
#' @export
#'

getAnovaTable <- function (trialData, traitName=NULL, genotypeEffectType='fixed', ...) {

  if (is.null(traitName)) stop('Trait name is missing.')

  tr <- grep(traitName, colnames(trialData))
  if (length(tr) == 0) {
    stop('Can not find ', traitName, ' in the data set.')
  }

  genotypeEffectType <<- genotypeEffectType
  traitData          <- structureTraitData(trialData, traitName=traitName)
  anovaTable         <- c()

  studyDesign <- trialData[2, 'studyDesign']
  if (is.na(studyDesign) == TRUE) studyDesign <- c('No Design')

  if (studyDesign == 'RCBD'&&  length(unique(traitData$blockNumber)) > 1) {
     if (genotypeEffectType == 'fixed') {
      modelOut   <- fixedRCBD(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    } else {
      modelOut   <- randomRCBD(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    }
  } else if (studyDesign == 'Augmented'
                &&  length(unique(traitData$blockNumber)) > 1) {

    if (genotypeEffectType == 'fixed') {
      modelOut   <- fixedAugmentedRCBD(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    } else {
      modelOut   <- randomAugmentedRCBD(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    }

  }  else if ((studyDesign == 'CRD')
             &&  length(unique(traitData$replicate)) > 1) {

    if (genotypeEffectType == 'fixed') {
      modelOut   <- fixedCRD(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    } else {
      modelOut   <- randomCRD(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    }

  } else if (studyDesign == 'Alpha') {

    if (genotypeEffectType == 'fixed') {
      modelOut  <- fixedAlpha(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    } else {
      modelOut   <- randomAlpha(traitData, traitName)
      anovaTable <- strAnovaTable(modelOut, ...)
    }

  } else {
    anovaTable <- c('This trial has no design.')
  }

  return(anovaTable)
}

strAnovaTable <- function(modelOut, ...) {

  anovaTable <- anova(modelOut)

  stargazer(anovaTable,
            type='text',
            title="ANOVA Table",
            summary=FALSE,
            digits=2,
            header=FALSE,
            ...)

}
