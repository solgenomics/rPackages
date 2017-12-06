#' fits model and generates anova table
#'
#' fits model and generates anova table
#' @param trialData a dataframe of the trial dataset, of the BrAPI structure and naming convention.
#' @param traitName a character vector of the trait name of interest in the dataset.
#' @param genotypeEffectType a character vector indicating the genotype effect type. By default, genotypes variable is considered fixed effects.
#' @param tableType a character vector indicating type of anova table output. Defaults to 'html' for web display and 'text' for saving in a file (pass the file name to the 'out' argument to save output in a file) .
#' @param ... arguments to be passed to \code{\link[stargazer]{stargazer}}.
#' @seealso see \code{\link[stargazer]{stargazer}} to modify the table output.
#' @return a stargazer formatted anova table of chosen type, html type by default.
#' @export
#'

getAnovaTable <- function (trialData, traitName = NULL, genotypeEffectType='fixed', ...) {

  if (is.null(traitName)) stop('Trait name is missing.')

  tr <- grep(traitName, colnames(trialData))
  if (length(tr) == 0) {
    stop('Can not find ', traitName, ' in the data set.')
  }

  genotypeEffectType <<- genotypeEffectType
  traitData          <- structureTraitData(trialData, traitName=traitName)
  anovaTable         <- c()
  modelOut           <- c()

  studyDesign <- trialData[2, 'studyDesign']
  if (is.na(studyDesign) == TRUE) studyDesign <- c('No Design')

  if (studyDesign == 'RCBD' && length(unique(traitData$blockNumber)) > 1) {
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
    modelOut <- c('This trial has no design.')
  }

  if (class(modelOut) == 'merModLmerTest') {
    anovaTable <- strAnovaTable(modelOut, ...)
  }

  return(anovaTable)
}

strAnovaTable <- function(modelOut, tableType=NULL, ...) {

  if (class(modelOut) == 'merModLmerTest') {
    anovaTable <- anova(modelOut)

    if (is.null(tableType)) {
      tableType <- 'html'
    }

    if (hasArg(out)) {
     stargazer(anovaTable,
              type= 'text',
              title="ANOVA Table",
              summary=FALSE,
              digits=2,
              header=FALSE,
              ...)
    }

    stargazer(anovaTable,
              type= tableType,
              title="ANOVA Table",
              summary=FALSE,
              digits=2,
              header=FALSE,
              ...)
    }
}
