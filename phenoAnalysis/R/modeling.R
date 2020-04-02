#' fits linear mixed model
#'
#' fits linear mixed model depending on the type of experimental design
#' as is found in the dataset and specification of the genotype effect type,
#' defaults to fixed effects.
#'
#' @param traitData a data frame of the trait data
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @return the model output, an object of class \code{\link[lmerTest]{lmer}}.

#' @name models


#' @export
#' @rdname models
fixedAugmentedRCBD <- function (traitData, traitName) {

  modelOut <- fixedRCBD(traitData, traitName)

  return(modelOut)

}

#' @export
#' @rdname models
randomAugmentedRCBD <- function (traitData, traitName) {

  modelOut <- randomRCBD(traitData, traitName)

  return(modelOut)

}

#' @export
#' @rdname models
fixedCRD <- function(traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ germplasmName + (1|replicate),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}


#' @export
#' @rdname models
randomCRD <- function(traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|germplasmName) + (1|replicate),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}


#' @export
#' @rdname models
fixedAlpha <- function(traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ germplasmName + (1|replicate/blockNumber),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}


#' @export
#' @rdname models
randomAlpha <- function(traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|germplasmName) + (1|replicate/blockNumber),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}


#' @export
#' @rdname models
fixedRCBD <- function (traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ germplasmName + (1|blockNumber),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}


#' @export
#' @rdname models
randomRCBD <- function (traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|germplasmName) + (1|blockNumber),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}


#' @export
#' @rdname models
fixedRowCol <- function(traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ germplasmName + (1|rowNumber) + (1|colNumber),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}
#' @export
#' @rdname models
randomRowCol <- function(traitData, traitName) {

  colnames(traitData)[1] <- "germplasmName"
  colnames(traitData)[which(names(traitData)==traitName)] <- "trait"

  modelOut <- try(lmer(trait ~ (1|germplasmName) + (1|rowNumber) + (1|colNumber),
                       traitData,
                       na.action = na.omit))

  if (class(modelOut) != "try-error") {
    return(modelOut)
  } else {
    return(paste0('Encountered error fitting the model: ', modelOut))
  }

}

