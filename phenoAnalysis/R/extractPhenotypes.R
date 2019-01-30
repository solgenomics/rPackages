#' Reads phenotype data from one or more files.
#'
#' Given a dataset with the BrAPI style column naming, reads phenotype data from one or more files..
#'
#' @param phenoFiles a character vector of phenotype file(s).
#' @param metaDataFile a file with the meta data columns, tab-delimited. Optional
#' @param ... additional args to pass to function combinePhenoData (from phenoAnalysis)
#' @return a data frame of the phenotype dataset with trait columns, and meta columns exempt from removal.
#' @export
#'
#' @seealso \code{\link[phenoAnalysis]{combinePhenoData}} which this function uses
extractPhenotype <- function(inputFiles, metaDataFile, ...) {

  phenoFiles <- grep("phenotype_data", inputFiles,  value = TRUE)

  phenoData <- c()
  if (length(phenoFiles) > 1 ) {
    phenoData <- combinePhenoData(phenoFiles,
                                  metaDataFile = metaDataFile,
                                  ...)

  } else {
    phenoFile <- phenoFiles
    phenoData <- fread(phenoFile,
                       sep= "\t",
                       na.strings = c("NA", " ", "--", "-", "."))

    phenoData <- data.frame(phenoData)

    if (is.null(phenoData)) {
      stop("There is no phenotype dataset.")
      q("no", 1, FALSE)
    }
  }

  return(phenoData)

}
