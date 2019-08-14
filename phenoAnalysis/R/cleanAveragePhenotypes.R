#' Reads, cleans and creates phenotype data with averaged individual data points for entries.
#'
#' Given a phenotype data file with the BrAPI style column naming and
#' a file with the metadata columns, reads phenotype data from one or more files,
#'  and then calculates averages for entries
#'
#' @param phenoFiles a character vector of phenotype file(s).
#' @param metaDataFile a file with the meta data columns, tab-delimited. Optional
#' @param keepMetaCols a vector of meta columns to keep in the data set.
#' @param ... additional args to pass to function combinePhenoData (from phenoAnalysis)
#' @return a data frame of the averaged phenotype dataset with trait columns, and meta columns exempt from removal.
#' @export
#'
#' @seealso \code{\link[phenoAnalysis]{combinePhenoData}},
#' \code{\link[phenoAnalysis]{extractPhenotypes}},
#' \code{\link[phenoAnalysis]{cleanMetaCols}},
#' \code{\link[phenoAnalysis]{summarizeTraits}}.


cleanAveragePhenotypes <- function(inputFiles=NULL,
                                   metaDataFile=NULL,
                                   keepMetaCols=c('germplasmName'),
                                   ...) {
#metaFile <- grep("meta", inputFiles,  value = TRUE)
  if (is.null(inputFiles))
      stop('Phenotype data file is missing.')

  if (is.null(metaDataFile))
      stop('Phenotype meta data file is missing.')

  pheno <-  extractPhenotype(inputFiles, metaDataFile)

  phenoData <- cleanMetaCols(metaDataFile=metaDataFile,
                           phenoData=pheno,
                           keepMetaCols=keepMetaCols)

  phenoData <- summarizeTraits(phenoData)

#phenoNa   <- phenoData %>% filter_all(any_vars(is.na(.)))
  phenoData <- column_to_rownames(phenoData, 'germplasmName')
  phenoData <- round(phenoData, 2)
  return (phenoData)

}
