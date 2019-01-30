#' Removes meta data columns from the phenotype data
#'
#' Given a dataset with the BrAPI style column naming, removes meta data columns from the phenotype data.
#'
#' @param metaData a vector of columns to remove.
#' @param metaDataFile a file with the meta data columns, tab-delimited. Optional
#' @param keepMetaCols a vector of meta columns to keep in the data set.
#' @param phenoData a data frame of the phenotype dataset
#' @param phenoFile a name of the file with the phenotype dataset
#' @return a data frame of the phenotype dataset with trait columns, and meta columns exempt from removal.
#' @export
#'
cleanMetaCols <- function(metaDataFile=NULL,
                          phenoData=NULL,
                          phenoFile=NULL,
                          metaData=NULL,
                          keepMetaCols=NULL) {

  if (is.null(metaData) & is.null(metaDataFile)) {
  stop('Got no meta columns to remove from the data set. ')
  }

  if (is.null(phenoData) & is.null(phenoFile)) {
    stop('Got no phenotype dataset or file to read the phenotype data from. ')
  }

  if (is.null(phenoData) & !is.null(phenoFile)) {
    phenoData <- data.frame(fread(phenoFile,
                                  sep="\t",
                                  na.strings = c("NA", " ", "--", "-", ".", "..")))
  }

  if(!is.null(metaDataFile)) {
    metaData <- scan(metaDataFile, what="character")
  }

  allCols    <- names(phenoData)
  traitNames <- allCols[! allCols %in% metaData]
  selectCols <- c(keepMetaCols, traitNames)
  phenoData  <- phenoData %>% select_(.dots=selectCols)

}
