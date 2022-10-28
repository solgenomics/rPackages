#' filters genotype dataset
#'
#' A function for filtering genotype dataset based on minimum allele frequency, fraction of markers and accessions with missing values. It also filters out monomorphic markers.
#'
#' @param gData A data.frame or data.table of the genotype dataset
#' @param maf Minimum allele frequency (MAF) used to filter out a marker, 0 to 1. By default, no MAF filtering done.
#' @param markerFilter Fraction of missing values in a marker, 0 to 1. By default, markers with > 60 percent missing values are removed.
#' @param indFilter Fraction of missing values in an accession/genotype, 0 to 1. By default, accessions with > 80 percent missing marker values are removed.
#' @logRetrun logical indicating whether to return a log of the data filtering process. Default is FALSE. 
#' @return A data.frame or data.table of the cleaned genotype dataset if the logReturn argument is set to FALSE; If TRUE, the function returns a list of the cleaned data and the log.
#'
#' @export
#'

filterGenoData <- function (gData=genoDf, maf=0, markerFilter=0.6, indFilter=0.8, logReturn=FALSE) {

  ifelse (missing(gData)==T, stop('You need to provide a genotype data.frame argument'),
  ifelse (is.null(gData)==T, stop('Genotype dataset is null.'),
  ifelse (length(grep( "data.frame", class(gData))) < 1, stop('The genotype dataset is not a data.frame or data.table'), '')))

  origDType <- class(gData)[1]

  if (origDType == 'data.frame') {
    gData <- data.table(gData, keep.rownames = TRUE)
  }

  #remove markers with missing values
  
  log <- c("No of markers before applying any filter: ", length(names(gData)), "\n")

  gData[, which(colSums(is.na(gData)) >= nrow(gData) * markerFilter) := NULL]
log <- append(log, "No. of markers (columns) remaining after filtering out individuals (rows) missing greater than or equal to " , markerFilter * 100, '%: ',  length(names(gData)), "\n")

  #remove indls with missing values
 log <- append(log, "No. of individuals (rows) before applying any filter: ", length(rownames(gData)), "\n")
  gData[, noMissing := apply(.SD, 1, function(x) sum(is.na(x)))]
  gData <- gData[noMissing <= ncol(gData) * indFilter]
  gData[, noMissing := NULL]
  
  log <- append(log, "No. of individuals (rows) remaining after filtering out markers (columns) missing greater than or equal to ", indFilter * 100, '%: ',  length(rownames(gData)), "\n")

  #remove monomorphic markers
  log <- append(log, "No. of markers remaining before applying monomorphic markers filter: ", length(names(gData)), "\n")
  gData[, which(apply(gData, 2,  function(x) length(unique(x))) < 2) := NULL ]
  
  log <- append(log, "No. of marker after filtering out monomorphic markers: ", length(names(gData)), "\n")

  #remove markers with MAF
log <- append( log, "No of markers before applying MAF filter: ", length(names(gData)), "\n")
  gData[, which(apply(gData, 2,  calculateMAF) < maf) := NULL ]
  
  log <- append(log, "No. of markers remaining after filtering out ,markers less than or equal to ", maf*100, '% MAF: ', length(names(gData)), "\n")

  if (origDType == 'data.frame') {
    gData <- data.frame(gData)
    gData <- column_to_rownames(gData, 'rn')
  }

  if (logReturn) {
    return list("data" <- gData, "log" <- log)
  } else {
    return(gData)
  }

}
