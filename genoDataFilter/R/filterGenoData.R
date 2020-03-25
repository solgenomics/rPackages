#' filters genotype dataset
#'
#' A function for filtering genotype dataset based on minimum allele frequency, fraction of markers and accessions with missing values. It also filters out monomorphic markers.
#'
#' @param gData A data.frame or data.table of the genotype dataset
#' @param maf Minimum allele frequency (MAF) used to filter out a marker, 0 to 1. By default, no MAF filtering done.
#' @param markerFilter Fraction of missing values in a marker, 0 to 1. By default, markers with > 60 percent missing values are removed.
#' @param indFilter Fraction of missing values in an accession/genotype, 0 to 1. By default, accessions with > 80 percent missing marker values are removed.
#' @return A data.frame or data.table of the cleaned genotype dataset.
#'
#' @export
#'

filterGenoData <- function (gData=genoDf, maf=0, markerFilter=0.6, indFilter=0.8) {

  ifelse (missing(gData)==T, stop('You need to provide a genotype data.frame argument'),
  ifelse (is.null(gData)==T, stop('Genotype dataset is null.'),
  ifelse (length(grep( "data.frame", class(gData))) < 1, stop('The genotype dataset is not a data.frame or data.table'), '')))

  origDType <- class(gData)[1]

  if (origDType == 'data.frame') {
    gData <- data.table(gData, keep.rownames = TRUE)
  }

  #remove markers with missing values
  message('No of markers before applying any filter: ', ncol(gData))
  gData[, which(colSums(is.na(gData)) >= nrow(gData) * markerFilter) := NULL]
  message('No. of markers after filtering out  >', markerFilter, ' missing: ',  ncol(gData))

  #remove indls with missing values
  message('No. of indls before applying any filter: ', nrow(gData))
  gData[, noMissing := apply(.SD, 1, function(x) sum(is.na(x)))]
  gData <- gData[noMissing <= ncol(gData) * indFilter]
  gData[, noMissing := NULL]
  message('No. of indls after filtering out >', indFilter, ' missing: ',  nrow(gData))

  #remove monomorphic markers
  message('No. of marker before applying monomorphic markers filter: ', ncol(gData))
  gData[, which(apply(gData, 2,  function(x) length(unique(x))) < 2) := NULL ]
  message('No. of marker after filtering out monomorphic markers: ', ncol(gData))

  #remove markers with MAF
  message('No of markers before applying MAF filter: ', ncol(gData))
  gData[, which(apply(gData, 2,  calculateMAF) < maf) := NULL ]
  message('No. of marker after filtering out < ', maf, ' MAF: ', ncol(gData))

  if (origDType == 'data.frame') {
    gData <- data.frame(gData)
    gData <- column_to_rownames(gData, 'rn')
  }

  return(gData)
}
