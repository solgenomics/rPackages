#' filters genotype dataset
#'
#' A function for filtering genotype dataset based on minimum allele frequency, fraction of markers and accessions with missing values. It also filters out monomorphic markers.
#'
#' @param gData A data.frame or data.table of the genotype dataset
#' @param maf Minimum allele frequency (MAF) used to filter out a marker, 0 to 1. By default, markers with MAF < 5 percent are removed.
#' @param markerFilter Fraction of missing values in a marker, 0 to 1. By default, markers with > 60 percent missing values are removed.
#' @param indFilter Fraction of missing values in an accession/genotype, 0 to 1. By default, accessions with > 80 percent missing marker values are removed.
#' @return A data.frame of the cleaned genotype dataset.
#'
#' @import data.table
#' @export
#'

filterGenoData <- function (gData=genoDf, maf=0.05, markerFilter=0.6, indFilter=0.8) {

  ifelse (missing(gData)==T, stop('You need to provide a genotype data.frame argument'),
  ifelse (is.null(gData)==T, stop('Genotype dataset is null.'),
  ifelse (grep(class(gData), "data.frame", value=T), stop('The genotype dataset is not a data.frame or data.table'), '')))

  if (class(gData)[1] == 'data.frame') {
    gData <- data.table(gData)
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

  gData           <- data.frame(gData)
  rownames(gData) <- gData[, 1]
  gData[, 1]      <- NULL

  return(gData)
}

#' calculates minimum allele frequency (MAF)
#'
#' A function for calculating minimum allele frequency. Genotype coding needs to be [0, 1, 2].
#'
#' @param x A vector of marker allele data
#
#' @return minimum allele frequency
#'
#' @export
#'

calculateMAF <- function (x) {

    ifelse (missing(x) == T, stop('You need to provide an argument.'),
    ifelse (is.null(x) == T, stop('You need to provide a vector with alllele data.'), ''))

    x <- na.omit(x)

    a0 <-  length(x[x==0])
    a1 <-  length(x[x==1])
    a2 <-  length(x[x==2])
    aT <- a0 + a1 + a2

    p <- ((2*a0)+a1)/(2*aT)
    q <- 1- p

    maf <- min(p, q)

    return(maf)
}
