#' calculates minimum allele frequency (MAF)
#'
#' A function for calculating minimum allele frequency. Genotype coding needs to be [0, 1, 2].
#' @importFrom stats na.omit
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
