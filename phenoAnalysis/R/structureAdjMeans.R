#' calculates the genotype adjusted means for a trait.
#'
#'calculates the genotype adjusted means for a trait.
#' @param modelOut a lmer model output.
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

structureAdjMeans <- function(modelOut, traitName) {

  if (genotypeEffectType == 'fixed') {
    genolsmean     <- lsmeans(modelOut, 'genotypes')
    genolsmeanSumm <- summary(genolsmean)
    genoMeans      <- genolsmeanSumm[c('genotypes', 'lsmean')]

    genoMeans$lsmean    <- round(genoMeans$lsmean, 2)
    names(genoMeans)[2] <- traitName
    genoMeans <- data.frame(genoMeans)

    return(genoMeans)
  } else {

    genoBlups <- ranef(modelOut)$genotypes
    names(genoBlups) <- traitName
    genoBlups$genotypes <- rownames(genoBlups)
    genoBlups <- genoBlups[, c(2, 1)]

    sumFormula <- paste0(traitName, ' + ', fixef(modelOut))
    colMeansName  <- traitName
    genoMeans <- genoBlups %>% mutate_(.dots = setNames(sumFormula, colMeansName))

    roundMeans <- paste0('round(', traitName, ' ,2)')
    genoMeans  <- genoMeans %>% mutate_(.dots = setNames(roundMeans, colMeansName))

    return(genoMeans)
  }

}
