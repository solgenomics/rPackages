#' calculates the genotype adjusted means for a trait.
#'
#'calculates the genotype adjusted means for a trait.
#' @param modelOut a lmer model output.
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @param adjMeansVariable name of the variable adjusted means to calculate for, the default is germplasmName.
#' @return a dataframe of the adjusted means for the genotypes.
#' @export
#'

structureAdjMeans <- function(modelOut,
                              traitName=NULL,
                              genotypeEffectType='fixed',
                              adjMeansVariable='germplasmName') {

  if (genotypeEffectType == 'fixed') {
    genolsmean     <- lsmeans(modelOut, adjMeansVariable)
    genolsmeanSumm <- summary(genolsmean)
    genoMeans      <- genolsmeanSumm[c(adjMeansVariable, 'lsmean')]

    genoMeans$lsmean    <- round(genoMeans$lsmean, 2)
    names(genoMeans)[2] <- traitName
    genoMeans <- data.frame(genoMeans)

    return(genoMeans)
  } else {

    blups <- lme4::ranef(modelOut)
    blups <- blups[[adjMeansVariable]]
    names(blups) <- traitName
    blups <- rownames_to_column(blups, adjMeansVariable)

    sumFormula <- paste0(traitName, " + ", lme4::fixef(modelOut))
    adjMeans <- blups %>% mutate_(.dots = setNames(sumFormula, traitName))
    roundMeans <- paste0('round(', traitName, ' ,5)')
    adjMeans  <- adjMeans %>% mutate_(.dots = setNames(roundMeans, traitName))

    return(adjMeans)
  }

}
