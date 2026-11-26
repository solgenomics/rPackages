#' calculates the genotype adjusted means for a trait.
#'
#'calculates the genotype adjusted means for a trait.
#' @param modelOut a lmer model output.
#' @param traitName a character vector or string of the trait name of interest in the dataset.
#' @param adjMeansVariable name of the variable adjusted means to calculate for, the default is germplasmName.
#' @return a dataframe of the adjusted means for the genotypes. The fixed effects means have
#'   6 significant digits and at least 2 decimals, so the means of a trait with small values
#'   do not become equal.
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

    # 6 significant digits, and at least 2 decimals as before
    means <- genoMeans$lsmean
    large <- !is.na(means) & abs(means) >= 1e4
    genoMeans$lsmean        <- signif(means, 6)
    genoMeans$lsmean[large] <- round(means[large], 2)
    names(genoMeans)[2] <- paste0(traitName, '_Adjusted_means_fixed_effects')

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
    names(adjMeans)[2] <- paste0(traitName, '_Adjusted_means_random_effects')

     return(adjMeans)
  }

}
