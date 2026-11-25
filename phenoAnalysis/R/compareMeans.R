#' compares the means of the levels of a factor with Fisher's least significant difference (LSD)
#'
#' Takes a fitted model, so that ANOVA, mixed models and other tools can use it.
#' The means are the estimated marginal means (adjusted means) of the factor.
#' Two means differ when their difference is larger than the LSD of the pair,
#' t(1 - alpha/2, df) times the standard error of the difference (SED). The SED
#' comes from the covariance matrix of the means, so each pair has its own LSD
#' in an unbalanced trial. The letter groups follow Fisher's protected LSD:
#' they are given only when the F test of the factor is significant at alpha;
#' otherwise all the levels share the letter "a". Letters are given for up to
#' maxGroupLevels levels: with more levels they are hard to read and slow to find.
#'
#' @param modelOut a fitted model with the factor as a fixed effect: an lmer model
#'   (lme4 or lmerTest) or an lm model.
#' @param factor the name of the factor whose levels to compare. Default is germplasmName.
#' @param alpha the significance level. Default is 0.05.
#' @param df the degrees of freedom of the t quantile. By default the denominator
#'   degrees of freedom of the F test of the factor: Satterthwaite's for an lmer model,
#'   the residual degrees of freedom for an lm model.
#' @param maxGroupLevels the largest number of levels that get letter groups. Default is 100.
#' @return a list with:
#'   \code{means}, a data frame of the levels sorted by mean, largest first, with the
#'   columns of the level, mean, SE (standard error of the mean) and group (letters);
#'   \code{lsd} and \code{sed}, the mean, smallest and largest LSD and SED of all pairs;
#'   \code{df}, \code{alpha}, \code{pValue} (the F test of the factor) and
#'   \code{significant} (TRUE when pValue < alpha) and \code{note}, why the letters
#'   are all "a" or missing, NULL otherwise. The values are not rounded.
#' @export
#'

compareMeans <- function(modelOut, factor='germplasmName', alpha=0.05, df=NULL, maxGroupLevels=100) {

  if (is.null(modelOut)) stop('The model is missing.')
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop('alpha must be a number between 0 and 1.')
  }

  if (inherits(modelOut, 'merMod')) {
    fTest <- lmerFTest(modelOut, factor)
  } else if (!inherits(modelOut, 'lm')) {
    stop('compareMeans supports lmer and lm models, not ', class(modelOut)[1], '.')
  } else if (stats::df.residual(modelOut) <= 0) {
    stop('The F test of ', factor, ' has no degrees of freedom left, so the means are not compared.')
  }

  # The covariance matrix of the model; the degrees of freedom are set below.
  emm  <- suppressMessages(emmeans::emmeans(modelOut, specs=factor, lmer.df='asymptotic'))
  summ <- as.data.frame(summary(emm))
  means <- summ$emmean
  names(means) <- as.character(summ[[factor]])

  estimable <- !is.na(means)
  if (sum(estimable) < 2) stop('At least two levels of ', factor, ' with a mean are needed to compare means.')

  if (inherits(modelOut, 'lm')) fTest <- meansFTest(emm[which(estimable)])
  if (is.null(df)) df <- fTest$df
  if (!is.finite(df) || df <= 0 || is.na(fTest$pValue)) {
    stop('The F test of ', factor, ' has no degrees of freedom left, so the means are not compared.')
  }

  covMatrix <- stats::vcov(emm)[estimable, estimable, drop=FALSE]
  means     <- means[estimable]
  variances <- diag(covMatrix)
  sed <- sqrt(pmax(outer(variances, variances, '+') - 2 * covMatrix, 0))
  lsd <- stats::qt(1 - alpha / 2, df) * sed

  pairs <- upper.tri(sed)
  differ <- abs(outer(means, means, '-')) > lsd
  dimnames(differ) <- list(names(means), names(means))

  ordered <- order(means, decreasing=TRUE)
  significant <- !is.na(fTest$pValue) && fTest$pValue < alpha
  note <- NULL
  if (length(means) > maxGroupLevels) {
    groups <- stats::setNames(rep(NA_character_, length(means)), names(means))
    note <- paste0('Letter groups are given for up to ', maxGroupLevels, ' levels; this trial has ',
                   length(means), '. Use the LSD.')
  } else if (significant) {
    groups <- multcompView::multcompLetters(differ[ordered, ordered])$Letters
  } else {
    groups <- stats::setNames(rep('a', length(means)), names(means))
    note <- paste0('The F test of ', factor, ' is not significant at ', alpha,
                   ', so no pair of means differs.')
  }

  meansTable <- data.frame(level=names(means)[ordered],
                           mean=unname(means[ordered]),
                           SE=summ$SE[estimable][ordered],
                           group=unname(groups[names(means)[ordered]]),
                           stringsAsFactors=FALSE)
  names(meansTable)[1] <- factor

  list(means=meansTable,
       lsd=c(mean=mean(lsd[pairs]), min=min(lsd[pairs]), max=max(lsd[pairs])),
       sed=c(mean=mean(sed[pairs]), min=min(sed[pairs]), max=max(sed[pairs])),
       df=df,
       alpha=alpha,
       pValue=fTest$pValue,
       significant=significant,
       note=note)
}

# The F test of the factor in an lmer model, adjusted for the other terms:
# type III with Satterthwaite's denominator degrees of freedom.
lmerFTest <- function(modelOut, factor) {

  if (!inherits(modelOut, 'lmerModLmerTest')) {
    modelOut <- lmerTest::as_lmerModLmerTest(modelOut)
  }
  anovaTable <- stats::anova(modelOut, type='III', ddf='Satterthwaite')
  row <- termRow(anovaTable, factor)

  list(pValue=anovaTable[row, 'Pr(>F)'], df=anovaTable[row, 'DenDF'])
}

# The F test of an lm model: the joint test that the compared means are equal,
# so the test and the comparison use the same means. anova() of an lm model is
# sequential, and drop1() leaves out a factor that is in an interaction.
meansFTest <- function(emm) {

  test <- as.data.frame(emmeans::test(emmeans::contrast(emm, 'consec'), joint=TRUE))

  list(pValue=test$p.value[1], df=test$df2[1])
}

# The row of the factor in an ANOVA table; a name with a space or a dash has
# backticks there.
termRow <- function(anovaTable, factor) {
  row <- match(factor, gsub('`', '', rownames(anovaTable), fixed=TRUE))
  if (is.na(row)) {
    stop(factor, ' is not a fixed effect of the model that can be tested on its own.')
  }
  row
}
