#' compares the means of the levels of a factor with Fisher's least significant difference (LSD)
#'
#' Takes a fitted model, so that ANOVA, mixed models and other tools can use it.
#' The means are the estimated marginal means (adjusted means) of the factor.
#' Fisher's protected LSD first tests whether the reported means are jointly
#' equal. If the F test is significant at alpha, a pair differs when its
#' absolute difference exceeds its LSD, t(1 - alpha/2, df) times the standard
#' error of the difference (SED). The SED comes from the covariance matrix of
#' the means and can vary by pair in an unbalanced trial. The reported LSD
#' summaries describe the thresholds across pairs. Use the letters for pairwise
#' decisions when they are shown.
#' With a nonsignificant F test, all levels share the letter "a" when letters
#' are shown. Above maxGroupLevels, letters are omitted for either test result.
#'
#' @param modelOut a fitted model with the factor as a fixed effect: an lmer model
#'   (lme4 or lmerTest) or an lm model.
#' @param factor the name of the factor whose levels to compare. Default is germplasmName.
#' @param alpha the significance level. Default is 0.05.
#' @param df the degrees of freedom of the t quantile. By default the denominator
#'   degrees of freedom of the joint F test: Satterthwaite's for an lmer model,
#'   the residual degrees of freedom for an lm model.
#' @param maxGroupLevels the largest number of levels that get letter groups. Default is 100.
#' @return a list with:
#'   \code{means}, a data frame of the levels sorted by mean, largest first, with the
#'   columns of the level, mean, SE (standard error of the mean) and group (letters);
#'   \code{lsd} and \code{sed}, the mean, smallest and largest LSD and SED of all pairs;
#'   \code{df}, \code{alpha}, \code{pValue} (the joint F test) and
#'   \code{significant} (TRUE when pValue < alpha) and \code{note}, a message
#'   when the F test is nonsignificant or letters are omitted, NULL otherwise.
#'   The values are not rounded.
#' @export
#'

compareMeans <- function(modelOut, factor='germplasmName', alpha=0.05, df=NULL, maxGroupLevels=100) {

  if (is.null(modelOut)) stop('The model is missing.')
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop('alpha must be a number between 0 and 1.')
  }

  mixed <- inherits(modelOut, 'lmerMod')
  if (!mixed && (!inherits(modelOut, 'lm') || inherits(modelOut, 'glm'))) {
    stop('compareMeans supports lmer and lm models, not ', class(modelOut)[1], '.')
  }
  if (!mixed && stats::df.residual(modelOut) <= 0) {
    stop('The F test of ', factor, ' has no degrees of freedom left, so the means are not compared.')
  }
  if (!factor %in% all.vars(stats::delete.response(stats::terms(modelOut)))) {
    stop(factor, ' is not a fixed effect of the model.')
  }

  # The covariance matrix of the model; the degrees of freedom are set below.
  emm  <- suppressMessages(emmeans::emmeans(modelOut, specs=factor, lmer.df='asymptotic'))
  summ <- as.data.frame(summary(emm))
  means <- summ$emmean
  names(means) <- as.character(summ[[factor]])

  estimable <- !is.na(means)
  if (sum(estimable) < 2) stop('At least two levels of ', factor, ' with a mean are needed to compare means.')

  emm <- emm[which(estimable)]
  fTest <- meansFTest(emm, modelOut, mixed)
  if (is.null(df)) df <- fTest$df
  if (!is.finite(df) || df <= 0 || is.na(fTest$pValue)) {
    stop('The F test of ', factor, ' has no degrees of freedom left, so the means are not compared.')
  }

  covMatrix <- stats::vcov(emm)
  means     <- means[estimable]
  variances <- diag(covMatrix)
  sed <- sqrt(pmax(outer(variances, variances, '+') - 2 * covMatrix, 0))
  lsd <- stats::qt(1 - alpha / 2, df) * sed

  pairs <- upper.tri(sed)
  differ <- abs(outer(means, means, '-')) > lsd
  dimnames(differ) <- list(names(means), names(means))

  ordered <- order(means, decreasing=TRUE)
  significant <- fTest$pValue < alpha
  note <- NULL
  if (!significant) {
    note <- paste0('The F test of ', factor, ' is not significant at ', alpha,
                   ', so protected LSD declares no pair different.')
  }
  if (length(means) > maxGroupLevels) {
    groups <- stats::setNames(rep(NA_character_, length(means)), names(means))
    limitNote <- paste0('Letter groups are given for up to ', maxGroupLevels, ' levels. This trial has ',
                        length(means), '.')
    note <- paste(c(note, limitNote), collapse=' ')
  } else if (significant) {
    groups <- multcompView::multcompLetters(differ[ordered, ordered])$Letters
  } else {
    groups <- stats::setNames(rep('a', length(means)), names(means))
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

# Consecutive contrasts jointly test equality of the reported means.
meansFTest <- function(emm, modelOut, mixed) {

  contrasts <- emmeans::contrast(emm, 'consec')
  if (mixed) {
    if (!inherits(modelOut, 'lmerModLmerTest')) {
      modelOut <- lmerTest::as_lmerModLmerTest(modelOut)
    }
    # emmeans restores aliased columns to its basis, but lmerTest expects
    # only the columns retained by the fitted model.
    L <- contrasts@linfct
    dropped <- attr(stats::model.matrix(modelOut), 'col.dropped')
    if (length(dropped)) L <- L[, -dropped, drop=FALSE]
    test <- lmerTest::contestMD(modelOut, L, ddf='Satterthwaite')
    return(list(pValue=test[['Pr(>F)']][1], df=test$DenDF[1]))
  }
  test <- as.data.frame(emmeans::test(contrasts, joint=TRUE))
  list(pValue=test$p.value[1], df=test$df2[1])
}
