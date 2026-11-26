rcbd <- function(scale=1) {
  d <- expand.grid(germplasmName=paste0('CP-', 1:8), blockNumber=factor(1:4), stringsAsFactors=TRUE)
  d$trait <- (10 + as.numeric(d$germplasmName) * 0.6 + c(0, 1.5, -1, 2)[d$blockNumber] +
              0.8 * sin(seq_len(nrow(d)))) * scale
  d
}

fit <- function(d) lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d)

test_that("the ANOVA table shows a small p value, not 0", {
  m <- fit(rcbd())
  p <- stats::anova(m)[['Pr(>F)']]
  expect_lt(p, 1e-4)

  for (tableType in c('text', 'html')) {
    table <- paste(getAnovaTable(m, tableType=tableType, traitName='trait'), collapse='\n')
    expect_match(table, format.pval(p, digits=3), fixed=TRUE)
    expect_no_match(table, '0.0000', fixed=TRUE)
  }
})

test_that("a trait with small values keeps them in the ANOVA table", {
  m <- fit(rcbd(scale=1/1000))
  a <- stats::anova(m)

  extensions <- c(text='.txt', html='.html')
  for (tableType in names(extensions)) {
    path <- tempfile(fileext=extensions[[tableType]])
    table <- tryCatch({
      getAnovaTable(m, tableType=tableType, out=path)
      paste(readLines(path), collapse='\n')
    }, finally=unlink(path))

    expect_match(table, format(a[['Sum Sq']], digits=4), fixed=TRUE)
    expect_match(table, format(a[['Mean Sq']], digits=4), fixed=TRUE)
  }
})

exactMeans <- function(m) as.data.frame(summary(lsmeans::lsmeans(m, 'germplasmName')))$lsmean

test_that("close adjusted means stay distinct in the numeric result", {
  d <- rcbd(scale=1/1000)
  target <- c('CP-1'=0.012345671, 'CP-2'=0.012345674)
  for (entry in names(target)) {
    plots <- d$germplasmName == entry
    d$trait[plots] <- d$trait[plots] - mean(d$trait[plots]) + target[[entry]]
  }
  m <- fit(d)
  exact <- exactMeans(m)
  means <- getAdjMeans(modelOut=m, traitName='trait')

  expect_equal(signif(exact[1], 6), signif(exact[2], 6))
  expect_lt(means[[2]][1], means[[2]][2])
  expect_identical(means[[2]], exact)
})

test_that("large adjusted means also keep their full numeric precision", {
  m <- fit(rcbd(scale=1000))
  means <- structureAdjMeans(m, 'trait')

  expect_true(all(exactMeans(m) >= 1e4))
  expect_identical(means[[2]], exactMeans(m))
})
