rcbd <- function(scale=1) {
  set.seed(42)
  d <- expand.grid(germplasmName=paste0('CP-', 1:8), blockNumber=factor(1:4), stringsAsFactors=TRUE)
  d$trait <- (10 + as.numeric(d$germplasmName) * 0.6 + c(0, 1.5, -1, 2)[d$blockNumber] +
              rnorm(nrow(d), sd=0.8)) * scale
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
  table <- paste(getAnovaTable(m, tableType='text'), collapse='\n')

  expect_match(table, format(stats::anova(m)[['Sum Sq']], digits=4), fixed=TRUE)
  expect_match(table, format(stats::anova(m)[['Mean Sq']], digits=4), fixed=TRUE)
})

exactMeans <- function(m) as.data.frame(summary(lsmeans::lsmeans(m, 'germplasmName')))$lsmean

test_that("the adjusted means of a trait with small values keep 6 significant digits", {
  m <- fit(rcbd(scale=1/1000))
  means <- structureAdjMeans(m, 'trait')

  expect_equal(means[[2]], signif(exactMeans(m), 6))
  expect_length(unique(means[[2]]), 8)
})

test_that("the adjusted means of a trait with large values keep 2 decimals", {
  m <- fit(rcbd(scale=1000))
  means <- structureAdjMeans(m, 'trait')

  expect_true(all(exactMeans(m) >= 1e4))
  expect_equal(means[[2]], round(exactMeans(m), 2))
})
