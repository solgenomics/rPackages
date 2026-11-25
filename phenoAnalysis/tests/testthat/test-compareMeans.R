rcbd <- function(seed=42, entries=8, blocks=4, effect=0.6) {
  set.seed(seed)
  d <- expand.grid(germplasmName=paste0('CP-', seq_len(entries)), blockNumber=factor(seq_len(blocks)),
                   stringsAsFactors=TRUE)
  blockEffect <- c(0, 1.5, -1, 2, 0.5, -0.5)[seq_len(blocks)]
  d$trait <- 10 + as.numeric(d$germplasmName) * effect + blockEffect[d$blockNumber] + rnorm(nrow(d), sd=0.8)
  d
}

test_that("a balanced RCBD gives the LSD and letter groups of agricolae", {
  skip_if_not_installed('agricolae')
  d <- rcbd()
  out <- compareMeans(lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d))
  ref <- agricolae::LSD.test(stats::aov(trait ~ germplasmName + blockNumber, data=d), 'germplasmName', alpha=0.05)

  expect_equal(unname(out$lsd['mean']), ref$statistics$LSD, tolerance=1e-6)
  expect_equal(out$df, ref$statistics$Df, tolerance=1e-6)
  expect_true(out$significant)
  expect_null(out$note)
  refGroups <- as.character(ref$groups$groups[match(out$means$germplasmName, rownames(ref$groups))])
  expect_identical(out$means$group, refGroups)
  expect_equal(out$means$mean, ref$groups$trait[match(out$means$germplasmName, rownames(ref$groups))])
  expect_false(is.unsorted(rev(out$means$mean)))
})

test_that("lm and lme4 models give the same comparison", {
  d <- rcbd()
  byLmerTest <- compareMeans(lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d))
  byLme4 <- compareMeans(lme4::lmer(trait ~ germplasmName + (1|blockNumber), data=d))
  byLm <- compareMeans(stats::lm(trait ~ germplasmName + blockNumber, data=d))

  expect_equal(byLme4$lsd, byLmerTest$lsd)
  expect_equal(byLm$lsd, byLmerTest$lsd, tolerance=1e-6)
  expect_identical(byLm$means$group, byLmerTest$means$group)
})

test_that("a missing plot gives each pair its own LSD", {
  d <- rcbd()
  d$trait[3] <- NA
  out <- compareMeans(lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d))

  expect_lt(out$lsd['min'], out$lsd['max'])
  expect_equal(nrow(out$means), 8)
  expect_false(anyNA(out$means$group))
})

test_that("without a significant F test all levels share one letter", {
  d <- rcbd(effect=0)
  out <- compareMeans(lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d))

  expect_false(out$significant)
  expect_true(all(out$means$group == 'a'))
  expect_match(out$note, 'not significant')
})

test_that("many levels get the LSD but no letters", {
  d <- rcbd(entries=12)
  out <- compareMeans(lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d), maxGroupLevels=10)

  expect_true(all(is.na(out$means$group)))
  expect_match(out$note, 'up to 10 levels')
  expect_gt(out$lsd['mean'], 0)
})

test_that("the means are not rounded", {
  d <- rcbd()
  d$trait <- d$trait / 1000
  out <- compareMeans(lmerTest::lmer(trait ~ germplasmName + (1|blockNumber), data=d))

  expect_true(all(out$means$mean != round(out$means$mean, 2)))
  expect_lt(out$lsd['mean'], 0.01)
})

test_that("the protection test of an lm model is adjusted for the blocks", {
  d <- data.frame(germplasmName=rep(c('A', 'A', 'B', 'B'), c(4, 2, 2, 4)),
                  blockNumber=rep(c('X', 'Y', 'X', 'Y'), c(4, 2, 2, 4)),
                  trait=c(-1, -1, 1, 1, 8, 10, -4, -2, 5, 7, 5, 7))
  model <- stats::lm(trait ~ germplasmName + blockNumber, data=d)
  # The raw means of A and B are equal, so the sequential F test sees nothing.
  expect_equal(stats::anova(model)['germplasmName', 'Pr(>F)'], 1)

  out <- compareMeans(model)
  expect_true(out$significant)
  expect_equal(abs(diff(out$means$mean)), 3)
  expect_equal(out$df, 9)
  expect_identical(out$means$group, c('a', 'b'))
})

test_that("a model with an interaction compares the marginal means", {
  d <- rbind(transform(rcbd(seed=1), site='A'), transform(rcbd(seed=2), site='B'))
  d$trait[d$site == 'B'] <- d$trait[d$site == 'B'] + 2
  byLm <- compareMeans(stats::lm(trait ~ germplasmName * site + blockNumber:site, data=d))
  byLmer <- compareMeans(lmerTest::lmer(trait ~ germplasmName * site + (1|site:blockNumber), data=d))

  expect_true(byLm$significant)
  expect_equal(nrow(byLm$means), 8)
  expect_equal(byLm$means$mean, byLmer$means$mean, tolerance=1e-6)
  expect_equal(byLm$lsd, byLmer$lsd, tolerance=1e-4)
})

test_that("the F test of an lm model uses the compared means", {
  x <- rep(-2:2, each=2)
  d <- data.frame(germplasmName=rep(c('A', 'B'), each=10), x=c(x, x),
                  trait=c(5 + rep(c(-0.1, 0.1), 5), 7 - 2 * x^2 + rep(c(-0.1, 0.1), 5)))
  out <- compareMeans(stats::lm(trait ~ germplasmName * poly(x, 2, raw=TRUE), data=d))

  expect_equal(out$means$mean, c(7, 5), tolerance=1e-6)
  expect_true(out$significant)
  expect_identical(out$means$group, c('a', 'b'))
})

test_that("a factor name with a space is found", {
  d <- rcbd()
  names(d)[names(d) == 'germplasmName'] <- 'entry group'
  out <- compareMeans(stats::lm(trait ~ `entry group` + blockNumber, data=d), factor='entry group')
  expect_true(out$significant)
  expect_identical(names(out$means)[1], 'entry group')
})

test_that("without degrees of freedom the means are not compared", {
  d <- data.frame(germplasmName=c('A', 'B', 'C'), trait=c(1, 2, 3))
  expect_error(compareMeans(stats::lm(trait ~ germplasmName, data=d)), 'no degrees of freedom')
})

test_that("a factor that is not a fixed effect is an error", {
  d <- rcbd()
  model <- suppressMessages(lmerTest::lmer(trait ~ (1|germplasmName) + (1|blockNumber), data=d))
  expect_error(compareMeans(model), 'not a fixed effect')
  expect_error(compareMeans(stats::lm(trait ~ germplasmName, data=d), alpha=1), 'between 0 and 1')
})
