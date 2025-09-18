test_that("density is the same as dbeta when pzero and pone default to zero", {
  x <- runif(100, min = 0.01, max = 0.99)
  expect_equal(dzoabeta(x, 2, 5), dbeta(x, 2, 5))
})

test_that("probability is the same as pbeta when pzero and pone default to zero", {
  x <- runif(100, min = 0.01, max = 0.99)
  expect_equal(pzoabeta(x, 2, 5), pbeta(x, 2, 5))
})

test_that("quantiles are the same as qbeta when pzero and pone default to zero", {
  p <- runif(100, min = 0.01, max = 0.99)
  expect_equal(qzoabeta(p, 2, 5), qbeta(p, 2, 5))
})

test_that("dzoabeta returns correct pzero",  {
  true_pzero <- runif(100)
  d <- dzoabeta(0, 2, 5, pzero = true_pzero)
  expect_equal(d, true_pzero)
})

test_that("dzoabeta returns correct pone",  {
  true_pone <- runif(100)
  d <- dzoabeta(1, 2, 5, pzero = 0, pone = true_pone)
  expect_equal(d, true_pone)
})

test_that("pzoabeta returns correct pzero",  {
  true_pzero <- c(0, runif(99))
  p <- pzoabeta(0, 2, 5, pzero = true_pzero)
  expect_equal(p, true_pzero)
})

test_that("pzoabeta always returns 1.0 for x=1.0",  {
  true_pone <- c(0, runif(99))
  p <- pzoabeta(1, 2, 5, pzero = 0, pone = true_pone)
  expect_equal(p, rep(1.0, length(true_pone)))
})

test_that("rzoabeta honours pzero", {
  Nrep <- 100
  Ndraws <- 1000

  true_pzero <- c(0, runif(Nrep-1))

  # For each replicate sample of Ndraws random values, calculate the proportion
  # of zero values
  obs_pzero <- sapply(1:Nrep, function(i) { mean(rzoabeta(Ndraws, 2, 5, pzero = true_pzero[i]) == 0) })

  # Compare true pzero values to observed proportion of zeros
  m <- lm(obs_pzero ~ true_pzero)

  # Intercept should be close to zero and slope should be close to one
  mpars <- coef(m)
  expect_lt(abs(mpars[1]), 0.01)
  expect_lt(abs(mpars[2] - 1), 0.01)
})

test_that("rzoabeta honours pone", {
  Nrep <- 100
  Ndraws <- 1000

  true_pone <- c(0, runif(Nrep-1))

  # For each replicate sample of Ndraws random values, calculate the proportion
  # of zero values
  obs_pone <- sapply(1:Nrep, function(i) { mean(rzoabeta(Ndraws, 2, 5, pone = true_pone[i]) == 1) })

  # Compare true pone values to observed proportion of zeros
  m <- lm(obs_pone ~ true_pone)

  # Intercept should be close to zero and slope should be close to one
  mpars <- coef(m)
  expect_lt(abs(mpars[1]), 0.01)
  expect_lt(abs(mpars[2] - 1), 0.01)
})


