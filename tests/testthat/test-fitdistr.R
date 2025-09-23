test_that("observed proportion of zeros is returned", {
  true_pzero <- seq(0, 1, 0.1)
  Nx <- 100

  for (pzero in true_pzero) {
    x <- runif(Nx)
    is_zero <- as.logical( rbinom(Nx, 1, pzero) )

    x[is_zero] <- 0
    obs_pzero <- mean(is_zero)

    res <- fit_zoabeta(x)
    expect_equal(res['pzero'], obs_pzero, ignore_attr = "names")
  }
})

test_that("observed proportion of ones is returned", {
  true_pone <- seq(0, 1, 0.1)
  Nx <- 100

  for (pone in true_pone) {
    x <- runif(Nx)
    is_one <- as.logical( rbinom(Nx, 1, pone) )

    x[is_one] <- 1
    obs_pone <- mean(is_one)

    res <- fit_zoabeta(x)
    expect_equal(res['pone'], obs_pone, ignore_attr = "names")
  }
})

test_that("shape params are NA when data have too many zeros", {
  x <- rep(0, 100)
  res <- fit_zoabeta(x, edge_threshold = 0.9)
  expect_true(is.na(res['shape1']))
  expect_true(is.na(res['shape2']))
})

test_that("shape params are NA when data have too many ones", {
  x <- rep(1, 100)
  res <- fit_zoabeta(x, edge_threshold = 0.9)
  expect_true(is.na(res['shape1']))
  expect_true(is.na(res['shape2']))
})

test_that("beta shape params are estimated reliably", {
  true_shapes <- expand.grid(
    shape1 = seq(1, 10, 1.5),
    shape2 = seq(1, 10, 1.5) ) |>

    as.matrix()

  Nx <- 1e3

  param_ratio <- apply(true_shapes, MARGIN = 1, FUN = function(shapes) {
    x <- rbeta(Nx, shapes[1], shapes[2])
    res <- fit_zoabeta(x)

    # ratio of estimated param values to true values
    res[c('shape1', 'shape2')] / shapes
  })

  # Check that the ratios are reasonably close to 1.
  # TODO - think about a more robust way to implement this test
  testthat::expect_true(all(param_ratio > 0.8 & param_ratio < 1.2))
})

