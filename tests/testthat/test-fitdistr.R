test_that("observed proportion of zeros is returned", {
  Nx <- 100
  Nzeros <- seq(2, Nx-1, by=5)
  true_pzero <- Nzeros / Nx

  estimated_pzero <- sapply(Nzeros, function(n) {
    x <- rbeta(Nx, 2, 2)

    # Set some values to 0
    x[sample(1:Nx, size=n)] <- 0

    res <- fit_zoabeta(x)
    res['pzero']
  })

  expect_true(all.equal(estimated_pzero, true_pzero, check.attributes = FALSE))
})

test_that("observed proportion of ones is returned", {
  Nx <- 100
  Nones <- seq(2, Nx-1, by=5)
  true_pone <- Nones / Nx

  estimated_pone <- sapply(Nones, function(n) {
    x <- rbeta(Nx, 2, 2)

    # Set some values to 1
    x[sample(1:Nx, size=n)] <- 1

    res <- fit_zoabeta(x)
    res['pone']
  })

  expect_true(all.equal(estimated_pone, true_pone, check.attributes = FALSE))
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

# TODO: this about a more robust way to test the fitting method
test_that("beta shape params are estimated reliably", {
  true_shapes <- expand.grid(
    shape1 = seq(1, 10, 1.5),
    shape2 = seq(1, 10, 1.5) ) |>

    as.matrix()

  Nx <- 1e3

  # Simulate beta values for each set of true shape parameters and then use
  # fit_zoabeta to try to recover the input parameters. Express agreement as the
  # ratio of estimated to true parameters.
  param_ratio <- apply(true_shapes, MARGIN = 1, FUN = function(shapes) {
    x <- rbeta(Nx, shapes[1], shapes[2])
    res <- fit_zoabeta(x)

    # ratio of estimated param values to true values
    res[c('shape1', 'shape2')] / shapes
  })

  # Allow some wiggle room between true and estimated parameters
  testthat::expect_true(all(param_ratio > 0.8 & param_ratio < 1.2))
})

