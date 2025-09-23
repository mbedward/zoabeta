#' Maximum likelihood fitting of the zero-inflated beta distribution
#'
#' Fits a zero-inflated beta distribution to a vector of data values that lie in
#' the interval \code{[0,1]}.
#'
#' @param x Numeric vector of values between 0 and 1
#'
#' @param edge_threshold (single numeric value; default 0.99) The maximum
#'   proportion of zero and/or one values in \code{x} above which fitting
#'   parameters for the beta component will be skipped.
#'
#' @return Named numeric vector with elements:
#'   \item{pzero}{Proportion of zero values}
#'   \item{pone}{Proportion of one values}
#'   \item{shape1}{Shape1 parameter of beta distribution (NA if not fitted)}
#'   \item{shape2}{Shape2 parameter of beta distribution (NA if not fitted)}
#'
#' @export
#
fit_zoabeta <- function(x, edge_threshold = 0.99) {
  checkmate::assert_numeric(x, lower=0, upper=1, any.missing = FALSE)

  # Proportion of zero sample values
  iszero <- x == 0
  pzero <- mean(iszero)

  isone <- x == 1
  pone <- mean(isone)

  isedge <- iszero | isone

  # Fit a beta distribution to the non-zero values, if there are enough.
  shape1 <- NA_real_
  shape2 <- NA_real_
  if (pzero + pone <= edge_threshold) {
    init_pars = c(mean(x[!isedge]), 1.0)
    suppressWarnings({
      o <- optim(par = init_pars, fn = .fn_beta_ll, gr=NULL, x = x[!isedge],
                 method = "L-BFGS-B",
                 lower = c(0.01, 0.1), upper = c(0.99, 100))
      fitted_pars <- o$par
    })

    shape1 <- fitted_pars[2] * fitted_pars[1]
    shape2 <- fitted_pars[2] * (1 - fitted_pars[1])
  }

  c(shape1=shape1, shape2=shape2, pzero=pzero, pone=pone)
}


#### Non-exported helper functions

# Beta distribution density function parameterized via mean and dispersion
.dbeta2 <- function(x, mu, phi, ...) dbeta(x, phi*mu, phi*(1-mu), ...)


# Calculate negative summed log-likelihood given a vector of two beta parameters and a
# vector of data values
.fn_beta_ll <- function(pars, x) {
  mu <- pars[1]
  phi <- pars[2]
  -sum(.dbeta2(x, mu, phi, log=TRUE))
}

