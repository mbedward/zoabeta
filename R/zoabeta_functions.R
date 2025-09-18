#' Zero-one-augmented beta Distribution
#'
#' Density, distribution function, quantile function and random generation for
#' the Zero-one-augmented beta distribution with parameters
#' \code{shape1}, \code{shape2}, \code{pzero} and \code{pone}.
#'
#' @param x Vector of quantiles.
#'
#' @param p Vector of probabilities.
#'
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#'   to be the number required.
#'
#' @param shape1,shape2 Non-negative parameters of the beta distribution for
#'   values greater than zero and less than one.
#'
#' @param pzero Probability of a value of zero (default is 0).
#'
#' @param pone Probability of a value of one (default is 0).
#'
#' @param log,log.p If \code{TRUE}, log density / probability values are returned.
#'
#' @param lower.tail If \code{TRUE} (default), probabilities are
#'   \eqn{P[X \le x]}, otherwise \eqn{P[X \gt x]}.
#'
#' @return \code{dzoabeta} returns the density, \code{pzoabeta} returns the
#'   probability, \code{qzoabeta} returns the quantile, and \code{rzoabeta}
#'   generates random values.
#'
#' @export
#'
dzoabeta <- function(x, shape1, shape2, pzero=0, pone=0, log=FALSE) {
  checkmate::assert_numeric(x)

  # Shape parameters must be positive
  checkmate::qassert(shape1, "N+(0,)")
  checkmate::qassert(shape2, "N+(0,)")

  # Probability parameters must both be in [0,1]
  checkmate::qassert(pzero, "N+[0,1]")
  checkmate::qassert(pone, "N+[0,1]")

  # Create five column matrix of p values and parameters, allowing default
  # recycling to happen for any variable(s) as per stats::pbeta()
  pars <- suppressWarnings(cbind(x, shape1, shape2, pzero, pone))
  X <- 1; SHAPE1 <- 2; SHAPE2 <- 3; PZERO <- 4; PONE <- 5;

  # Check that probability parameters never sum to more than 1.0
  if (!checkmate::qtest(pars[,PZERO] + pars[,PONE], "N+[0,1]")) {
    stop("Probabilities of zero and one values sum to more than 1.0")
  }

  d <- apply(pars, MARGIN = 1, function(xpars) {
    if(xpars[X] == 0) xpars[PZERO]
    else if (xpars[X] == 1.0) xpars[PONE]
    else (1-xpars[PZERO]-xpars[PONE]) * dbeta(xpars[X], xpars[SHAPE1], xpars[SHAPE2])
  })

  if (log[1]) d <- log(d)

  d
}


#' @rdname dzoabeta
#'
#' @export
#'
pzoabeta <- function(x, shape1, shape2, pzero=0, pone=0, lower.tail = TRUE, log.p = FALSE) {
  # Create five column matrix of p values and parameters, allowing default
  # recycling to happen for any variable(s) as per stats::pbeta()
  pars <- suppressWarnings(cbind(x, shape1, shape2, pzero, pone))
  X <- 1; SHAPE1 <- 2; SHAPE2 <- 3; PZERO <- 4; PONE <- 5;

  # Check that probability parameters never sum to more than 1.0
  if (!checkmate::qtest(pars[,PZERO] + pars[,PONE], "N+[0,1]")) {
    stop("Probabilities of zero and one values sum to more than 1.0")
  }

  p <- apply(pars, MARGIN = 1, function(xpars) {
    if(xpars[X] == 0) xpars[PZERO]
    else if (xpars[X] == 1.0) 1.0
    else xpars[PZERO] + (1-xpars[PZERO]-xpars[PONE]) * pbeta(xpars[X], xpars[SHAPE1], xpars[SHAPE2])
  })

  if (!lower.tail[1]) p <- 1-p
  if (log.p[1]) p <- log(p)

  p
}

#' @rdname dzoabeta
#'
#' @export
#'
qzoabeta <- function(p, shape1, shape2, pzero=0, pone=0, lower.tail = TRUE, log.p = FALSE) {
  # Create five column matrix of p values and parameters, allowing default
  # recycling to happen for any variable(s) as per stats::qbeta()
  pars <- suppressWarnings(cbind(p, shape1, shape2, pzero, pone))
  P <- 1; SHAPE1 <- 2; SHAPE2 <- 3; PZERO <- 4; PONE <- 5;

  # Check that probability parameters never sum to more than 1.0
  if (!checkmate::qtest(pars[,PZERO] + pars[,PONE], "N+[0,1]")) {
    stop("Probabilities of zero and one values sum to more than 1.0")
  }

  if (!lower.tail[1]) pars[,P] <- 1 - pars[,P]
  if (log.p[1]) pars[,P] <- exp(pars[,P])

  apply(pars, MARGIN = 1, function(xpars) {
    if(xpars[P] <= xpars[PZERO]) 0.0
    else if (1 - xpars[P] <= xpars[PONE]) 1.0
    else qbeta(p = (xpars[P]-xpars[PZERO]-xpars[PONE]) / (1-xpars[PZERO]-xpars[PONE]),
               shape1 = xpars[SHAPE1], shape2 = xpars[SHAPE2],
               lower.tail=TRUE, log.p=FALSE)
  })
}


#' @rdname dzoabeta
#'
#' @export
#'
rzoabeta <- function(n, shape1, shape2, pzero=0, pone=0) {
  # Force all parameters to be length(1) to avoid confusion
  checkmate::assert_count(n)
  checkmate::assert_number(shape1, lower=0, finite=TRUE)
  checkmate::assert_number(shape2, lower=0, finite=TRUE)
  checkmate::assert_number(pzero, lower=0, upper = 1, finite=TRUE)
  checkmate::assert_number(pone, lower=0, upper = 1, finite=TRUE)

  # Check that probability parameters do not sum to more than 1.0
  if (pzero + pone > 1.0) {
    stop("Probabilities of zero and one values sum to more than 1.0")
  }

  xout <- rep(0, n)

  # Identify any elements to set to zero or one
  r <- runif(n)
  is.zero <- r < pzero
  is.one <- r > 1 - pone
  is.beta <- !(is.zero | is.one)

  xout[is.beta] <- rbeta(sum(is.beta), shape1, shape2)
  xout[is.one] <- 1

  xout
}
