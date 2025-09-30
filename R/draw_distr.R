#' Draw a zero-one-augmented beta distribution based on parameters or data
#'
#' This function displays a ZOA-beta distribution as a ggplot object. It accepts
#' either a named vector or list of distribution parameters, or a vector of
#' numeric values in the interval [0,1]. If \code{x} is named vector or list of
#' parameters must include elements 'shape1' and 'shape2', while elements
#' 'pzero' and 'pone' are optional and, if missing, will be set to zero.  If
#' \code{x} is a numeric vector of data values in the interval \code{[0,1]}, the
#' function \code{\link{fit_zoabeta}} will be called to estimate the
#' distribution parameters. The distribution will be displayed as a combination
#' of vertical lines indicating the probability of values of 0 and 1 (if
#' present), together with a density curve for values strictly between 0 and 1.
#' When \code{x} is a data vector, there is also the option of displaying the
#' values strictly between 0 and 1 as a histrogram rather than a density curve.
#'
#' @param x Either named list or numeric vector of distribution parameters, or
#'   a numeric vector of values in the interval \code{[0,1]} to which a
#'   distribution can be fitted.
#'
#' @param draw_hist (logical, default \code{FALSE}) In the case of \code{x}
#'   being a numeric vector of data values drawn from a ZOA-beta distribution,
#'   setting \code{hist=TRUE} will display a histogram for the values in
#'   \code{(0,1)} rather than a fitted density curve. Ignored if \code{x} is a
#'   named list or numeric vector of parameters.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes after_stat element_line element_text
#'   geom_area geom_histogram geom_segment scale_x_continuous scale_y_continuous
#'   sec_axis theme theme_minimal
#'
#' @importFrom scales oob_keep
#'
#' @export
#
draw_zoabeta <- function(x, draw_hist = FALSE) {
  x_is_data <- FALSE

  # If x is a list, try to retrieve distribution parameters
  if (is.list(x)) {
    params <- .get_params(x)

  } else if (is.numeric(x)) {
    if (length(x) %in% 2:4) {
      # Treat as a vector of parameters
      params <- .get_params(x)
    } else {
      # Treat as a vector of values and try to fit a distribution
      checkmate::assert_numeric(x, lower=0, upper=1, finite = TRUE, any.missing = FALSE)
      x_is_data <- TRUE
      params <- fit_zoabeta(x)
    }
  }

  checkmate::assert_flag(draw_hist)

  if (draw_hist && !x_is_data) {
    warning("Ignoring draw_hist argument since input is parameters rather than data values")
  }

  has_edge <- any(params[c('pzero', 'pone')] > 0)

  dat <- data.frame(x = seq(0, 1, length.out = 101))

  dat$dens <- stats::dbeta(dat$x, params['shape1'], params['shape2'])

  if (has_edge) {
    # Scale density if edge probabilities are non-zero
    dat$dens <- dat$dens * (1 - sum(params[c('pzero', 'pone')]))
  }

  scale_factor <- max(dat$dens)
  if (has_edge) {
    scale_factor <- scale_factor / max(params[c('pzero', 'pone')])
  }

  gg <- ggplot()

  if (x_is_data && draw_hist) {
    x_non_edge <- x[x>0 & x<1]
    gg <- gg + geom_histogram(aes(x = x_non_edge, after_stat(density)),
                              binwidth = 0.05,
                              colour = "black", fill = "grey80")
  } else {
    gg <- gg + geom_area(data = dat, aes(x = x, y = dens),
                         colour = "black", fill = "grey80")
  }
  gg <- gg + theme_minimal()

  if (has_edge) {
    gg <- gg +
      geom_segment(aes(x = 0, xend = 0, y = 0, yend = params['pzero'] * scale_factor),
                   color = "darkred", linewidth = 2) +

      geom_segment(aes(x = 1, xend = 1, y = 0, yend = params['pone'] * scale_factor),
                   color = "darkred", linewidth = 2) +

      scale_y_continuous(
        name = "Density",
        sec.axis = sec_axis(~ ./scale_factor, name = "Probability")
      ) +

      scale_x_continuous(name = "X", limits = c(0, 1), oob = scales::oob_keep) +

      theme(axis.line.y.right = element_line(color = "darkred", linewidth = 1),
            axis.ticks.y.right = element_line(color = "darkred"),
            axis.text.y.right = element_text(color = "darkred"),
            axis.title.y.right = element_text(color = "darkred"))
  }

  gg
}


.get_params <- function(x) {
  nms <- tolower(names(x))

  if (all(c("shape1", "shape2") %in% nms)) {
    names(x) <- nms

    shape1 <- x[['shape1']]
    shape2 <- x[['shape2']]

    if ('pzero' %in% nms) pzero <- x[['pzero']]
    else pzero <- 0

    if ('pone' %in% nms) pone <- x[['pone']]
    else pone <- 0

    c(shape1 = shape1, shape2 = shape2, pzero = pzero, pone = pone)

  } else {
    cli::cli_abort(
      "If input x is a list or vector of parameters it should have elements
       'shape1' and 'shape2'")
  }
}


