#' fishmax: Extreme Value Analysis for Estimating the maximum body length of fishes
#'
#' This package provides tools for extreme value analysis specifically
#' designed for fish body max length. It implements both traditional
#' Extreme Value Theory (EVT) and Extreme Fish Size (EFS) models using
#' Bayesian methods via Stan.
#'
#' @docType package
#' @name fishmax-package
#' @aliases fishmax
#' @import methods
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fit_max_model}}}{Fit EVT or EFS models to data}
#'   \item{\code{\link{get_posterior}}}{Extract posterior samples}
#'   \item{\code{\link{get_lmax}}}{Estimate maximum size for given return period}
#'   \item{\code{\link{get_pdf}}}{Compute probability density function}
#'   \item{\code{\link{plot_fit}}}{Visually compare the PDFs of the fitted models}
#'   \item{\code{\link{plot_traceplot}}}{Visualise the fitting process of the models}
#' }
#'
"_PACKAGE"

## Quiets concerns of R CMD check regarding undefined global variables
if (getRversion() >= "4.1") {
  utils::globalVariables(c(
    ".",
    ".chain",
    ".draw",
    ".iteration",
    "cdf",
    "lambda",
    "loc",
    "max_fit",
    "max_lwr",
    "max_upr",
    "mu",
    "pdf",
    "pdf_fit",
    "pdf_lwr",
    "pdf_upr",
    "scale",
    "shape",
    "sigma",
    "size",
    "value",
    "x"
  ))
}
