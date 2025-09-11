#' fishEVA: Extreme Value Analysis for Marine Biology
#'
#' This package provides tools for extreme value analysis specifically
#' designed for marine biological data. It implements both traditional
#' Extreme Value Theory (EVT) and Extreme Fish Size (EFS) models using
#' Bayesian methods via Stan.
#'
#' @docType package
#' @name fishEVA-package
#' @aliases fishEVA
#' @import methods
#' @importFrom magrittr %>%
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fit_evt_model}}}{Fit EVT or EFS models to data}
#'   \item{\code{\link{get_posterior}}}{Extract posterior samples}
#'   \item{\code{\link{est_max}}}{Estimate maximum size for given return period}
#'   \item{\code{\link{get_pdf}}}{Compute probability density function}
#'   \item{\code{\link{plot_model_comparison}}}{Create model comparison plots}
#' }
#'
"_PACKAGE"

## Quiets concerns of R CMD check regarding undefined global variables
if (getRversion() >= "2.15.1") {
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
