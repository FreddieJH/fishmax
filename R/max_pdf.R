#' PDF of the maximum of n samples
#'
#' @param x Numeric vector of values at which to evaluate the PDF
#' @param n Integer, number of samples
#' @param cdf Function returning the CDF F(x)
#' @param pdf Function returning the PDF f(x)
#'
#' @return Numeric vector of PDF values for the maximum
#'
#' @details Uses the formula: g(x) = n * F(x)^(n-1) * f(x)
#'
#' @examples
#' # Standard normal distribution
#' max_pdf(1.5, n = 10, cdf = pnorm, pdf = dnorm)
#'
#' # Normal with mean = 5, sd = 2
#' max_pdf(6, n = 10, cdf = \(x) pnorm(x, 5, 2), pdf = \(x) dnorm(x, 5, 2))
#'
#' # Exponential with rate = 0.5
#' max_pdf(2, n = 5, cdf = \(x) pexp(x, 0.5), pdf = \(x) dexp(x, 0.5))
#'
#' # Beta distribution
#' max_pdf(0.7, n = 8, cdf = \(x) pbeta(x, 2, 5), pdf = \(x) dbeta(x, 2, 5))
max_pdf <- function(x, n, cdf, pdf) {
  n * cdf(x)^(n - 1) * pdf(x)
}
