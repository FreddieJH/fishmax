#' CDF of the maximum of n samples
#'
#' @param x Numeric vector of values at which to evaluate the CDF
#' @param n Integer, number of samples
#' @param cdf Function returning the CDF F(x)
#'
#' @return Numeric vector of CDF values for the maximum
#'
#' @details Uses the formula: G(x) = F(x)^n
#'
#' @examples
#' # Standard normal distribution
#' G(1.5, n = 10, cdf = pnorm)
#'
#' # Normal with mean = 5, sd = 2
#' G(6, n = 10, cdf = \(x) pnorm(x, 5, 2))
#'
#' # Exponential with rate = 0.5
#' G(2, n = 5, cdf = \(x) pexp(x, 0.5))
#'
#' # Beta distribution
#' G(0.7, n = 8, cdf = \(x) pbeta(x, 2, 5))
G <- function(x, n, cdf) {
  cdf(x)^n
}
