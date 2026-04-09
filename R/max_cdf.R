#' CDF of the maximum of n samples
#'
#' #' Computes the cumulative distribution function of the maximum of `n` i.i.d. samples
#' from a distribution with CDF `cdf`.
#'
#' @param x Numeric vector of values at which to evaluate the CDF
#' @param n Single positive integer: number of i.i.d. samples
#' @param cdf Function returning the CDF F(x)
#'
#' @return Numeric vector of CDF values for the maximum
#'
#' @details For i.i.d. samples, the CDF of the maximum is
#'   \eqn{G(x) = F(x)^n}, where \eqn{F} is the CDF of the underlying distribution.
#'
#' @examples
#' # Standard normal distribution
#' max_cdf(x = c(1, 1.5), n = 10, cdf = pnorm)
#'
#' # Normal with mean = 5, sd = 2
#' max_cdf(x = 6, n = 10, cdf = \(x) pnorm(x, 5, 2))
#'
#' # Exponential with rate = 0.5
#' max_cdf(x = 2, n = 5, cdf = \(x) pexp(x, 0.5))
#'
#' # Beta distribution
#' max_cdf(x = 0.7, n = 8, cdf = \(x) pbeta(x, 2, 5))
#' @export
max_cdf <- function(x, n, cdf) {
  stopifnot(is.numeric(x))
  stopifnot(length(n) == 1 && n >= 1 && n == as.integer(n))
  stopifnot(is.function(cdf))

  cdf(x)^n
}
