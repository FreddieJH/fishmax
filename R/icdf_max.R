#' Inverse CDF of the maximum of n samples
#'
#' @param n Integer, number of samples
#' @param cdf Function returning the CDF F(x)
#' @param p Numeric between 0 and 1, desired quantile, default = 0.95
#' @param lwr Integer, lower bound of the derivative calculation, default = 0
#' @param upr Integer, upper bound of the derivative calculation, default = 5000
#'
#' @return Numeric vector of CDF values for the maximum
#'
#' @details Uses the formula: G(x) = F(x)^n
#'
#' @examples
#' # Standard normal distribution
#' invG(n = 1000, cdf = pnorm, p = 0.95)
#'
#' # Normal with mean = 5, sd = 2
#' invG(n = 1000, cdf = \(x) pnorm(x, 5, 2), p = 0.95)
#'
#' # Exponential with rate = 0.5
#' invG(n = 1000, cdf = \(x) pexp(x, 0.5), p = 0.95)
#'
#' # Beta distribution
#' invG(n = 1000, cdf = \(x) pbeta(x, 2, 5), p = 0.95)
invG <- function(n, cdf, p = 0.95, lwr = 0, upr = 1000) {
  # Check if root exists in initial interval
  f <- function(x) G(x, n, cdf) - p

  f_lwr <- f(lwr)
  f_upr <- f(upr)

  # Extend upper bound if needed
  while (f_upr < 0) {
    upr <- upr * 10
    f_upr <- f(upr)
    if (upr > 1e10) stop("Upper bound exceeded reasonable limit")
  }

  # Extend lower bound if needed (for negative values)
  while (f_lwr > 0) {
    lwr <- lwr - abs(lwr) * 10 - 1000
    f_lwr <- f(lwr)
    if (lwr < -1e10) stop("Lower bound exceeded reasonable limit")
  }

  uniroot(f, interval = c(lwr, upr))$root
}
