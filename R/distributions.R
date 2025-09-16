#' Truncated normal distribution functions
#'
#' Density, distribution, quantile, and random generation functions
#' for truncated normal distribution (truncated at 0)
#'
#' @param x,q Vector of quantiles
#' @param p Vector of probabilities
#' @param n Number of observations
#' @param par1 Mean parameter
#' @param par2 Standard deviation parameter
#'
#' @return Numeric vector of densities, probabilities, quantiles, or random values
#' @name truncated_normal
NULL

#' @rdname truncated_normal
#' @export
dtnorm <- function(x, par1, par2) {
  truncnorm::dtruncnorm(x = x, a = 0, mean = par1, sd = par2)
}

#' @rdname truncated_normal
#' @export
ptnorm <- function(x, par1, par2) {
  truncnorm::ptruncnorm(q = x, a = 0, mean = par1, sd = par2)
}

#' @rdname truncated_normal
#' @export
qtnorm <- function(x, par1, par2) {
  truncnorm::qtruncnorm(p = x, a = 0, mean = par1, sd = par2)
}

#' @rdname truncated_normal
#' @export
rtnorm <- function(x, par1, par2) {
  truncnorm::rtruncnorm(n = x, a = 0, mean = par1, sd = par2)
}

#' PDF of maxima given distribution parameters
#'
#' Computes the probability density function of sample maxima
#' given the underlying distribution
#'
#' @param x Vector of values
#' @param distr Distribution name (e.g., "tnorm")
#' @param n Sample size
#' @param par1 First parameter of underlying distribution
#' @param par2 Second parameter of underlying distribution
#'
#' @return Vector of density values
#' @export
pdf_max <- function(x, distr, n, par1, par2) {
  f_x <- function(x) get(paste0("d", distr))(x, par1, par2)
  F_x <- function(x) get(paste0("p", distr))(x, par1, par2)

  log_pdf_max <- log(n) + (n - 1) * log(F_x(x)) + log(f_x(x))
  exp(log_pdf_max)
}

#' CDF of maxima
#'
#' Computes the cumulative distribution function of sample maxima
#'
#' @inheritParams pdf_max
#'
#' @return Vector of cumulative probabilities
#' @export
cdf_max <- function(x, distr, n, par1, par2) {
  F_x <- function(x) get(paste0("p", distr))(x, par1, par2)
  F_x(x)^n
}

#' Quantile function for maxima
#'
#' Computes quantiles for the distribution of sample maxima
#'
#' @param distr Distribution name
#' @param n Sample size
#' @param par1,par2 Distribution parameters
#' @param p Probability
#' @param interval_lwr,interval_upr Search interval bounds
#'
#' @return Quantile value
#' @export
inverse_cdf_x <- function(
  distr,
  n,
  par1,
  par2,
  p,
  interval_lwr = 1,
  interval_upr = 1000
) {
  stats::uniroot(
    function(x) cdf_max(x, distr = distr, n = n, par1 = par1, par2 = par2) - p,
    lower = interval_lwr,
    upper = interval_upr
  )$root
}
