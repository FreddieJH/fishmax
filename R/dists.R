#' Truncated normal distribution (lower bound at 0)
#'
#' Density, distribution function, quantile function and random generation
#' for the normal distribution truncated at zero from below.
#'
#' @param x,q Vector of quantiles
#' @param p Vector of probabilities
#' @param n Number of observations
#' @param mean Vector of means
#' @param sd Vector of standard deviations
#'
#' @return
#' \code{dtnorm} gives the density, \code{ptnorm} gives the distribution
#' function, \code{qtnorm} gives the quantile function, and \code{rtnorm}
#' generates random deviates.
#'
#' @details
#' These are convenience wrappers around \code{\link[truncnorm]{dtruncnorm}}
#' and related functions, with the lower bound fixed at 0.
#'
#' @name tnorm
#' @examples
#' # Density at x = 1
#' dtnorm(1, mean = 0, sd = 1)
#'
#' # Generate 100 random values
#' rtnorm(100, mean = 10, sd = 2)
NULL

#' @rdname tnorm
#' @export
dtnorm <- function(x, mean, sd) {
  truncnorm::dtruncnorm(x = x, mean = mean, sd = sd, a = 0)
}

#' @rdname tnorm
#' @export
ptnorm <- function(q, mean, sd) {
  truncnorm::ptruncnorm(q = q, mean = mean, sd = sd, a = 0)
}

#' @rdname tnorm
#' @export
qtnorm <- function(p, mean, sd) {
  truncnorm::qtruncnorm(p = p, mean = mean, sd = sd, a = 0)
}

#' @rdname tnorm
#' @export
rtnorm <- function(n, mean, sd) {
  truncnorm::rtruncnorm(n = n, mean = mean, sd = sd, a = 0)
}

#' Vectorised GEV distribution functions
#'
#' Vectorised versions of the generalised extreme value distribution functions
#' from the evd package.
#'
#' @param x,q Vector of quantiles
#' @param loc,scale,shape Location, scale and shape parameters
#' @param ... Additional arguments passed to underlying evd functions
#'
#' @return
#' \code{dgev} gives the density and \code{pgev} gives the distribution function.
#'
#' @details
#' These are vectorised wrappers around \code{\link[evd]{dgev}} and
#' \code{\link[evd]{pgev}}, allowing all parameters to be vectors.
#'
#' @seealso \code{\link[evd]{gev}} for the original non-vectorised functions
#' @name gev_vectorised
NULL

#' @rdname gev_vectorised
#' @export
dgev_v <- Vectorize(evd::dgev)

#' @rdname gev_vectorised
#' @export
pgev_v <- Vectorize(evd::pgev)

#' @rdname gev_vectorised
#' @export
qgev_v <- Vectorize(evd::qgev)

#' Vectorised Gumbel distribution functions
#'
#' Vectorised versions of the Gumbel distribution functions from the evd package.
#'
#' @param x,q Vector of quantiles
#' @param loc,scale Location and scale parameters
#' @param ... Additional arguments passed to underlying evd functions
#'
#' @return
#' \code{dgumbel} gives the density and \code{dgumbel} gives the distribution
#' function.
#'
#' @details
#' These are vectorised wrappers around \code{\link[evd]{dgumbel}} and
#' \code{\link[evd]{pgumbel}}, allowing all parameters to be vectors.
#'
#' @seealso \code{\link[evd]{gumbel}} for the original non-vectorised functions
#' @name gumbel_vectorised
NULL

#' @rdname gumbel_vectorised
#' @export
dgumbel_v <- Vectorize(evd::dgumbel)

#' @rdname gumbel_vectorised
#' @export
pgumbel_v <- Vectorize(evd::pgumbel)

#' @rdname gumbel_vectorised
#' @export
qgumbel_v <- Vectorize(evd::qgumbel)
