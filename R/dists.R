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
  # stopifnot(all(x >= 0))
  normalisation_const <- 1 - pnorm(0, mean = mean, sd = sd)
  dnorm(x, mean = mean, sd = sd) / normalisation_const
}


#' @rdname tnorm
#' @export
ptnorm <- function(q, mean = 0, sd = 1) {
  # stopifnot(all(q >= 0))
  normalisation_const <- 1 - pnorm(0, mean = mean, sd = sd)
  (pnorm(q, mean = mean, sd = sd) - pnorm(0, mean = mean, sd = sd)) /
    normalisation_const
}

#' @rdname tnorm
#' @export
qtnorm <- function(p, mean, sd) {
  # stopifnot(all(p >= 0 & p <= 1))
  normalisation_const <- 1 - pnorm(0, mean = mean, sd = sd)
  qnorm(
    p * normalisation_const + pnorm(0, mean = mean, sd = sd),
    mean = mean,
    sd = sd
  )
}

#' @rdname tnorm
#' @export
rtnorm <- function(n, mean = 0, sd = 1) {
  u <- runif(n, min = 0, max = 1)
  qtnorm(u, mean = mean, sd = sd)
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
dgev_v <- function(x, loc = 0, scale = 1, shape = 0, ...) {
  # recycle parameters to match length of x
  args <- mapply(
    function(xi, l, s, sh) evd::dgev(xi, loc = l, scale = s, shape = sh, ...),
    x,
    loc,
    scale,
    shape,
    SIMPLIFY = TRUE
  )
  return(args)
}


#' @rdname gev_vectorised
#' @export
pgev_v <- function(q, loc = 0, scale = 1, shape = 0, ...) {
  args <- mapply(
    function(qi, l, s, sh) evd::pgev(qi, loc = l, scale = s, shape = sh, ...),
    q,
    loc,
    scale,
    shape,
    SIMPLIFY = TRUE
  )
  return(args)
}

#' @rdname gev_vectorised
#' @export
qgev_v <- function(p, loc = 0, scale = 1, shape = 0, ...) {
  args <- mapply(
    function(pi, l, s, sh) evd::qgev(pi, loc = l, scale = s, shape = sh, ...),
    p,
    loc,
    scale,
    shape,
    SIMPLIFY = TRUE
  )
  return(args)
}

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
dgumbel_v <- function(x, loc = 0, scale = 1, ...) {
  # recycle parameters to match length of x
  args <- mapply(
    function(xi, l, s) evd::dgumbel(xi, loc = l, scale = s, ...),
    x,
    loc,
    scale,
    SIMPLIFY = TRUE
  )
  return(args)
}

#' @rdname gumbel_vectorised
#' @export
pgumbel_v <- function(q, loc = 0, scale = 1, ...) {
  args <- mapply(
    function(qi, l, s) evd::pgumbel(qi, loc = l, scale = s, ...),
    q,
    loc,
    scale,
    SIMPLIFY = TRUE
  )
  return(args)
}

#' @rdname gumbel_vectorised
#' @export
qgumbel_v <- function(p, loc = 0, scale = 1, ...) {
  args <- mapply(
    function(pi, l, s) evd::qgumbel(pi, loc = l, scale = s, ...),
    p,
    loc,
    scale,
    SIMPLIFY = TRUE
  )
  return(args)
}
