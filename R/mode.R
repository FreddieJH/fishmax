#' Find Mode of a Probability Density Function
#'
#' Numerically determines the mode (maximum) of a univariate probability density
#' function using optimisation over a specified interval.
#'
#' @param f Function. A univariate function (typically a probability density
#'   function) that takes a numeric value and returns a scalar.
#' @param lwr Numeric. Lower bound of the search interval. Default is 0.
#' @param upr Numeric. Upper bound of the search interval. Default is 1000.
#'
#' @return Numeric value representing the x-coordinate where f(x) is maximised
#'   within the specified interval.
#'
#' @details Uses R's built-in `optimise()` function to find the maximum of f over the
#' interval 'lwr' to 'upr'. The search interval should be chosen to contain the
#' mode of the distribution. For distributions with support outside 0 to 1000,
#' adjust lwr and upr accordingly.
#'
#' @examples
#' # Find mode of standard normal density
#' mode_f(dnorm, lwr = -5, upr = 5)
#'
#' # Find mode of gamma distribution
#' mode_f(\(x) dgamma(x, shape = 3, rate = 1), lwr = 0, upr = 10)
#'
#' # Find mode of custom density
#' f <- \(x) dbeta(x, shape1 = 2, shape2 = 5)
#' mode_f(f, lwr = 0, upr = 1)
#'
mode_f <- function(f, lwr = -500, upr = 500) {
  # Evaluate at several points to find best starting region
  x_grid <- seq(lwr, upr, length.out = 100)
  f_vals <- sapply(x_grid, f)
  best_idx <- which.max(f_vals)

  # Narrow the search around the best point
  if (best_idx == 1) {
    search_lwr <- lwr
    search_upr <- x_grid[min(best_idx + 10, length(x_grid))]
  } else if (best_idx == length(x_grid)) {
    search_lwr <- x_grid[max(best_idx - 10, 1)]
    search_upr <- upr
  } else {
    search_lwr <- x_grid[max(best_idx - 10, 1)]
    search_upr <- x_grid[min(best_idx + 10, length(x_grid))]
  }

  optimise(f, interval = c(search_lwr, search_upr), maximum = TRUE)$maximum
}
