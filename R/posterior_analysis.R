#' Extract posterior samples
#'
#' Extracts posterior samples from fitted model as a tibble
#'
#' @param fit CmdStanMCMC object from fit_evt_model
#'
#' @return Tibble containing posterior samples
#' @export
#' @importFrom posterior as_draws_df
#' @importFrom dplyr as_tibble
get_posterior <- function(fit) {
  fit |>
    posterior::as_draws_df() |>
    dplyr::as_tibble()
}

#' Estimate maximum size
#'
#' Estimates maximum size given return period k
#'
#' @param fit Fitted model object
#' @param ci Credible interval width (default: 0.8)
#' @param k Return period
#'
#' @return Vector of c(estimate, lower CI, upper CI)
#' @export
#' @importFrom dplyr mutate summarise
#' @importFrom purrr pmap_dbl
est_max <- function(fit, ci = 0.8, k) {
  posterior_samples <- get_posterior(fit)
  is_evt <- sum(c("loc", "scale", "shape") %in% colnames(posterior_samples)) ==
    3

  posterior_samples |>
    dplyr::mutate(
      pdf = if (is_evt) {
        purrr::pmap_dbl(
          list(p = 1 - (1 / k), loc = loc, scale = scale, shape = shape),
          evd::qgev
        )
      } else {
        purrr::pmap_dbl(
          list(
            distr = "tnorm",
            n = lambda,
            par1 = mu,
            par2 = sigma,
            p = 1 - (1 / k)
          ),
          inverse_cdf_x
        )
      }
    ) |>
    dplyr::summarise(
      max_fit = mean(pdf),
      max_lwr = stats::quantile(pdf, (1 - ci) / 2),
      max_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2))
    ) |>
    as.numeric()
}

#' Get probability density function
#'
#' Computes PDF across a range of values
#'
#' @param fit Fitted model object
#' @param xmin,xmax Range bounds
#' @param xstep Step size
#' @param ci Credible interval width
#'
#' @return Tibble with size and PDF estimates
#' @export
#' @importFrom tidyr expand_grid
get_pdf <- function(fit, xmin = 0, xmax = 300, xstep = 1, ci = 0.8) {
  posterior_samples <- get_posterior(fit)
  is_evt <- sum(c("loc", "scale", "shape") %in% colnames(posterior_samples)) ==
    3

  posterior_samples |>
    tidyr::expand_grid(size = seq(xmin, xmax, xstep)) |>
    dplyr::mutate(
      pdf = if (is_evt) {
        purrr::pmap_dbl(
          list(x = size, loc = loc, scale = scale, shape = shape),
          evd::dgev
        )
      } else {
        purrr::pmap_dbl(
          list(x = size, distr = "tnorm", n = lambda, par1 = mu, par2 = sigma),
          pdf_max
        )
      }
    ) |>
    dplyr::summarise(
      pdf_fit = stats::quantile(pdf, 0.5),
      pdf_lwr = stats::quantile(pdf, (1 - ci) / 2),
      pdf_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2)),
      .by = size
    )
}

#' Get underlying distribution PDF
#'
#' Computes the underlying distribution PDF (for EFS models)
#'
#' @inheritParams get_pdf
#'
#' @return Tibble with size and underlying PDF estimates
#' @export
get_underlying <- function(fit, xmin = 0, xmax = 300, xstep = 1, ci = 0.8) {
  get_posterior(fit) |>
    tidyr::expand_grid(size = seq(xmin, xmax, xstep)) |>
    dplyr::mutate(
      pdf = purrr::pmap_dbl(
        list(x = size, par1 = mu, par2 = sigma),
        dtnorm
      )
    ) |>
    dplyr::summarise(
      pdf_fit = stats::quantile(pdf, 0.5),
      pdf_lwr = stats::quantile(pdf, (1 - ci) / 2),
      pdf_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2)),
      .by = size
    )
}

#' Get cumulative distribution function
#'
#' Computes CDF across a range of values
#'
#' @inheritParams get_pdf
#'
#' @return Tibble with size and CDF estimates
#' @export
get_cdf <- function(fit, xmin = 0, xmax = 300, xstep = 1, ci = 0.8) {
  posterior_samples <- get_posterior(fit)
  is_evt <- sum(c("loc", "scale", "shape") %in% colnames(posterior_samples)) ==
    3

  posterior_samples |>
    tidyr::expand_grid(size = seq(xmin, xmax, xstep)) |>
    dplyr::mutate(
      cdf = if (is_evt) {
        purrr::pmap_dbl(
          list(q = size, loc = loc, scale = scale, shape = shape),
          evd::pgev
        )
      } else {
        purrr::pmap_dbl(
          list(x = size, distr = "tnorm", n = lambda, par1 = mu, par2 = sigma),
          cdf_max
        )
      }
    ) |>
    dplyr::summarise(
      cdf_fit = stats::quantile(cdf, 0.5),
      cdf_lwr = stats::quantile(cdf, (1 - ci) / 2),
      cdf_upr = stats::quantile(cdf, 1 - ((1 - ci) / 2)),
      .by = size
    )
}

#' Quantile Estimates from a GEV Posterior
#'
#' Computes posterior predictive quantiles for extreme values using the
#' Generalized Extreme Value (GEV) distribution parameters sampled from a
#' posterior distribution.
#'
#' @param evt_fitted_model A fitted model object, created using the fit_mod(model_type = "evt") function
#' @param q Numeric scalar giving the quantile probability to evaluate
#'   (default = 0.95).
#'
#' @return A tibble with three summary statistics of the posterior quantile
#'   distribution:
#'   \item{fit}{Posterior median of the quantile}
#'   \item{lwr}{10th percentile of the quantile distribution}
#'   \item{upr}{90th percentile of the quantile distribution}
#'
#' @examples
#' \dontrun{
#' est_max_evt(evt_fitted_model, q = 0.99)
#' }
#'
#' @importFrom dplyr mutate summarise
#' @importFrom purrr pmap_dbl
#' @importFrom evd qgev
est_max_evt <- function(evt_fitted_mod, q = 0.95) {
  evt_fitted_mod |>
    get_posterior() |>
    dplyr::mutate(
      q = purrr::pmap_dbl(
        .l = list(p = q, loc = loc, scale = scale, shape = shape),
        .f = evd::qgev
      )
    ) |>
    dplyr::summarise(
      fit = stats::quantile(q, 0.5),
      lwr = stats::quantile(q, 0.1),
      upr = stats::quantile(q, 0.9)
    )
}

#' Quantile Estimates from Truncated Normal-Based Posterior
#'
#' Computes posterior predictive quantiles for maximum values using a
#' truncated-normal-based model of fish size distributions.
#'
#' @param posterior A fitted model object, created using the fit_mod(model_type = "efs") function
#' @param q Numeric scalar giving the quantile probability to evaluate
#'   (default = 0.95).
#'
#' @return A tibble with three summary statistics of the posterior quantile
#'   distribution:
#'   \item{fit}{Posterior median of the quantile}
#'   \item{lwr}{10th percentile of the quantile distribution}
#'   \item{upr}{90th percentile of the quantile distribution}
#'
#' @details This function requires an \code{inverse_cdf_x()} function to be
#' defined, which computes quantiles for the truncated normal extreme value
#' model.
#'
#' @examples
#' \dontrun{
#' est_max_efs(efs_fitted_model, q = 0.95)
#' }
#'
#' @importFrom dplyr mutate summarise
#' @importFrom purrr pmap_dbl
est_max_efs <- function(fitted_mod, q = 0.95) {
  fitted_mod |>
    get_posterior() |>
    dplyr::mutate(
      q = purrr::pmap_dbl(
        .l = list(
          distr = "tnorm",
          n = lambda,
          par1 = mu,
          par2 = sigma,
          p = q
        ),
        .f = inverse_cdf_x
      )
    ) |>
    dplyr::summarise(
      fit = stats::quantile(q, 0.5),
      lwr = stats::quantile(q, 0.1),
      upr = stats::quantile(q, 0.9)
    )
}
