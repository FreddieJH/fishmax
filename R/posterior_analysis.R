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
#' Estimates maximum , with uncertainty, size given k-samples
#'
#' @param fit Fitted model object
#' @param ci Credible interval width (default: 0.8)
#' @param k The k-sample LMAX. By default this is set to 20, for ease of comparison between studies and species. Note that this is not the number of sample maxima used to fit the model. (default: 20)
#'
#' @return Tibble of maximum length, lower and upper confidence intervals
#' @export
#' @importFrom dplyr mutate summarise
#' @importFrom purrr pmap_dbl
est_max <- function(fit, ci = 0.8, k = 20) {
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
      max_fit = stats::quantile(pdf, 0.5),
      max_lwr = stats::quantile(pdf, (1 - ci) / 2),
      max_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2))
    )
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
