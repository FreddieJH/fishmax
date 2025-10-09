#' Estimate maximum size
#'
#' Estimates maximum size with uncertainty given k-samples
#'
#' @param fit Named list of CmdStanMCMC objects
#' @param ci Credible interval width (default: 0.8 = 80% credible interval)
#' @param k The k-sample LMAX in unit length (e.g., cm) (default: 20)
#'
#' @return Named list of tibbles with maximum length estimates for each model
#' @export
#' @importFrom dplyr mutate summarise
#' @importFrom purrr pmap_dbl map2
get_max <- function(fit, ci = 0.8, k = 20) {
  validate_fit(fit)

  # Validate parameters
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) {
    stop("'ci' must be a single numeric value between 0 and 1", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("'k' must be a single positive numeric value", call. = FALSE)
  }

  fit_slim <- fit[names(fit) != "maxima"]
  posterior_list <- get_posterior(fit_slim)

  output_list <-
    purrr::map(posterior_list, function(posterior_samples) {
      is_evt <- sum(
        c("loc", "scale", "shape") %in% colnames(posterior_samples)
      ) ==
        3
      is_evtg <- sum(
        c("loc", "scale") %in% colnames(posterior_samples)
      ) ==
        2

      posterior_samples |>
        dplyr::mutate(
          pdf = if (is_evt) {
            purrr::pmap_dbl(
              list(p = 1 - (1 / k), loc = loc, scale = scale, shape = shape),
              qgev_v
            )
          } else if (is_evtg) {
            purrr::pmap_dbl(
              list(p = 1 - (1 / k), loc = loc, scale = scale),
              qgumbel_v
            )
          } else {
            purrr::pmap_dbl(
              .l = list(mu, sigma, lambda),
              .f = \(mu, sigma, lambda) {
                cdf <- \(x) {
                  ptnorm(q = x, mean = mu, sd = sigma)
                }
                pdf <- \(x) {
                  dtnorm(x = x, mean = mu, sd = sigma)
                }
                gmax <- \(x) {
                  g(x = x, n = lambda * k, cdf = cdf, pdf = pdf)
                }
                mode_f(gmax)
              }
            )
          }
        ) |>
        dplyr::summarise(
          max_fit = stats::quantile(pdf, 0.5),
          max_lwr = stats::quantile(pdf, (1 - ci) / 2),
          max_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2))
        )
    })
  names(output_list) <- names(fit_slim)
  # output_list[["maxima"]] <- fit[["maxima"]]
  return(
    output_list |>
      dplyr::bind_rows(.id = "model")
  )
}
