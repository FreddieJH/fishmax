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
get_lmax <- function(fit, ci = 0.8, k = 20) {
  validate_fit(fit)

  # Validate parameters
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) {
    stop("'ci' must be a single numeric value between 0 and 1", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("'k' must be a single positive numeric value", call. = FALSE)
  }

  fit_slim <- fit[names(fit) != "maxima"]

  posterior_list <- lmax_posterior(fit = fit, ci = ci, k = k)
  output_list <- lapply(posterior_list, function(pdf) {
    c(
      max_fit = stats::quantile(pdf, 0.5),
      max_lwr = stats::quantile(pdf, (1 - ci) / 2),
      max_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2))
    )
  })

  names(output_list) <- names(fit_slim)
  # output_list[["maxima"]] <- fit[["maxima"]]
  out <- do.call(
    rbind,
    lapply(seq_along(output_list), function(i) {
      cbind(
        model = names(output_list)[i],
        as.data.frame(t(output_list[[i]]))
      )
    })
  ) |>
    dplyr::mutate(
      model = dplyr::case_match(
        model,
        "efs" ~ "EFS",
        "evt" ~ "EVT (GEV)",
        "evt_gumbel" ~ "EVT (Gumbel)",
        "efsmm" ~ "EFSmm"
      )
    )

  return(out)
}
