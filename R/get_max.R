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
  .validate_fit(fit)
  .validate_ci(ci)
  .validate_k(k)

  fit_slim <- fit[names(fit) != "maxima"]
  posterior_list <- max_posterior(fit = fit_slim, ci = ci, k = k)
  posterior_summary_list <- lapply(posterior_list, function(pdf) {
    c(
      max_fit = stats::quantile(pdf, 0.5),
      max_lwr = stats::quantile(pdf, (1 - ci) / 2),
      max_upr = stats::quantile(pdf, 1 - ((1 - ci) / 2))
    )
  })

  names(posterior_summary_list) <- names(fit_slim)

  out <- do.call(
    rbind,
    lapply(seq_along(posterior_summary_list), function(i) {
      cbind(
        model = names(posterior_summary_list)[i],
        as.data.frame(t(posterior_summary_list[[i]]))
      )
    })
  )
  out$model <- c(
    efs = "EFS",
    evt = "EVT (GEV)",
    evt_gumbel = "EVT (Gumbel)",
    efsmm = "EFSmm"
  )[out$model]

  return(out)
}
