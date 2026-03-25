#' Get the posterior samples of Lmax
#'
#' Extracts posterior samples from fitted models and estimates the lmax given the parameter posteriors
#'
#' @param fit Named list of CmdStanMCMC objects
#' @param ci Credible interval width (default = 0.8)
#' @param k Integer. Number of samples to be used in the estimation of the EFS PDF (default = 20)
#' @param upper_boundary Upper search range for the maximum. Default is 500, but increase if expecting larger values.
#'
#' @importFrom truncnorm dtruncnorm ptruncnorm
#' @return Named list of vectors containing posterior samples for each model
#' @export
max_posterior <- function(fit, ci = 0.8, k = 20, upper_boundary = 500) {
  .validate_fit(fit)
  .validate_ci(ci)
  .validate_k(k)

  fit_slim <- fit[names(fit) != "maxima"]
  posterior_list <- .get_posterior(fit_slim)
  output_list <- lapply(posterior_list, function(ps) {
    cn <- colnames(ps)

    type <- if (all(c("loc", "scale", "shape") %in% cn)) {
      "evt"
    } else if (all(c("loc", "scale") %in% cn)) {
      "gumbel"
    } else {
      "tnorm"
    }

    pdf <- switch(
      type,
      evt = mapply(
        qgev_v,
        p = 1 - (1 / k),
        loc = ps$loc,
        scale = ps$scale,
        shape = ps$shape
      ),
      gumbel = mapply(
        qgumbel_v,
        p = 1 - (1 / k),
        loc = ps$loc,
        scale = ps$scale
      ),
      tnorm = mapply(
        function(mu, sigma, lambda) {
          cdf <- function(x) {
            truncnorm::ptruncnorm(q = x, mean = mu, sd = sigma, a = 0)
          }
          pdf <- function(x) {
            truncnorm::dtruncnorm(x = x, mean = mu, sd = sigma, a = 0)
          }

          gmax <- function(x) {
            max_pdf(x = x, n = lambda * k, cdf = cdf, pdf = pdf)
          }

          mode_f(gmax, upr = upper_boundary)
        },
        ps$mu,
        ps$sigma,
        ps$lambda
      )
    )
  })
  return(output_list)
  # names(output_list) <- names(fit_slim)
  # # output_list[["maxima"]] <- fit[["maxima"]]
  # out <- do.call(
  #   rbind,
  #   lapply(seq_along(output_list), function(i) {
  #     cbind(
  #       model = names(output_list)[i],
  #       as.data.frame(t(output_list[[i]]))
  #     )
  #   })
  # ) |>
  #   dplyr::mutate(
  #     model = dplyr::case_match(
  #       model,
  #       "efs" ~ "EFS",
  #       "evt" ~ "EVT (GEV)",
  #       "evt_gumbel" ~ "EVT (Gumbel)",
  #       "efsmm" ~ "EFSmm"
  #     )
  #   )

  # return(out)
}
