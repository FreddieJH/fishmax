#' Get the posterior samples of Lmax
#'
#' Extracts posterior samples from fitted models and estimates the lmax given the parameter posteriors
#'
#' @param fit Named list of CmdStanMCMC objects
#'
#' @return Named list of vectors containing posterior samples for each model
#' @export
lmax_posterior <- function(fit, ci = 0.8, k = 20) {
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
          cdf <- function(x) ptnorm(q = x, mean = mu, sd = sigma)
          pdf <- function(x) dtnorm(x = x, mean = mu, sd = sigma)

          gmax <- function(x) {
            max_pdf(x = x, n = lambda * k, cdf = cdf, pdf = pdf)
          }

          mode_f(gmax)
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
