#' Get probability density function of the sample maxima
#'
#' Computes PDF across a range of values
#'
#' @param fit Named list of fitted model objects.
#'   Names should be one or more of: "evt", "evt_gumbel", "efs", "efsmm"
#' @param xmin,xmax Range bounds
#' @param xstep Step size
#' @param ci Credible interval width (default = 0.8)
#' @param k Integer. Number of samples to be used in the estimation of the EFS PDF (default = 20)
#'
#' @return Named list of tibbles with size and PDF estimates for each model.
#' @export
get_pdf <- function(fit, xmin = 0, xmax = 300, xstep = 1, ci = 0.8, k = 20) {
  fit_slim <- fit[names(fit) != "maxima"]
  posterior_list <- get_posterior(fit_slim)
  sizes <- seq(xmin, xmax, xstep)

  output_list <- lapply(
    names(fit_slim),
    function(model_name) {
      result <- compute_pdf_single(
        posterior_list[[model_name]],
        sizes,
        ci,
        k
      )
      cbind(model = model_name, result, stringsAsFactors = FALSE)
    }
  )
  names(output_list) <- names(fit_slim)
  output_list[["maxima"]] <- fit[["maxima"]]
  return(output_list)
}

#' Compute PDF for a single fitted model
#' @noRd
compute_pdf_single <- function(posterior_samples, sizes, ci, k) {
  col_names <- colnames(posterior_samples)
  is_evt <- all(c("loc", "scale", "shape") %in% col_names)
  is_evtg <- all(c("loc", "scale") %in% col_names) && !("shape" %in% col_names)

  # Compute PDF for each size value
  purrr::map_dfr(
    sizes,
    \(x) {
      pdf_samples <- if (is_evt) {
        dgev_v(
          x = x,
          loc = posterior_samples$loc,
          scale = posterior_samples$scale,
          shape = posterior_samples$shape
        )
      } else if (is_evtg) {
        dgumbel_v(
          x = x,
          loc = posterior_samples$loc,
          scale = posterior_samples$scale
        )
      } else {
        # EFS/EFSMM models
        vapply(
          seq_len(nrow(posterior_samples)),
          \(i) {
            cdf <- \(y) {
              ptnorm(
                q = y,
                mean = posterior_samples$mu[i],
                sd = posterior_samples$sigma[i]
              )
            }
            pdf <- \(y) {
              dtnorm(
                x = y,
                mean = posterior_samples$mu[i],
                sd = posterior_samples$sigma[i]
              )
            }
            g(x = x, n = posterior_samples$lambda[i] * k, cdf = cdf, pdf = pdf)
          },
          numeric(1)
        )
      }

      tibble::tibble(
        size = x,
        pdf_fit = stats::quantile(pdf_samples, 0.5),
        pdf_lwr = stats::quantile(pdf_samples, (1 - ci) / 2),
        pdf_upr = stats::quantile(pdf_samples, 1 - ((1 - ci) / 2))
      )
    }
  )
}
