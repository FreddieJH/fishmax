#' Create trace plot for MCMC diagnostics
#'
#' Creates trace plots for model parameters to assess MCMC convergence
#'
#' @param fit Fitted model object
#'
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_path facet_wrap theme_classic theme
#' @importFrom tidyr pivot_longer
traceplot <- function(fit) {
  posterior_samples <- get_posterior(fit)
  n_params <- ncol(posterior_samples) - 3 # chain, iteration and draw cols

  posterior_samples |>
    tidyr::pivot_longer(cols = -c(.chain, .iteration, .draw)) |>
    ggplot2::ggplot(ggplot2::aes(
      .iteration,
      value,
      colour = as.factor(.chain),
      group = .chain
    )) +
    ggplot2::geom_path(alpha = 0.4) +
    ggplot2::facet_wrap(
      ~name,
      scales = "free",
      ncol = ceiling(sqrt(n_params))
    ) +
    ggplot2::theme_classic(20) +
    ggplot2::theme(legend.position = "none")
}
