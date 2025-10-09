#' Create traceplot for MCMC diagnostics
#'
#' Creates traceplots for model parameters to assess MCMC convergence
#'
#' @param fit Fitted model object
#'
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_path facet_wrap theme_classic theme
#' @importFrom tidyr pivot_longer
plot_traceplot <- function(fit) {
  posterior_samples <- get_posterior(fit)

  purrr::map(
    names(posterior_samples)[names(posterior_samples) != "maxima"],
    function(model_name) {
      n_params <- ncol(posterior_samples[[model_name]]) - 3

      posterior_samples[[model_name]] |>
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
        ggplot2::labs(title = model_name) +
        ggplot2::theme_classic(20) +
        ggplot2::theme(legend.position = "none")
    }
  )
}
