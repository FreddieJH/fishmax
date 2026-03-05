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
  .validate_fit(fit)

  posterior_samples <- get_posterior(fit)
  plots <- lapply(
    names(posterior_samples)[names(posterior_samples) != "maxima"],
    function(model_name) {
      ps <- posterior_samples[[model_name]]

      n_params <- ncol(ps) - 3

      long <- ps |>
        tidyr::pivot_longer(
          cols = -c(.chain, .iteration, .draw),
          names_to = "name",
          values_to = "value"
        )

      long$name <- ifelse(
        long$name == "lp__",
        "Log posterior density",
        long$name
      )

      ggplot2::ggplot(
        long,
        ggplot2::aes(
          x = .iteration,
          y = value,
          colour = factor(.chain),
          group = .chain
        )
      ) +
        ggplot2::geom_path(alpha = 0.4) +
        ggplot2::facet_wrap(
          ~name,
          scales = "free",
          ncol = ceiling(sqrt(n_params))
        ) +
        ggplot2::labs(
          title = paste(
            "Traceplot for",
            model_name,
            "model (n = ",
            n_params - 1,
            "parameters)"
          ),
          x = "Iteration",
          y = "Parameter value"
        ) +
        ggplot2::theme_classic(20) +
        ggplot2::theme(legend.position = "none")
    }
  )
  return(plots)
}
