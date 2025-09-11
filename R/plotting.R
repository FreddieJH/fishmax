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
  n_params <- ncol(posterior_samples) - 3

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

#' Plot model comparison
#'
#' Creates publication-ready plots comparing EVT and EFS model fits
#'
#' @param evt_fit EVT model fit
#' @param efs_fit EFS model fit
#' @param data_vector Vector of observed maxima
#' @param species_name Scientific name for plot annotation
#' @param image_path Optional path to species image
#'
#' @return Combined ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_rug scale_x_continuous labs theme_classic
#' @importFrom scales label_number
#' @importFrom dplyr tibble
plot_model_comparison <- function(
  evt_fit,
  efs_fit,
  data_vector,
  species_name = NULL,
  image_path = NULL
) {
  evt_pdf <- get_pdf(evt_fit)
  efs_pdf <- get_pdf(efs_fit)
  efs_underlying <- get_underlying(efs_fit)

  # Define colours
  evt_colour <- "#2E86AB"
  efs_colour <- "#C77BA0"
  underlying_colour <- "#A23B72"

  p_main <- evt_pdf |>
    ggplot2::ggplot(ggplot2::aes(size, pdf_fit)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pdf_lwr, ymax = pdf_upr),
      alpha = 0.2,
      fill = underlying_colour,
      data = efs_underlying
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = pdf_fit),
      col = underlying_colour,
      lty = 2,
      data = efs_underlying,
      alpha = 0.3
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pdf_lwr, ymax = pdf_upr),
      alpha = 0.5,
      fill = "#7DB3D3"
    ) +
    ggplot2::geom_line(col = evt_colour, linewidth = 2) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pdf_lwr, ymax = pdf_upr),
      alpha = 0.5,
      data = efs_pdf,
      fill = efs_colour
    ) +
    ggplot2::geom_line(col = efs_colour, data = efs_pdf, linewidth = 2) +
    ggplot2::geom_rug(
      ggplot2::aes(x = x),
      data = dplyr::tibble(x = data_vector),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(suffix = "cm"),
      limits = c(0, 200)
    ) +
    ggplot2::labs(x = "Body size", y = "Probability density") +
    ggplot2::theme_classic(20)

  p_main
}
