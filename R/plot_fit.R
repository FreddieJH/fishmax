#' Plot model comparison
#'
#' Creates publication-ready plots comparing EVT and EFS model fits
#'
#' @param fit Model fit object
#' @param xmin,xmax Range bounds
#' @param xstep Step size
#' @param ci Credible interval width (default = 0.8)
#' @param k The k-sample LMAX for estimation. Note that this is not mecesarily the number of sample maxima used to fit the model (default: 20)
#'
#' @return Combined ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_rug scale_x_continuous labs theme_classic
#' @importFrom scales label_number
#' @importFrom dplyr tibble
plot_fit <- function(
  fit,
  xmin = 0,
  xmax = 100,
  xstep = 1,
  ci = 0.8,
  k = 20,
  col_pallette = c("#2E86AB", "#C77BA0", "#9e7948ff", "#7e348dff")
) {
  maxima_vals <- unlist(lapply(fit[["maxima"]], FUN = max))
  # k <- length(maxima_vals)
  fit_slim <- fit[names(fit) != "maxima"]

  pdf_list <- get_pdf(
    fit_slim,
    xmin = xmin,
    xmax = xmax,
    xstep = xstep,
    k = k,
    ci = ci
  )

  pdf_tbl <-
    pdf_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      model = dplyr::case_match(
        model,
        "efs" ~ "EFS",
        "evt" ~ "EVT (GEV)",
        "evt_gumbel" ~ "EVT (Gumbel)",
        "efsmm" ~ "EFSmm"
      )
    )

  # efs_underlying <- get_underlying(efs_fit)

  p_main <-
    pdf_tbl |>
    ggplot2::ggplot(ggplot2::aes(size, pdf_fit, col = model, fill = model)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pdf_lwr, ymax = pdf_upr),
      col = "transparent",
      alpha = 0.3
    ) +
    ggplot2::geom_line(linewidth = 2) +
    ggplot2::geom_rug(
      ggplot2::aes(x = x),
      data = dplyr::tibble(x = maxima_vals),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(suffix = "cm"),
      limits = c(xmin, xmax)
    ) +
    ggplot2::scale_color_manual(values = col_pallette) +
    ggplot2::scale_fill_manual(values = col_pallette) +
    ggplot2::labs(
      x = "Body size",
      y = "Probability density",
      fill = NULL,
      col = NULL
    ) +
    ggplot2::theme_classic(20) +
    ggplot2::theme(
      legend.position = c(0.9, 0.9),
      legend.justification = c(1, 1)
    )

  max_table <- get_lmax(fit_slim, k = k)
  colnames(max_table) <- gsub("\\.[0-9]+%", "", colnames(max_table))

  p_partb <-
    max_table |>
    ggplot2::ggplot() +
    ggplot2::aes(x = max_fit, y = model, col = model) +
    ggplot2::geom_errorbar(
      orientation = "y",
      ggplot2::aes(xmin = max_lwr, xmax = max_upr),
      width = 0
    ) +
    ggplot2::geom_point(size = 5) +
    ggplot2::labs(y = NULL, x = expression(paste("Estimated ", L[max]))) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(suffix = "cm"),
      limits = ggplot2::layer_scales(p_main)$x$range$range
    ) +
    ggplot2::scale_color_manual(values = col_pallette) +
    ggplot2::theme_classic(20) +
    ggplot2::theme(legend.position = "none")

  output_p <-
    patchwork::wrap_plots(
      p_main,
      p_partb,
      ncol = 1,
      heights = c(5, 1)
    )
  return(output_p)
}
