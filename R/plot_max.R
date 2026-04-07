#' Plot model comparison
#'
#' Creates publication-ready plots comparing EVT and EFS model fits
#'
#' @param fit Model fit object
#' @param xmin,xmax Bounds of x-axis (Lmax)
#' @param xstep Resolution of x-axis, larger step size = lower resolution = faster to run
#' @param ci Credible interval width (default = 0.8)
#' @param k The k-sample Lmax for estimation. Note that this is not necessarily the number of sample maxima used to fit the model (default: 20)
#' @param text_overlay Set to FALSE to remove the vertical line and text overlay on the plot
#'
#' @return Combined ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_rug scale_x_continuous labs theme_classic layer_scales
#' @importFrom dplyr tibble mutate bind_rows recode_values distinct
plot_max <- function(
  fit,
  xmin = 0,
  xmax = 100,
  xstep = 1,
  ci = 0.8,
  k = 20,
  show_title = FALSE,
  show_lines = TRUE,
  col_pallette = c("#2E86AB", "#9e7948ff", "#C77BA0", "#7e348dff"),
  yaxis_title = "Probability density",
  xaxis_title = "Body size, cm",
  xaxis_title_panelB = expression(paste("Estimated ", L[max], ", cm"))
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

  # validate ci
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) {
    stop("'ci' must be a single numeric value between 0 and 1", call. = FALSE)
  }

  # validate k
  if (!is.numeric(k) || length(k) != 1 || k < 3) {
    stop(
      "'k' must be a single positive numeric value of at least 3",
      call. = FALSE
    )
  }

  pdf_tbl <-
    pdf_list |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      model_label = dplyr::recode_values(
        model,
        "efs" ~ "EFS",
        "evt" ~ "EVT (GEV)",
        "evt_gumbel" ~ "EVT (Gumbel)",
        "efsmm" ~ "EFSmm"
      )
    ) |>
    dplyr::mutate(
      model_colour = dplyr::recode_values(
        model,
        "evt" ~ col_pallette[1],
        "evt_gumbel" ~ col_pallette[2],
        "efs" ~ col_pallette[3],
        "efsmm" ~ col_pallette[4]
      )
    )

  color_map_by_label <- pdf_tbl |>
    dplyr::distinct(model_label, model_colour) |>
    with(setNames(model_colour, model_label))

  color_map_by_model <- pdf_tbl |>
    dplyr::distinct(model, model_colour) |>
    with(setNames(model_colour, model))

  max_table <- get_max(fit_slim, k = k, ci = ci)
  colnames(max_table) <- gsub("\\.[0-9]+%", "", colnames(max_table))

  percentile_percent <- (1 - (1 / k)) * 100
  # efs_underlying <- get_underlying(efs_fit)

  ordinal <- function(x) {
    suffix <- ifelse(
      x %% 100 %in% 11:13,
      "th",
      c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[
        (x %% 10) + 1
      ]
    )
    paste0(x, suffix)
  }

  build_subtitle <- function(models_present, k) {
    lines <- c(
      if ("efs" %in% models_present) {
        glue::glue("EFS distribution = Expected Lmax given {k} samples")
      },

      if ("evt" %in% models_present) {
        glue::glue(
          "EVT (GEV) distribution = {ordinal(round(percentile_percent))} percentile of expected Lmax given 1 sample"
        )
      },

      if ("evt_gumbel" %in% models_present) {
        glue::glue(
          "EVT (Gumbel) distribution = {ordinal(round(percentile_percent))} percentile of expected Lmax given 1 sample"
        )
      },

      if ("efsmm" %in% models_present) {
        glue::glue(
          "EFSmm distribution = Expected Lmax given {k} samples"
        )
      }
    )

    paste(lines, collapse = "\n")
  }

  p_main <-
    pdf_tbl |>
    ggplot2::ggplot(ggplot2::aes(
      size,
      pdf_fit,
      col = model_label,
      fill = model_label
    )) +
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
      # labels = scales::label_number(suffix = "cm"),s
      limits = c(xmin, xmax)
    ) +
    {
      if (show_lines) {
        ggplot2::geom_vline(
          ggplot2::aes(xintercept = max_fit, col = model),
          lty = 2,
          linewidth = 2,
          data = max_table,
          show.legend = FALSE
        )
      }
    } +
    ggplot2::scale_color_manual(values = color_map_by_label) +
    ggplot2::scale_fill_manual(values = color_map_by_label) +
    ggplot2::labs(
      x = xaxis_title,
      y = yaxis_title,
      fill = NULL,
      col = NULL
    ) +
    {
      if (show_title) {
        ggplot2::labs(
          subtitle = build_subtitle(unique(pdf_tbl$model), k)
        )
      }
    } +
    ggplot2::theme_classic(20) +
    ggplot2::theme(
      legend.position = c(0.9, 0.9),
      legend.justification = c(1, 1)
    )

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
    ggplot2::labs(y = NULL, x = xaxis_title_panelB) +
    ggplot2::scale_x_continuous(
      # labels = scales::label_number(suffix = "cm"),
      limits = ggplot2::layer_scales(p_main)$x$range$range
    ) +
    ggplot2::scale_color_manual(values = color_map_by_label) +
    ggplot2::theme_classic(20) +
    ggplot2::theme(
      legend.position = "none"
    )

  output_p <-
    patchwork::wrap_plots(
      p_main,
      p_partb,
      ncol = 1,
      heights = c(5, 1)
    )
  return(output_p)
}
