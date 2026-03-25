#' Extract posterior samples
#'
#' Extracts posterior samples from fitted models as a named list of tibbles
#'
#' @param fit Named list of CmdStanMCMC objects
#'
#' @return Named list of tibbles containing posterior samples for each model
#' @importFrom posterior as_draws_df
#' @importFrom purrr map
#' @importFrom tibble as_tibble
.get_posterior <- function(fit) {
  .validate_fit(fit)
  fit_slim <- fit[names(fit) != "maxima"]

  fit_slim |>
    purrr::map(\(model_fit) {
      tryCatch(
        as_tibble(posterior::as_draws_df(model_fit)),
        error = \(e) {
          stop(
            sprintf("Failed to extract posterior samples: %s", e$message),
            call. = FALSE
          )
        }
      )
    })
}
