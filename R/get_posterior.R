#' Extract posterior samples
#'
#' Extracts posterior samples from fitted models as a named list of tibbles
#'
#' @param fit Named list of CmdStanMCMC objects
#'
#' @return Named list of tibbles containing posterior samples for each model
#' @export
#' @importFrom posterior as_draws_df
#' @importFrom dplyr as_tibble
#' @importFrom purrr map
get_posterior <- function(fit) {
  fit_slim <- fit[names(fit) != "maxima"]
  validate_fit(fit_slim)

  # Extract posteriors for all models
  output_list <-
    purrr::map(fit_slim, \(model_fit) {
      tryCatch(
        {
          posterior::as_draws_df(model_fit) |>
            dplyr::as_tibble()
        },
        error = function(e) {
          stop(
            sprintf("Failed to extract posterior samples: %s", e$message),
            call. = FALSE
          )
        }
      )
    })
  names(output_list) <- names(fit_slim)
  output_list[["maxima"]] <- fit[["maxima"]]
  return(output_list)
}
