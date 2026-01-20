#' Validate cmdstanr fit object
#'
#' Internal function to validate CMDstanR fit input
#'
#' @param fit Named list of CmdStanMCMC objects
#' @noRd
validate_fit <- function(fit) {
  valid_names <- c("evt", "evt_gumbel", "efs", "efsmm", "maxima")

  # Check fit is a list
  if (!is.list(fit)) {
    stop("'fit' must be a named list of CmdStanMCMC objects", call. = FALSE)
  }

  # Check fit has names
  if (is.null(names(fit)) || any(names(fit) == "")) {
    stop("'fit' must be a named list", call. = FALSE)
  }

  # Check all names are valid
  invalid_names <- setdiff(names(fit), valid_names)
  if (length(invalid_names) > 0) {
    stop(
      sprintf(
        "Invalid model names: %s. Must be one of: %s",
        paste(invalid_names, collapse = ", "),
        paste(valid_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Check all elements (except maxima) are CmdStanMCMC objects
  is_cmdstan <- vapply(
    fit[names(fit) != "maxima"],
    function(x) {
      inherits(x, "CmdStanMCMC")
    },
    logical(1)
  )

  if (!all(is_cmdstan)) {
    stop("All elements of 'fit' must be CmdStanMCMC objects", call. = FALSE)
  }

  invisible(TRUE)
}
