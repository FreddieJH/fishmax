#' One-time setup for CmdStan
#'
#' Checks whether CmdStan is available and installs it if not.
#'
#' @return Invisibly returns the CmdStan version.
#' @export
setup_cmdstan <- function(
  version = NULL,
  cores = max(1L, parallel::detectCores(logical = FALSE))
) {
  # cmdstanR: install if not already
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("Installing CmdStanR from mc-stan.org...")
    tryCatch(
      {
        install.packages(
          'cmdstanr',
          repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
        )
      },
      error = function(e) {
        stop(
          "Failed to install CmdStanR. Please install manually: ",
          "remotes::install_github('stan-dev/cmdstanr')"
        )
      }
    )
  } else {
    message("CmdStanR is already installed.")
  }

  # Step 2: Check if CmdStan installed
  cmdstan_installed <- tryCatch(
    {
      !is.null(cmdstanr::cmdstan_version())
    },
    error = function(e) {
      FALSE # Will be FALSE if CmdStan is not installed
    }
  )

  # Step 3: Install CmdStan if missing
  if (!cmdstan_installed) {
    message(
      "CmdStan not found. Installing now... (note: this may take several minutes)"
    )
    cmdstanr::install_cmdstan(
      version = version,
      cores = cores,
      quiet = FALSE
    )
  } else {
    message(
      "CmdStan already installed (version ",
      cmdstanr::cmdstan_version(),
      ")."
    )
    return(invisible(cmdstanr::cmdstan_version()))
  }

  # If problems sent the user to the mc-stan article
  if (is.null(cmdstanr::cmdstan_version())) {
    stop(
      "CmdStan installation failed.\n",
      "See https://mc-stan.org/cmdstanr/articles/cmdstanr.html",
      call. = FALSE
    )
  }

  # All good!
  message(
    "CmdStan successfully installed (version ",
    cmdstanr::cmdstan_version(),
    ")."
  )

  # invisible return of CmdStan version
  return(invisible(cmdstanr::cmdstan_version()))
}
