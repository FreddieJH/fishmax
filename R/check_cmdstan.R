#' Check CmdStan and its toolchain
#'
#' Checks whether CmdStan is installed and the toolchain is working.
#'
#' @return Invisibly returns a list with CmdStan version and toolchain check results.
#' @export
check_cmdstan <- function() {
  cmdstan_installed <- tryCatch(
    {
      !is.null(cmdstanr::cmdstan_version())
    },
    error = function(e) {
      FALSE # Will be FALSE if CmdStan is not installed
    }
  )

  if (!cmdstan_installed) {
    message(
      "CmdStan not found. Please run fishmax::setup_cmdstan()"
    )
  }

  # Check toolchain
  toolchain_ok <- tryCatch(
    {
      suppressMessages(cmdstanr::check_cmdstan_toolchain())
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  if (!toolchain_ok) {
    message(
      "Issues with Cmdstan C++ toolchain Please run fishmax::setup_cmdstan()"
    )
  }

  return(invisible(cmdstanr::cmdstan_version()))
}
