#' Install cmdstanr
#'
#' Installs cmdstanr if not already installed on the machine
#'
#'
#' @export
setup_stan <- function() {
  if (is.null(cmdstanr::cmdstan_version())) {
    message("Installing CmdStan (this may take a few minutes)...")
    cmdstanr::install_cmdstan()
  }
  invisible(TRUE)
}
