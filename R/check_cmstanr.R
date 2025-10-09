check_cmdstan <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      "Package 'cmdstanr' is required. Install with: 
         install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"
    )
  }

  if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    stop("CmdStan is not installed. Install with: cmdstanr::install_cmdstan()")
  }
}
