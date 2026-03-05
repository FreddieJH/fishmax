#' Check CmdStan and its toolchain
#'
#' Checks whether CmdStan is installed and the toolchain is working.
#'
#' @return Invisibly returns a list with CmdStan version and toolchain check results.
#' @noRd
.check_cmdstan <- function() {
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
      "CmdStan not found. Please run cmdstanr::install_cmdstan()"
    )
  }
  return(invisible(cmdstanr::cmdstan_version()))
}

#' @noRd
.initialise_pars <- function(type, maxima_median) {
  if (type %in% c("evt", "evt_gumbel")) {
    function(chain_id) {
      list(loc = maxima_median, scale = 10, shape = 0)
    }
  } else {
    function(chain_id) {
      list(mu = maxima_median, sigma = 10, lambda = 100)
    }
  }
}


#' @noRd
.find_stanfile <- function(model_type) {
  system.file(
    "stan",
    paste0(ifelse(model_type == "efsmm", "efs", model_type), ".stan"),
    package = "fishmax"
  )
}

#' @noRd
.make_standata <- function(maxima_list) {
  list(
    x = unlist(maxima_list),
    n_obs = length(unlist(maxima_list)),
    n_per_sample = lengths(maxima_list),
    start_idx = cumsum(c(0, lengths(maxima_list)[-length(maxima_list)])) + 1,
    k = length(maxima_list)
  )
}

#' @noRd
.executable_exists <- function(stan_file) {
  exe <- sub("\\.stan$", "", stan_file)

  if (.Platform$OS.type == "windows") {
    exe <- paste0(exe, ".exe")
  }

  file.exists(exe)
}


#' @noRd
.compilation_message <- function(model_file) {
  if (!.executable_exists(model_file)) {
    message(
      paste(
        model_file,
        "model not yet compiled in this machine — compilation may take several minutes..."
      )
    )
  }
}
