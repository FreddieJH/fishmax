#' Fit extreme value models
#'
#' Fits either EVT (Extreme Value Theory) or EFS (Extreme Fish Size) models
#' to marine biological data using Bayesian methods
#'
#' @param maxima Vector or list of maximum values
#' @param model_type Character: "evt", "efs", or "efsmult"
#' @param chains Number of MCMC chains (default: 4)
#' @param iter_warmup Number of warmup iterations (default: 9000)
#' @param iter_sampling Number of sampling iterations (default: 1000)
#'
#' @return CmdStanMCMC object containing fitted model
#' @export
#' @importFrom cmdstanr cmdstan_model
fit_mod <- function(
  maxima,
  model_type = c("evt", "efs", "efsmult"),
  chains = 4,
  iter_warmup = 9000,
  iter_sampling = 1000
) {
  # Auto-install cmdstanr if not available
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("Installing cmdstanr...")
    install.packages(
      "cmdstanr",
      repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
    )

    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      stop("Failed to install cmdstanr")
    }

    # Install CmdStan
    message("Installing CmdStan...")
    cmdstanr::install_cmdstan()
  }

  model_type <- match.arg(model_type)
  maxima_list <- as.list(maxima)

  mod_dat <- list(
    x = unlist(maxima_list),
    n_obs = length(unlist(maxima_list)),
    n_per_sample = lengths(maxima_list),
    start_idx = cumsum(lengths(maxima_list)),
    k = length(maxima_list)
  )

  init_func <- function(type, maxima_median) {
    if (type == "evt") {
      function(chain_id) {
        list(loc = maxima_median, scale = 10, shape = 0)
      }
    } else {
      function(chain_id) {
        list(mu = maxima_median, sigma = 10, lambda = 100)
      }
    }
  }

  model_file <- system.file(
    "stan",
    paste0(ifelse(model_type == "efsmult", "efs", model_type), ".stan"),
    package = ""
  )

  if (!file.exists(model_file) || model_file == "") {
    stop(
      "Stan model file not found. Available files: ",
      paste(
        list.files(system.file("stan", package = "fishEVA")),
        collapse = ", "
      ),
      "\nLooking for: ",
      paste0(ifelse(model_type == "efsmult", "efs", model_type), ".stan")
    )
  }

  mod <- cmdstanr::cmdstan_model(model_file)

  fit <- mod$sample(
    data = mod_dat,
    chains = chains,
    init = init_func(model_type, median(unlist(maxima))),
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling
  )

  fit
}
