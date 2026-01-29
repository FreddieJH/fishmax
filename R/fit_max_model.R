#' Fit LMAX model to a set of length maxima
#'
#' Fits a Bayesian model to a set of length maxima, using one of two approaches:
#' Extreme Value Theory (EVT) or Exact-Finite Sampling (EFS).
#' For EVT, either the Generalised Extreme Value (GEV, three parameters) or
#' Gumbel (two parameters) distribution can be selected.
#' For EFS, data can be in the form of either a single maximum per sample (EFS),
#' or multiple maxima per sample (EFSMM).
#'
#' @param length_maxima A numeric vector or list of numeric vectors of length maxima.
#'   If a vector is provided, these will be treated as individual sample maxima,
#'   where the length of the vector equals the number of samples. For the multiple
#'   maxima approach (EFSMM), provide a list of vectors, with the length of the list
#'   equalling the number of samples, and the length of each vector within the list
#'   representing the number of maxima available per sample.
#' @param model_type Character vector specifying which model(s) to fit: "evt",
#'   "evt_gumbel", "efs", or "efsmm". If not specified, all available models will
#'   be fitted (default: all models).
#' @param chains Integer. Number of chains for MCMC sampling (default: 4).
#' @param iter_warmup Integer. Number of warmup iterations (default: 2000).
#' @param iter_sampling Integer. Number of sampling iterations (default: 1000).
#' @param adapt_delta Numeric. cmdstanR argument, see cmdstanR documentation (default: 0.999).
#' @param max_treedepth Integer. cmdstanR argument, see cmdstanR documentation (default: 12).
#'
#' @return If a single model is fitted, returns a CmdStanMCMC object. If multiple
#'   models are fitted, returns a named list of CmdStanMCMC objects.
#'
#' @importFrom checkmate assert assert_numeric assert_list assert_int test_numeric test_list
#' @export
#'
#' @examples
#' # Single maximum per sample
#' maxima_vec <- c(45.2, 52.1, 48.7, 51.3)
#' fit_max_model(maxima_vec, model_type = "efs")
#'
#' # Multiple maxima per sample
#' maxima_list <- list(c(45.2, 44.1), c(52.1), c(48.7, 47.3, 46.8))
#' fit_max_model(maxima_list, model_type = "efsmm")
#'
#' # Fit all models
#' fit_max_model(maxima_vec)
fit_max_model <- function(
  length_maxima,
  model_type = c("evt", "evt_gumbel", "efs", "efsmm"),
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 1000,
  adapt_delta = 0.999,
  max_treedepth = 12,
  refresh = 1000
) {
  check_cmdstan()
  # Input validation
  checkmate::assert(
    checkmate::test_numeric(
      length_maxima,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1
    ),
    checkmate::test_list(length_maxima, types = "numeric", min.len = 1),
    combine = "or",
    .var.name = "length_maxima"
  )

  # If list, check at least one vector has length > 1 (for EFSMM)
  if (is.list(length_maxima)) {
    if (!any(lengths(length_maxima) > 1)) {
      stop(
        "When length_maxima is a list, at least one vector must have length > 1",
        call. = FALSE
      )
    }
    # Check all elements are numeric and finite
    if (
      !all(vapply(
        length_maxima,
        function(x) {
          is.numeric(x) && all(is.finite(x)) && !anyNA(x)
        },
        logical(1)
      ))
    ) {
      stop(
        "All elements in length_maxima list must be numeric, finite, and non-missing",
        call. = FALSE
      )
    }
  }

  # Validate model_type selection
  all_models <- c("evt", "evt_gumbel", "efs", "efsmm")
  if (missing(model_type)) {
    if (is.list(length_maxima)) {
      model_type <- all_models
    } else {
      model_type <- all_models[which(all_models != "efsmm")]
    }
  } else {
    model_type <- match.arg(model_type, several.ok = TRUE)
  }

  # Check EFSMM only used with list input
  if ("efsmm" %in% model_type && !is.list(length_maxima)) {
    stop(
      "model_type 'efsmm' requires length_maxima to be a list",
      call. = FALSE
    )
  }

  # # Check non-EFSMM models not used with multiple values per sample
  # if (is.list(length_maxima) && any(lengths(length_maxima) > 1)) {
  #   invalid_models <- setdiff(model_type, "efsmm")
  #   if (length(invalid_models) > 0) {
  #     stop(
  #       "Models ",
  #       paste(invalid_models, collapse = ", "),
  #       " cannot handle multiple values per sample. Use 'efsmm' only.",
  #       call. = FALSE
  #     )
  #   }
  # }

  checkmate::assert_int(chains, lower = 1)
  checkmate::assert_int(iter_warmup, lower = 100)
  checkmate::assert_int(iter_sampling, lower = 100)

  # Convert to list format
  maxima_list <- if (is.list(length_maxima)) {
    length_maxima
  } else {
    as.list(length_maxima)
  }

  # Fit each model
  fits <- lapply(model_type, function(mtype) {
    fit_single_model(
      maxima_list = maxima_list,
      model_type = mtype,
      chains = chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      refresh = refresh
    )
  })

  names(fits) <- model_type
  fits[["maxima"]] <- length_maxima
  return(fits)
}

#' Fit a single model type
#' @noRd
fit_single_model <- function(
  maxima_list,
  model_type,
  chains,
  iter_warmup,
  iter_sampling,
  adapt_delta,
  max_treedepth,
  refresh
) {
  if (model_type != "efsmm" & is.list(maxima_list)) {
    maxima_list <- unlist(lapply(maxima_list, FUN = max))
  }
  mod_dat <- list(
    x = unlist(maxima_list),
    n_obs = length(unlist(maxima_list)),
    n_per_sample = lengths(maxima_list),
    start_idx = cumsum(c(0, lengths(maxima_list)[-length(maxima_list)])) + 1,
    k = length(maxima_list)
  )

  init_func <- function(type, maxima_median) {
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

  model_file <- system.file(
    "stan",
    paste0(ifelse(model_type == "efsmm", "efs", model_type), ".stan"),
    package = "fishmax"
  )

  if (!file.exists(model_file) || model_file == "") {
    stop(
      "Stan model file not found. Available files: ",
      paste(
        list.files(system.file("stan", package = "fishmax")),
        collapse = ", "
      ),
      "\nLooking for: ",
      paste0(ifelse(model_type == "efsmm", "efs", model_type), ".stan"),
      call. = FALSE
    )
  }
  executable_exists <- function(stan_file) {
    exe <- sub("\\.stan$", "", stan_file)

    if (.Platform$OS.type == "windows") {
      exe <- paste0(exe, ".exe")
    }

    file.exists(exe)
  }

  cat("Fitting ", model_type, " model...\n")
  if (!executable_exists(model_file)) {
    message(
      paste(
        model_type,
        "model not yet compiled in this machine — compilation may take several minutes..."
      )
    )
  }
  mod <- cmdstanr::cmdstan_model(model_file)

  fit <- mod$sample(
    data = mod_dat,
    chains = chains,
    init = init_func(model_type, median(unlist(maxima_list))),
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )

  return(fit)
}
