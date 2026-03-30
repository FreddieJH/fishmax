#' Fit max model to a set of length maxima
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
#' @param ... further arguments passed to the cmdstanr sample function
#'
#' @return If a single model is fitted, returns a CmdStanMCMC object. If multiple
#'   models are fitted, returns a named list of CmdStanMCMC objects.
#'
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
  refresh = 1000,
  ...
) {
  .check_cmdstan()
  .validate_maxima(length_maxima)
  .validate_modelname(model_type)

  .validate_chains(chains)
  .validate_iterations(iter_warmup)
  .validate_iterations(iter_sampling)

  model_type <- .remove_efsmm_ifnotlist(length_maxima, model_type)

  if (length(model_type) < 1) {
    stop("Please provide at least one suitable model type")
  }
  fits <-
    lapply(model_type, function(mtype) {
      .fit_single_model(
        maxima_list = as.list(length_maxima),
        model_type = mtype,
        chains = chains,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth,
        refresh = refresh,
        ...
      )
    })

  names(fits) <- model_type
  fits[["maxima"]] <- length_maxima
  return(fits)
}

#' Fit a single model type
#' @noRd
.fit_single_model <- function(
  maxima_list,
  model_type,
  chains,
  iter_warmup,
  iter_sampling,
  adapt_delta,
  max_treedepth,
  refresh,
  ...
) {
  if (model_type == "efsmm") {
    .check_if_list(maxima_list)
  } else {
    # if not efsmm
    if (is.list(maxima_list)) {
      maxima_list <- unlist(lapply(maxima_list, FUN = max))
    }
  }

  model_file <- .find_stanfile(model_type)
  .validate_stanfile(model_file)
  .validate_maxima(maxima_list)

  cat("Fitting ", model_type, " model...\n")
  .compilation_message(model_file)

  mod <- cmdstanr::cmdstan_model(model_file)
  fit <- mod$sample(
    data = .make_standata(maxima_list),
    chains = chains,
    init = .initialise_pars(model_type, median(unlist(maxima_list))),
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh,
    ...
  )

  return(fit)
}
