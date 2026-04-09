#' @noRd
.validate_fit <- function(fit) {
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
}

#' @noRd
.validate_ci <- function(ci) {
  if (!is.numeric(ci) || length(ci) != 1 || ci <= 0 || ci >= 1) {
    stop("'ci' must be a single numeric value between 0 and 1", call. = FALSE)
  }
}


#' @noRd
.validate_k <- function(k) {
  if (!is.numeric(k) || length(k) != 1 || k < 3) {
    stop(
      "'k' must be a single positive numeric value of at least 3",
      call. = FALSE
    )
  }
}


#' @noRd
.validate_maxima <- function(length_maxima) {
  x <- unique(length_maxima)
  is_numeric_vec <- is.numeric(x) &&
    all(is.finite(x)) &&
    !anyNA(x) &&
    length(x) >= 3

  is_list_numeric <- is.list(x) &&
    length(x) >= 3 &&
    all(vapply(x, is.numeric, logical(1)))

  if (!is_numeric_vec && !is_list_numeric) {
    stop(
      "`length_maxima` must be either:\n",
      "  - a numeric vector (finite, no missing values), or\n",
      "  - a list of numeric vectors\n",
      "with at least three unique values.",
      call. = FALSE
    )
  }

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
}

.remove_efsmm_ifnotlist <- function(length_maxima, model_type) {
  compatible_models <- if (is.list(length_maxima)) {
    model_type
  } else {
    setdiff(model_type, "efsmm")
  }

  # if (missing(model_type)) {
  #   model_type <- compatible_models
  # } else {
  #   model_type <- match.arg(model_type, all_models, several.ok = TRUE)
  #   if ("efsmm" %in% model_type && !is.list(length_maxima)) {
  #     stop("`efsmm` requires `length_maxima` to be a list.", call. = FALSE)
  #   }
  #   model_type <- intersect(model_type, compatible_models)
  # }

  compatible_models
}

#' @noRd
.check_if_list <- function(length_maxima) {
  if (!is.list(length_maxima)) {
    stop(
      "model_type 'efsmm' requires length_maxima to be a list",
      call. = FALSE
    )
  }
}


#' @noRd
.validate_modelname <- function(model_type) {
  all_models <- c("evt", "evt_gumbel", "efs", "efsmm")

  invalid <- setdiff(model_type, all_models)
  if (length(invalid) > 0) {
    stop(
      "Invalid model_type: ",
      paste(invalid, collapse = ", "),
      ". Must be one of: ",
      paste(all_models, collapse = ", "),
      call. = FALSE
    )
  }
}

#' @noRd
.validate_chains <- function(chains) {
  if (
    !is.numeric(chains) || length(chains) != 1 || chains < 1 || chains %% 1 != 0
  ) {
    stop("`chains` must be a single integer >= 1.", call. = FALSE)
  }
}

#' @noRd
.validate_iterations <- function(iterations) {
  if (
    !is.numeric(iterations) ||
      length(iterations) != 1 ||
      iterations < 100 ||
      iterations %% 1 != 0
  ) {
    stop(
      "Number of iterations (both for sampling and warmup) must be a single integer >= 100.",
      call. = FALSE
    )
  }
}


.validate_stanfile <- function(stan_filename, model_type) {
  if (!file.exists(stan_filename) || stan_filename == "") {
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
}
