test_that("traceplots shown", {
  testthat::skip_if_not_installed("cmdstanr")

  cmdstan_installed <- tryCatch(
    {
      !is.null(cmdstanr::cmdstan_version())
    },
    error = function(e) {
      FALSE # Will be FALSE if CmdStan is not installed
    }
  )

  if (!cmdstan_installed) {
    testthat::skip("CmdStan not installed")
  }
  set.seed(123)
  model_fit <- fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1
  )
  expect_equal(class(plot_traceplot(model_fit)), "list")
  expect_true("ggplot" %in% class(plot_traceplot(model_fit)[[1]]))
})
