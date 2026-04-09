test_that("model fit shown", {
  testthat::skip_if_not_installed("cmdstanr")

  if (cmdstanr::cmdstan_version(NULL) == "") {
    testthat::skip("CmdStan not installed")
  }
  set.seed(123)
  model_fit <- fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1
  )
  p <- plot_max(model_fit)

  expect_true(
    "ggplot" %in% class(p)
  )
})
