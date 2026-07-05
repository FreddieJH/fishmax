test_that("estimates consitent max value", {
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
  model_fit <- fit_max_model(c(40, 41, 35, 38))
  max_estimates <- get_max(model_fit)
  expect_equal(class(max_estimates), c("data.frame"))
  expect_equal(nrow(max_estimates), 3)
  expect_equal(as.numeric(round(max_estimates$max_fit[1])), 45)
  expect_equal(as.numeric(round(max_estimates$max_fit[2])), 46)
  expect_equal(as.numeric(round(max_estimates$max_fit[3])), 43)
})
