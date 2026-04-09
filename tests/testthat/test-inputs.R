test_that("Input validations", {
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
  expect_error(fit_max_model(c(10, 12))) # too few k
  expect_error(fit_max_model(c(10, 12, 12))) # too few unique k
  expect_error(fit_max_model(list(10, c(11, 12)))) # too few k (multiple)
  expect_error(fit_max_model(list(10, c(11, 12), 10))) # too few unique k (multiple)
})
