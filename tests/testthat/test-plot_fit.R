test_that("model fit shown", {
  set.seed(123)
  model_fit <- fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1
  )
  p <- plot_fit(model_fit)

  expect_true(
    "ggplot" %in% class(p)
  )
})
