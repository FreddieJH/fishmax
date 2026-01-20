test_that("model fitting outputs a list, including the input vector (maxima values)", {
  set.seed(123)
  model_fit <- fit_max_model(c(40, 41, 35, 38))
  expect_equal(class(model_fit), "list")
  expect_equal(length(model_fit), 4)
  expect_equal(model_fit$maxima, c(40, 41, 35, 38))
})

test_that("model fitting requires numeric values only", {
  expect_error(fit_max_model(c(40, "50")))
})

test_that("Fits models individually", {
  set.seed(123)
  model_fit_efs <- fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1,
    model_type = "efs"
  )
  model_fit_evt <- fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1,
    model_type = "evt"
  )
  model_fit_evtg <- fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1,
    model_type = "evt_gumbel"
  )
  expect_equal(length(model_fit_efs), 2)
  expect_equal(length(model_fit_evt), 2)
  expect_equal(length(model_fit_evtg), 2)
})

test_that("EFSMM only works on lists, where at least one element of list of of length > 1", {
  set.seed(123)
  expect_error(fit_max_model(
    c(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1,
    model_type = "efsmm"
  ))
  expect_error(fit_max_model(
    list(40, 41, 35, 38),
    iter_warmup = 200,
    iter_sampling = 200,
    chains = 1,
    model_type = "efsmm"
  ))
})

test_that("EFSMM only works on lists", {
  set.seed(123)
  expect_equal(
    class(fit_max_model(
      list(40, 41, 35, c(39, 38)),
      iter_warmup = 200,
      iter_sampling = 200,
      chains = 1,
      model_type = "efsmm"
    )),
    "list"
  )
})
