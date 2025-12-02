test_that("fit_cox_weighted works correctly", {
  skip_on_cran()

  # Dummy data
  set.seed(1)
  n <- 50
  df <- data.frame(
    time   = rexp(n, 0.1),
    status = rbinom(n, 1, 0.6),
    age    = rnorm(n, 50, 10),
    trt    = sample(c(0, 1), n, TRUE)
  )

  w <- runif(n, 0.5, 2)  # random weights

  # Run model
  fit <- fit_cox_weighted(
    data       = df,
    time       = "time",
    status     = "status",
    covariates = c("age", "trt"),
    weights    = w
  )

  # Tests
  expect_s3_class(fit, "coxph")
  expect_true(!is.null(fit$coefficients))
  expect_equal(length(fit$coefficients), 2)
  expect_true(all(names(fit$coefficients) == c("age", "trt")))
  expect_true(!any(is.na(fit$coefficients)))
})

