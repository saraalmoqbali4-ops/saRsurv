test_that("survfit_weighted runs (unweighted)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit <- survfit_weighted(lung, time = "time", status = "status01")
  expect_s3_class(fit, "survfit")

  sm <- summary(fit)
  expect_true(length(sm$time) >= 1)
})

test_that("survfit_weighted runs with weights", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  set.seed(123)
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$w <- runif(nrow(lung), 0.5, 1.5)

  fit_w <- survfit_weighted(lung, "time", "status01", weights = "w")
  expect_s3_class(fit_w, "survfit")
})

test_that("survfit_weighted supports strata", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit_s <- survfit_weighted(lung, "time", "status01", strata = "sex")
  expect_s3_class(fit_s, "survfit")
})

# --- Recommended additions for completeness ---

test_that("survfit_weighted prints message when no weights are provided", {
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  expect_message(
    survfit_weighted(lung, "time", "status01"),
    "No weights provided"
  )
})

test_that("survfit_weighted throws an error if the weight column does not exist", {
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  expect_error(
    survfit_weighted(lung, "time", "status01", weights = "not_exist"),
    "not found"
  )
})
