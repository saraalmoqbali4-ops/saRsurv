test_that("calibrate_surv returns expected columns and ranges (KM method)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")


  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)

  cal <- calibrate_surv(
    fit        = fit,
    data       = lung,
    time_col   = "time",
    status_col = "status01",
    time_point = 365,
    ngroups    = 5,
    observed_method = "km"
  )

  expect_s3_class(cal, "data.frame")
  expect_setequal(names(cal), c("RiskGroup", "n", "ObservedSurvival", "PredictedSurvival"))
  expect_true(all(cal$ObservedSurvival  >= 0 & cal$ObservedSurvival  <= 1, na.rm = TRUE))
  expect_true(all(cal$PredictedSurvival >= 0 & cal$PredictedSurvival <= 1, na.rm = TRUE))
  expect_equal(nrow(cal), 5)
})

test_that("calibrate_surv works with binary observed method", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)

  cal <- calibrate_surv(
    fit        = fit,
    data       = lung,
    time_col   = "time",
    status_col = "status01",
    time_point = 365,
    ngroups    = 4,
    observed_method = "binary"
  )

  expect_s3_class(cal, "data.frame")
  expect_setequal(names(cal), c("RiskGroup", "n", "ObservedSurvival", "PredictedSurvival"))
  expect_true(all(cal$ObservedSurvival  >= 0 & cal$ObservedSurvival  <= 1, na.rm = TRUE))
  expect_true(all(cal$PredictedSurvival >= 0 & cal$PredictedSurvival <= 1, na.rm = TRUE))
  expect_equal(nrow(cal), 4)
})
