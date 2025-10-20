test_that("calibrate_surv works correctly for Cox model (KM method)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  cfit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)

  cal <- calibrate_surv(
    fit = cfit,
    data = lung,
    time_col = "time",
    status_col = "status01",
    time_point = 365,
    ngroups = 5,
    observed_method = "km"
  )

  expect_s3_class(cal, "data.frame")
  expect_true(all(c("RiskGroup", "ObservedSurvival", "PredictedSurvival") %in% names(cal)))
  expect_true(all(cal$PredictedSurvival >= 0 & cal$PredictedSurvival <= 1))
})

test_that("calibrate_surv works with binary observed method", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  cfit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)

  cal_bin <- calibrate_surv(
    fit = cfit,
    data = lung,
    time_col = "time",
    status_col = "status01",
    time_point = 365,
    ngroups = 5,
    observed_method = "binary"
  )

  expect_s3_class(cal_bin, "data.frame")
  expect_true(all(c("RiskGroup", "ObservedSurvival", "PredictedSurvival") %in% names(cal_bin)))
})

