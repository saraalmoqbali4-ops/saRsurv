test_that("plot_calibration_surv returns a ggplot", {
  skip_on_cran()
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)
  cal <- calibrate_surv(fit, lung, "time", "status01", time_point = 365, ngroups = 6)

  p <- plot_calibration_surv(cal)
  expect_s3_class(p, "ggplot")
})
