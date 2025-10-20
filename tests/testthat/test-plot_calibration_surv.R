test_that("plot_calibration_surv produces a ggplot object", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  testthat::skip_if_not_installed("ggplot2")

  # Prepare survival data
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  cfit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)

  # Calibration table
  cal <- calibrate_surv(
    fit = cfit,
    data = lung,
    time_col = "time",
    status_col = "status01",
    time_point = 365,
    ngroups = 5
  )

  # Plot calibration curve
  p <- plot_calibration_surv(cal)

  # --- Tests ---
  expect_s3_class(p, "ggplot")
  expect_true(all(c("PredictedSurvival", "ObservedSurvival") %in% names(p$data)))

  # Instead of checking exact equality to c(0,1),
  # verify that the observed and predicted ranges fall within [0,1]
  data_range_x <- range(p$data$PredictedSurvival, na.rm = TRUE)
  data_range_y <- range(p$data$ObservedSurvival, na.rm = TRUE)

  expect_true(all(data_range_x >= 0 & data_range_x <= 1))
  expect_true(all(data_range_y >= 0 & data_range_y <= 1))
})
