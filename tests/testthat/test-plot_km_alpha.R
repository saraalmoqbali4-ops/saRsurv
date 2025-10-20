# ===== tests/testthat/test-plot_km_alpha.R =====

test_that("plot_km_alpha works and sets per-stratum alpha", {
  skip_on_cran()
  testthat::skip_if_not_installed("survminer")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("survival")

  # ---- Prepare data ----
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  fit <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)

  # ---- Run function ----
  gp <- suppressWarnings(
    plot_km_alpha(
      fit,
      data = lung,
      group_var = "sex",
      transform = "log",
      min_alpha = 0.2,
      max_alpha = 0.9
    )
  )

  # ---- Check structure ----
  expect_true(is.list(gp))
  expect_true("plot" %in% names(gp))
  expect_s3_class(gp$plot, "ggplot")

  # ---- Check alpha column ----
  d <- gp$plot$data
  expect_true("alpha_v" %in% names(d))
  expect_true(all(d$alpha_v >= 0.2 & d$alpha_v <= 0.9, na.rm = TRUE))

  # ---- Check alpha varies by group ----
  if ("strata" %in% names(d)) {
    a_levels <- tapply(d$alpha_v, d$strata, mean, na.rm = TRUE)
    expect_true(length(unique(round(a_levels, 3))) > 1)
  }
})

test_that("plot_km_alpha handles non-stratified fits", {
  skip_on_cran()
  testthat::skip_if_not_installed("survminer")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  fit_single <- survival::survfit(survival::Surv(time, status01) ~ 1, data = lung)

  gp <- suppressWarnings(
    plot_km_alpha(fit_single, data = lung, group_var = "sex")
  )

  expect_true("plot" %in% names(gp))
  expect_true("alpha_v" %in% names(gp$plot$data))
  expect_true(all(gp$plot$data$alpha_v <= 1))
})
