test_that("plot_km_alpha works and sets per-stratum alpha", {
  skip_on_cran()
  testthat::skip_if_not_installed("survminer")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)

  # نستخدم suppressWarnings علشان نخفي تحذير ggplot2 "size" is deprecated
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

  expect_true(is.list(gp) && "plot" %in% names(gp))
  expect_true("alpha_v" %in% names(gp$plot$data))
  expect_true(all(gp$plot$data$alpha_v >= 0.2 & gp$plot$data$alpha_v <= 0.9, na.rm = TRUE))
})
