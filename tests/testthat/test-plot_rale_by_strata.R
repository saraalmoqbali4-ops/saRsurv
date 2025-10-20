test_that("plot_rale_by_strata works correctly", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  testthat::skip_if_not_installed("ggplot2")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit_strat <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
  p <- plot_rale_by_strata(fit_strat, t0 = 0)

  expect_s3_class(p, "ggplot")

  # Check data embedded in the ggplot
  df <- ggplot2::ggplot_build(p)$data[[1]]
  expect_true(all(df$y >= 0))
})

test_that("plot_rale_by_strata errors for unstratified fit", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  fit0 <- survival::survfit(survival::Surv(time, status01) ~ 1, data = lung)

  expect_error(plot_rale_by_strata(fit0), "not stratified")
})
