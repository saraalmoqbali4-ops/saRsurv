test_that("compute_rale works for unstratified and stratified fits", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  # Unstratified
  fit0 <- survival::survfit(survival::Surv(time, status01) ~ 1, data = lung)
  rale0 <- compute_rale(fit0)
  expect_true(is.numeric(rale0))
  expect_length(rale0, 1)
  expect_true(rale0 > 0)

  # Stratified by sex
  fits <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
  rales <- compute_rale(fits)
  expect_true(is.numeric(rales))
  expect_named(rales)
  expect_true(all(rales > 0))

  # t0 cutoff
  rale_cut <- compute_rale(fit0, t0 = 500)
  expect_true(rale_cut < rale0)
})
