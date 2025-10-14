# tests/testthat/test-compute_rale.R
test_that("compute_rale works for unstratified and stratified fits", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  f0 <- survival::survfit(survival::Surv(time, status01) ~ 1, data = lung)
  r0 <- compute_rale(f0, t0 = 0)
  expect_type(r0, "double")
  expect_equal(length(r0), 1L)
  expect_true(r0 >= 0)

  fs <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
  rs <- compute_rale(fs, t0 = 0)
  expect_type(rs, "double")
  expect_true(length(rs) >= 2L)
  expect_true(all(rs >= 0))
  expect_false(is.null(names(rs)))
})
