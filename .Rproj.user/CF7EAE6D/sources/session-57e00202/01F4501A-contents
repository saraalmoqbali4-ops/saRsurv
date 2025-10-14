# ===== tests/testthat/test-fit_cox_timedep.R =====
test_that("fit_cox_td runs on survival::heart", {
  skip_on_cran()
  data(heart, package = "survival")

  fit <- fit_cox_td(heart,
                    start = "start", stop = "stop", event = "event",
                    covars = c("age","year","surgery"))
  expect_s3_class(fit, "coxph")
  sm <- summary(fit)
  expect_true(length(sm$coefficients) > 0)
})
