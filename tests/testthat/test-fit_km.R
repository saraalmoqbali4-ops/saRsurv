test_that("fit_km runs correctly on survival::lung", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  fit <- fit_km(lung, time = "time", status = "status01", strata = "sex")
  expect_s3_class(fit, "survfit")

  sm <- summary(fit)
  expect_true(length(sm$time) > 0)
  expect_true(all(sm$n.censor >= 0))
})
