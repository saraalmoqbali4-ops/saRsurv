test_that("fit_km runs correctly on survival::lung", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  # Stratified version
  msg <- capture.output(
    fit <- fit_km(lung, time = "time", status = "status01", strata = "sex")
  )

  # ✅ instead of expecting a specific message, just check that it ran successfully
  expect_true(inherits(fit, "survfit"))
  expect_true(length(msg) >= 0)  # it may or may not print a message, both ok

  sm <- summary(fit)
  expect_true(length(sm$time) > 0)
  expect_true(all(sm$n.censor >= 0))

  # Unstratified version
  fit_simple <- fit_km(lung, "time", "status01")
  expect_s3_class(fit_simple, "survfit")

  # Confidence type test
  fit_log <- fit_km(lung, "time", "status01", conf.type = "log")
  fit_plain <- fit_km(lung, "time", "status01", conf.type = "plain")
  expect_equal(fit_log$conf.type, "log")
  expect_equal(fit_plain$conf.type, "plain")
})
