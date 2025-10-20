# ===== tests/testthat/test-fit_km_auto.R =====

test_that("fit_km_auto detects columns in survival::lung", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung

  # should detect automatically and print messages
  expect_message(
    fit <- fit_km_auto(lung, strata = "sex", event_code = 2),
    "Auto-detected columns"
  )

  expect_s3_class(fit, "survfit")

  sm <- summary(fit)
  expect_true(length(sm$time) > 0)
  expect_true(all(sm$n.censor >= 0))
})

test_that("fit_km_auto works with custom names (futime/event)", {
  skip_on_cran()

  set.seed(1)
  df <- data.frame(
    futime = c(5, 8, 10, 12, 21),
    event  = c(0, 1, 0, 1, 1),
    sex    = c(1, 2, 1, 2, 2)
  )

  fit <- fit_km_auto(df, strata = "sex")
  expect_s3_class(fit, "survfit")

  sm <- summary(fit)
  expect_true(length(sm$time) > 0)
})

test_that("fit_km_auto errors helpfully if cannot detect columns", {
  skip_on_cran()

  df <- data.frame(a = 1:3, b = 1:3)
  expect_error(
    fit_km_auto(df),
    "Could not detect `time`|`status`"
  )
})

test_that("fit_km_auto needs status mappable to 0/1 unless event_code is given", {
  skip_on_cran()

  df_bad <- data.frame(
    time   = c(2, 3, 4),
    status = c(2, 3, 2)
  )

  # should error without event_code
  expect_error(
    fit_km_auto(df_bad, time = "time", status = "status"),
    "Cannot normalize `status`"
  )

  # should work when event_code is supplied
  fit_ok <- fit_km_auto(df_bad, time = "time", status = "status", event_code = 3)
  expect_s3_class(fit_ok, "survfit")
})

test_that("fit_km_auto works when columns are passed manually (non-auto mode)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  fit_manual <- fit_km_auto(lung,
                            time = "time",
                            status = "status",
                            strata = "sex",
                            event_code = 2)
  expect_s3_class(fit_manual, "survfit")

  sm <- summary(fit_manual)
  expect_true(length(sm$time) > 0)
})
