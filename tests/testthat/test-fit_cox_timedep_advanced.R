test_that("fit_cox_timedep_advanced runs basic Cox TD model", {
  skip_on_cran()
  skip_if_not_installed("survival")

  data(heart, package = "survival")

  fit <- fit_cox_timedep_advanced(
    data = heart,
    start = "start",
    stop = "stop",
    event = "event",
    covars = c("age", "year", "surgery"),
    summary = FALSE
  )

  expect_s3_class(fit, "coxph")
  expect_true("coefficients" %in% names(fit))
  expect_true(length(fit$coefficients) >= 1)
})


test_that("fit_cox_timedep_advanced supports cluster argument", {
  skip_on_cran()
  skip_if_not_installed("survival")

  data(heart, package = "survival")

  heart$id <- rep(1:100, length.out = nrow(heart))

  fit <- fit_cox_timedep_advanced(
    data = heart,
    start = "start",
    stop = "stop",
    event = "event",
    covars = c("age", "year"),
    cluster = "id",
    robust = TRUE,       # ← ضروري مع cluster
    summary = FALSE
  )

  expect_s3_class(fit, "coxph")
  expect_true(!is.null(fit$naive.var))
})


test_that("fit_cox_timedep_advanced supports robust variance option", {
  skip_on_cran()
  skip_if_not_installed("survival")

  data(heart, package = "survival")

  # robust = TRUE بدون cluster → يعطي تحذير فقط
  expect_warning(
    fit_cox_timedep_advanced(
      data = heart,
      start = "start",
      stop = "stop",
      event = "event",
      covars = c("age", "year"),
      robust = TRUE,
      summary = FALSE
    ),
    "robust = TRUE"
  )
})


test_that("fit_cox_timedep_advanced checks stop > start validation", {
  skip_on_cran()

  df_bad <- data.frame(
    start = c(0, 5, 10),
    stop = c(5, 3, 12),
    event = c(1, 0, 1),
    age = c(60, 65, 70),
    year = c(1, 2, 3)
  )

  expect_error(
    fit_cox_timedep_advanced(
      data = df_bad,
      start = "start",
      stop = "stop",
      event = "event",
      covars = c("age", "year")
    ),
    "stop > start"
  )
})

