test_that("weighted_logrank basic functionality", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  res <- weighted_logrank(lung, "time", "status01", "sex")
  expect_type(res, "list")
  expect_true(all(c("statistic", "p.value") %in% names(res)))
  expect_true(res$statistic > 0)
  expect_true(res$p.value <= 1)
})

test_that("weighted_logrank handles rho/gamma variants", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  r0 <- weighted_logrank(lung, "time", "status01", "sex", rho = 0, gamma = 0)
  r1 <- weighted_logrank(lung, "time", "status01", "sex", rho = 1, gamma = 0)
  r2 <- weighted_logrank(lung, "time", "status01", "sex", rho = 0, gamma = 1)

  expect_true(is.numeric(r0$statistic))
  expect_true(is.numeric(r1$statistic))
  expect_true(is.numeric(r2$statistic))
})

test_that("weighted_logrank works with subject weights", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$w <- runif(nrow(lung), 0.5, 1.5)

  res <- weighted_logrank(lung, "time", "status01", "sex", weight = "w")
  expect_true(is.numeric(res$statistic))
})

test_that("weighted_logrank returns data.frame when return_df = TRUE", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- weighted_logrank(lung, "time", "status01", "sex", return_df = TRUE)

  expect_s3_class(df, "data.frame")
  expect_named(df, c("statistic", "p.value", "rho", "gamma", "method"))
})
