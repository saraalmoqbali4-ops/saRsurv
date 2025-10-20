test_that("stratified_weighted_logrank basic functionality", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]

  res <- stratified_weighted_logrank(df, "time", "status01", "sex", "ph.ecog")
  expect_type(res, "list")
  expect_true(all(c("statistic", "p.value", "method") %in% names(res)))
  expect_true(is.numeric(res$statistic))
  expect_true(res$n_strata >= 1)
})

test_that("stratified_weighted_logrank works with IPW weights", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
  set.seed(123)
  df$ipw <- runif(nrow(df), 0.5, 1.5)

  res <- stratified_weighted_logrank(df, "time", "status01", "sex", "ph.ecog", ipw = "ipw")
  expect_true(is.numeric(res$statistic))
  expect_true(res$n > 0)
})

test_that("stratified_weighted_logrank matches survdiff when rho=0", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]

  res1 <- stratified_weighted_logrank(df, "time", "status01", "sex", "ph.ecog", rho = 0)
  sd_fit <- survival::survdiff(survival::Surv(time, status01) ~ sex + strata(ph.ecog), data = df)
  p_ref <- stats::pchisq(sd_fit$chisq, df = 1, lower.tail = FALSE)

  expect_true(abs(res1$p.value - p_ref) < 0.05)
})

