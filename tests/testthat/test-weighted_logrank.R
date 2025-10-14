test_that("weighted_logrank matches survdiff for standard log-rank", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  res <- weighted_logrank(lung, "time", "status01", "sex")  # rho=gamma=0

  sd  <- survival::survdiff(survival::Surv(time, status01) ~ sex, data = lung, rho = 0)
  stat_ref <- as.numeric(sd$chisq)
  p_ref    <- stats::pchisq(stat_ref, df = 1, lower.tail = FALSE)

  expect_true(is.finite(res$statistic) && is.finite(res$p.value))
  expect_lt(abs(res$statistic - stat_ref), 1e-6)
  expect_lt(abs(res$p.value  - p_ref),     1e-6)
})

test_that("weighted_logrank runs with late-emphasis (gamma=1)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  res <- weighted_logrank(lung, "time", "status01", "sex", rho = 0, gamma = 1)
  expect_true(is.numeric(res$p.value) && res$p.value >= 0 && res$p.value <= 1)
})

test_that("weighted_logrank accepts IPW", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  set.seed(1)
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$ipw <- runif(nrow(lung), 0.5, 1.5)

  res <- weighted_logrank(lung, "time", "status01", "sex", ipw = "ipw")
  expect_true(is.list(res) && "p.value" %in% names(res))
})
