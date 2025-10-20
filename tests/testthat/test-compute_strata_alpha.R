# ===== tests/testthat/test-compute_strata_alpha.R =====

test_that("compute_strata_alpha works for multiple transforms", {
  x <- rep(letters[1:3], times = c(5, 10, 20))

  # log
  a_log <- compute_strata_alpha(x, transform = "log")
  expect_true(all(a_log >= 0.3 & a_log <= 1))

  # sqrt
  a_sqrt <- compute_strata_alpha(x, transform = "sqrt")
  expect_true(all(a_sqrt >= 0.3 & a_sqrt <= 1))

  # linear
  a_lin <- compute_strata_alpha(x, transform = "linear")
  expect_true(all(a_lin >= 0.3 & a_lin <= 1))

  # power
  a_pow <- compute_strata_alpha(x, transform = "power", power = 0.25)
  expect_true(all(a_pow >= 0.3 & a_pow <= 1))

  # reverse
  a_rev <- compute_strata_alpha(x, transform = "reverse")
  expect_true(all(a_rev >= 0.3 & a_rev <= 1))
})

test_that("compute_strata_alpha returns data frame when return_df = TRUE", {
  x <- rep(LETTERS[1:3], times = c(3, 6, 9))
  df <- compute_strata_alpha(x, return_df = TRUE)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("stratum", "alpha"))
  expect_true(all(df$alpha >= 0.3 & df$alpha <= 1))
})

test_that("compute_strata_alpha handles single group gracefully", {
  x <- rep("A", 10)
  a <- compute_strata_alpha(x)
  expect_equal(as.numeric(a), 1)
  expect_named(a, "A")

  df <- compute_strata_alpha(x, return_df = TRUE)
  expect_equal(df$alpha, 1)
})

test_that("compute_strata_alpha warns on missing values", {
  x <- c("A", "A", "B", NA, "B")
  expect_warning(compute_strata_alpha(x), "Missing values")
})

test_that("compute_strata_alpha bounds and errors work", {
  x <- rep(c("A", "B"), each = 5)

  # min_alpha > 1 (out of range)
  expect_error(compute_strata_alpha(x, min_alpha = 1.1), "within")

  # max_alpha < 0 (out of range)
  expect_error(compute_strata_alpha(x, max_alpha = -0.1), "within")

  # min_alpha >= max_alpha (invalid ordering)
  expect_error(compute_strata_alpha(x, min_alpha = 0.9, max_alpha = 0.5), "smaller")
})

test_that("compute_strata_alpha scales correctly when groups are equal", {
  x <- rep(letters[1:3], each = 10)
  a <- compute_strata_alpha(x)
  expect_true(all(a == a[1]))
})
