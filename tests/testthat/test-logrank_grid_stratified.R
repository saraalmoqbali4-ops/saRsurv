test_that("logrank_grid_stratified runs and returns data frame", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]

  res <- logrank_grid_stratified(df, "time", "status01", "sex", "ph.ecog")

  expect_s3_class(res, "data.frame")
  expect_true(all(c("rho", "gamma", "statistic", "p.value") %in% names(res)))
  expect_true(nrow(res) >= 3)
  expect_true(all(res$p.value >= 0 & res$p.value <= 1, na.rm = TRUE))
})

test_that("logrank_grid_stratified works with custom grid", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
  grd <- expand.grid(rho = c(0, 1), gamma = c(0, 1))

  res <- logrank_grid_stratified(df, "time", "status01", "sex", "ph.ecog", grid = grd)
  expect_equal(nrow(res), 4)
  expect_true(is.numeric(res$statistic))
})
