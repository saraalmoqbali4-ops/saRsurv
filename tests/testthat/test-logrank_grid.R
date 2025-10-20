test_that("logrank_grid runs grid search and returns data frame", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  gdf <- logrank_grid(lung, "time", "status01", "sex")
  expect_s3_class(gdf, "data.frame")
  expect_named(gdf, c("rho", "gamma", "statistic", "p.value"))
  expect_true(all(gdf$p.value >= 0 & gdf$p.value <= 1))
  expect_true(all(gdf$rho >= 0))
})

test_that("logrank_grid accepts custom grid and sorts results", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  grid <- expand.grid(rho = c(0, 1), gamma = c(0, 1))
  gdf <- logrank_grid(lung, "time", "status01", "sex", grid = grid, sort = TRUE)

  expect_equal(ncol(gdf), 4)
  expect_true(is.numeric(gdf$statistic))
  expect_true(is.numeric(gdf$p.value))
})
