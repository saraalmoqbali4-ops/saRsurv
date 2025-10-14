test_that("logrank_grid returns a table over (rho, gamma)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  grd <- expand.grid(rho = c(0, 0.5), gamma = c(0, 1))
  gdf <- logrank_grid(lung, "time", "status01", "sex", grid = grd)

  expect_s3_class(gdf, "data.frame")
  expect_equal(nrow(gdf), nrow(grd))
  expect_true(all(is.finite(gdf$statistic)))
  expect_true(all(gdf$p.value >= 0 & gdf$p.value <= 1, na.rm = TRUE))
})
