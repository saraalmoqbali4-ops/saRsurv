test_that("plot_stratified_logrank runs and returns ggplot", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  testthat::skip_if_not_installed("ggplot2")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]

  p1 <- plot_stratified_logrank(df, "time", "status01", "sex", "ph.ecog", metric = "statistic")
  p2 <- plot_stratified_logrank(df, "time", "status01", "sex", "ph.ecog", metric = "pvalue")

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  d1 <- ggplot2::ggplot_build(p1)$data[[1]]
  expect_true(all(d1$y >= 0, na.rm = TRUE))
})
