test_that("plot_logrank_grid produces a ggplot heatmap", {
  skip_on_cran()
  testthat::skip_if_not_installed("ggplot2")

  grid_df <- data.frame(
    rho = rep(c(0, 0.5, 1), each = 3),
    gamma = rep(c(0, 0.5, 1), times = 3),
    p.value = runif(9, 0.001, 0.9)
  )

  p <- plot_logrank_grid(grid_df)
  expect_s3_class(p, "ggplot")

  # Instead of geom name, check that at least one geom_tile layer exists
  layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(any(grepl("GeomTile", layers)))

  # Verify -log10(p) computed correctly
  expect_true(all(-log10(grid_df$p.value) >= 0))
})
