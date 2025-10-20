test_that("plot_cox_forest works on a standard Cox model", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  testthat::skip_if_not_installed("ggplot2")

  data(heart, package = "survival")

  fit <- survival::coxph(
    survival::Surv(start, stop, event) ~ age + year + surgery,
    data = heart
  )

  p <- plot_cox_forest(fit, title = "Test Forest Plot")

  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
  expect_true(any(c("HR", "Lower", "Upper") %in% names(p$data)))
})


test_that("plot_cox_forest hides p-values correctly when show_pvalue = FALSE", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  testthat::skip_if_not_installed("ggplot2")

  data(heart, package = "survival")

  fit <- survival::coxph(
    survival::Surv(start, stop, event) ~ age + year + surgery,
    data = heart
  )

  p <- plot_cox_forest(fit, show_pvalue = FALSE)
  expect_s3_class(p, "ggplot")

  # Extract text layers (should be none)
  text_layers <- vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))
  expect_false(any(text_layers))
})


test_that("plot_cox_forest throws error on non-coxph object", {
  expect_error(plot_cox_forest(mtcars), "coxph")
})
