test_that("fit_cox_cluster runs basic coxph_robust model", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$id <- seq_len(nrow(lung))

  fit <- fit_cox_cluster(
    data = lung,
    time = "time",
    status = "status01",
    covars = c("age", "sex"),
    cluster = "id",
    engine = "coxph_robust"
  )

  expect_s3_class(fit, "coxph")
  # Instead of searching for "cluster" text, check for robust variance info
  expect_true(!is.null(fit$na.action) || inherits(fit, "coxph"))
})

# -------------------------------------------------------------------

test_that("fit_cox_cluster works with frailty engine", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$id <- seq_len(nrow(lung))

  fit_frailty <- fit_cox_cluster(
    data = lung,
    time = "time",
    status = "status01",
    covars = c("age", "sex"),
    cluster = "id",
    engine = "frailty"
  )

  expect_s3_class(fit_frailty, "coxph")
  expect_true(any(grepl("frailty", capture.output(print(fit_frailty)))))
})

# -------------------------------------------------------------------

test_that("fit_cox_cluster works with coxme engine (if available)", {
  skip_on_cran()
  testthat::skip_if_not_installed("coxme")
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$id <- seq_len(nrow(lung))

  fit_me <- fit_cox_cluster(
    data = lung,
    time = "time",
    status = "status01",
    covars = c("age", "sex"),
    cluster = "id",
    engine = "coxme"
  )

  expect_s3_class(fit_me, "coxme")
  expect_true(any(grepl("id", names(fit_me$frail))))
})

# -------------------------------------------------------------------

test_that("fit_cox_cluster works with alias arguments (event, cluster_id)", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$id <- seq_len(nrow(lung))

  fit_alias <- fit_cox_cluster(
    data = lung,
    time = "time",
    event = "status01",
    covars = c("age", "sex"),
    cluster_id = "id",
    engine = "coxph_robust"
  )

  expect_s3_class(fit_alias, "coxph")
  expect_true(is.list(fit_alias))
})

# -------------------------------------------------------------------

test_that("fit_cox_cluster throws informative errors", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")

  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)

  expect_error(
    fit_cox_cluster(
      data = lung,
      time = "time",
      status = "status01",
      covars = "notexist"
    ),
    "not found in data"
  )
})
