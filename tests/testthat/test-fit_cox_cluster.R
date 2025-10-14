test_that("fit_cox_cluster runs with coxph_robust on lung", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  library(survival)

  set.seed(1)
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$cluster  <- sample(seq_len(8), nrow(lung), TRUE)

  m <- fit_cox_cluster(
    data    = lung,
    time    = "time",
    status  = "status01",
    covars  = c("age", "sex"),
    cluster = "cluster",
    engine  = "coxph_robust"
  )
  expect_s3_class(m, "coxph")
})

test_that("fit_cox_cluster runs with frailty()", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  library(survival)

  set.seed(2)
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$cluster  <- sample(seq_len(6), nrow(lung), TRUE)

  m <- fit_cox_cluster(
    data    = lung,
    time    = "time",
    status  = "status01",
    covars  = c("age", "sex"),
    cluster = "cluster",
    engine  = "frailty"
  )
  expect_s3_class(m, "coxph")
})

test_that("survfit_strata_cluster alias works", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  library(survival)

  set.seed(3)
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$cluster  <- sample(seq_len(5), nrow(lung), TRUE)

  m <- survfit_strata_cluster(
    data       = lung,
    time       = "time",
    status     = "status01",
    covars     = c("age", "sex"),
    strata_var = "sex",
    cluster_id = "cluster",
    engine     = "coxph_robust"
  )
  expect_s3_class(m, "coxph")
})

test_that("fit_cox_cluster runs with coxme when available", {
  skip_on_cran()
  testthat::skip_if_not_installed("survival")
  testthat::skip_if_not_installed("coxme")
  library(survival)

  set.seed(4)
  lung <- survival::lung
  lung$status01 <- as.integer(lung$status == 2)
  lung$cluster  <- sample(seq_len(4), nrow(lung), TRUE)

  m <- fit_cox_cluster(
    data    = lung,
    time    = "time",
    status  = "status01",
    covars  = c("age", "sex"),
    cluster = "cluster",
    engine  = "coxme"
  )
  expect_s3_class(m, "coxme")
})

