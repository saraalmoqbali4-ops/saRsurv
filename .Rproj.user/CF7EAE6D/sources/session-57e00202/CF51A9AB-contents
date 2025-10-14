#' Weighted Kaplan–Meier (with optional strata)
#'
#' @param data data.frame
#' @param time Name of time column
#' @param status Name of status column (1 = event, 0 = censor)
#' @param strata Optional name of a grouping column
#' @param weights Optional vector or column-name of case weights
#' @param conf.type CI type passed to `survival::survfit()`
#' @return A `survfit` object
#' @export
survfit_weighted <- function(data, time, status, strata = NULL, weights = NULL, conf.type = "log") {
  wv <- NULL
  if (!is.null(weights)) {
    wv <- if (is.character(weights)) data[[weights]] else weights
  }
  f <- if (is.null(strata)) {
    stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ 1"))
  } else {
    stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ ", strata))
  }
  survival::survfit(formula = f, data = data, weights = wv, conf.type = conf.type)
}
