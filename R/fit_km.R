#' Fit Kaplan-Meier survival curves
#' @param data Data frame containing the variables.
#' @param time Name of the time variable.
#' @param status Name of the event variable (1 = event, 0 = censor).
#' @param strata Optional name of a grouping (strata) variable.
#' @param weights Optional case weights.
#' @param conf.type Type of confidence interval ("log", "plain", etc).
#' @return A survfit object.
#' @export
fit_km <- function(data, time, status, strata = NULL, weights = NULL, conf.type = "log") {
  f <- if (is.null(strata)) {
    stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ 1"))
  } else {
    stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ ", strata))
  }
  survival::survfit(formula = f, data = data, weights = weights, conf.type = conf.type)
}
