# ===== R/fit_cox_timedep.R =====
#' Time-Dependent Cox Model (counting-process form)
#'
#' Fits a Cox proportional hazards model with time-dependent covariates using
#' counting-process notation \code{Surv(start, stop, event)}.
#'
#' @param data   data.frame in long format.
#' @param start  Character. Column name for interval start time.
#' @param stop   Character. Column name for interval stop time.
#' @param event  Character. Column name for event indicator (1=event, 0=censor).
#' @param covars Character vector of covariate column names.
#' @param ties   Ties handling for \code{survival::coxph} (default \code{"efron"}).
#' @return A \code{coxph} model object.
#'
#' @examples
#' \dontrun{
#'   data(heart, package = "survival")
#'   fit_td <- fit_cox_td(heart,
#'                        start = "start", stop = "stop", event = "event",
#'                        covars = c("age","year","surgery"))
#'   summary(fit_td)
#' }
#'
#' @importFrom survival coxph Surv
#' @export
fit_cox_td <- function(data, start, stop, event, covars, ties = "efron"){
  stopifnot(is.data.frame(data))
  stopifnot(all(c(start, stop, event) %in% names(data)))
  stopifnot(length(covars) > 0, all(covars %in% names(data)))

  f <- stats::as.formula(
    paste0("survival::Surv(", start, ", ", stop, ", ", event, ") ~ ",
           paste(covars, collapse = " + "))
  )
  survival::coxph(f, data = data, ties = ties)
}
