#' Advanced Time-Dependent Cox Model (with cluster & robust options)
#'
#' Extends `fit_cox_td()` by adding support for clustering, robust SEs,
#' and input validation. Useful for longitudinal or repeated-measure survival data.
#'
#' @param data   data.frame in long format.
#' @param start,stop,event Character columns defining counting-process intervals.
#' @param covars Character vector of covariate column names.
#' @param cluster Optional character; subject ID for within-cluster correlation.
#' @param robust Logical; if TRUE, uses robust variance estimation.
#' @param ties   Ties handling method (default = "efron").
#' @param summary Logical; if TRUE, prints model summary.
#'
#' @return A `coxph` model object.
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' data(heart, package = "survival")
#' fit_td <- fit_cox_timedep_advanced(
#'   data = heart,
#'   start = "start", stop = "stop", event = "event",
#'   covars = c("age", "year", "surgery"),
#'   summary = FALSE
#' )
#' summary(fit_td)
#'
#' @importFrom survival coxph Surv
#' @export
fit_cox_timedep_advanced <- function(data, start, stop, event, covars,
                                     cluster = NULL, robust = FALSE,
                                     ties = "efron", summary = TRUE) {
  stopifnot(is.data.frame(data))
  stopifnot(all(c(start, stop, event) %in% names(data)))
  stopifnot(length(covars) > 0, all(covars %in% names(data)))

  if (any(data[[stop]] <= data[[start]]))
    stop("Each interval must satisfy stop > start.")

  rhs <- paste(covars, collapse = " + ")
  if (!is.null(cluster)) rhs <- paste0(rhs, " + cluster(", cluster, ")")

  f <- stats::as.formula(
    paste0("survival::Surv(", start, ", ", stop, ", ", event, ") ~ ", rhs)
  )

  # Robust variance requires cluster or id
  if (robust && is.null(cluster)) {
    warning("`robust = TRUE` without cluster; disabling robust variance.")
    robust <- FALSE
  }

  fit <- survival::coxph(f, data = data, ties = ties, robust = robust)

  if (isTRUE(summary)) print(summary(fit))
  invisible(fit)
}
