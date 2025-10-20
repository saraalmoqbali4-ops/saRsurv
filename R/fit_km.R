#' Fit Kaplan-Meier Survival Curves (Professional Version)
#'
#' @description
#' A clean and robust wrapper around `survival::survfit()` for fitting
#' Kaplan-Meier survival curves. This function automatically builds the formula
#' from variable names and supports optional stratification and weighting.
#'
#' @details
#' The Kaplan-Meier estimator provides a nonparametric estimate of the
#' survival function \( S(t) = P(T > t) \).
#' This implementation allows users to easily fit survival curves, compare
#' groups (via the `strata` argument), and include optional case weights.
#'
#' @param data A data frame containing survival variables.
#' @param time Character name of the time-to-event variable.
#' @param status Character name of the event indicator variable (1 = event, 0 = censored).
#' @param strata Optional character name of a grouping (stratification) variable.
#' @param weights Optional character name of a column containing case weights.
#' @param conf.type Type of confidence interval ("log", "plain", "log-log", etc). Default is "log".
#'
#' @return A `survfit` object (or subclass "fit_km") containing:
#' \itemize{
#'   \item Estimated survival probabilities over time
#'   \item Confidence intervals for \( S(t) \)
#'   \item (Optionally) separate curves for each stratum
#' }
#'
#' @examples
#' library(survival)
#' data(lung)
#' lung$status01 <- as.integer(lung$status == 2)
#'
#' # Example 1: Simple KM curve
#' fit1 <- fit_km(lung, time = "time", status = "status01")
#'
#' # Example 2: Stratified by gender
#' fit2 <- fit_km(lung, time = "time", status = "status01", strata = "sex")
#'
#' # Example 3: With weights
#' set.seed(123)
#' lung$w <- runif(nrow(lung), 0.5, 1.5)
#' fit3 <- fit_km(lung, time = "time", status = "status01", weights = "w")
#'
#' @export
fit_km <- function(data,
                   time,
                   status,
                   strata = NULL,
                   weights = NULL,
                   conf.type = "log") {
  # ---- Input validation ----
  required_cols <- c(time, status)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("? Required column(s) not found in data: ",
                paste(missing_cols, collapse = ", ")))
  }

  # Check strata column
  if (!is.null(strata) && !strata %in% names(data)) {
    stop(paste0("? Strata column '", strata, "' not found in data."))
  }

  # Handle weights
  if (!is.null(weights)) {
    if (!weights %in% names(data)) {
      stop(paste0("? Weight column '", weights, "' not found in data."))
    }
    message(paste0("? Using weights from column: '", weights, "'."))
    w <- data[[weights]]
  } else {
    message("? No weights provided - defaulting to equal weights (all = 1).")
    w <- rep(1, nrow(data))
  }

  # Inform user about model type
  if (is.null(strata)) {
    message("? Fitting Kaplan-Meier curve (no strata).")
  } else {
    message(paste0("? Fitting Kaplan-Meier curves by '", strata, "'."))
  }

  # ---- Build the formula ----
  f <- if (is.null(strata)) {
    stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ 1"))
  } else {
    stats::as.formula(paste0("survival::Surv(", time, ",", status, ") ~ ", strata))
  }

  # ---- Fit the model ----
  fit <- survival::survfit(formula = f, data = data,
                           weights = w, conf.type = conf.type)

  # ---- Return with custom class ----
  class(fit) <- c("fit_km", class(fit))
  return(fit)
}
