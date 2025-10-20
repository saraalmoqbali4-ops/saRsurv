#' Weighted Kaplan-Meier Estimator (with Optional Weights)
#'
#' @description
#' Computes a Kaplan-Meier survival curve that optionally incorporates subject-level weights
#' (e.g., inverse probability weights). If no weights are provided, equal weights are assumed.
#'
#' @param data A data frame containing the survival variables.
#' @param time The column name for the time-to-event variable.
#' @param status The column name for the event indicator (1 = event occurred, 0 = censored).
#' @param strata Optional column name for the stratification variable.
#' @param weights Optional column name for subject-level weights (e.g., IPW).
#'
#' @return A `survfit` object from the `survival` package.
#' @examples
#' \dontrun{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   data(lung, package = "survival")
#'   lung$status01 <- as.integer(lung$status == 2)
#'
#'   # Example without weights
#'   fit1 <- survfit_weighted(
#'     data = lung,
#'     time = "time",
#'     status = "status01",
#'     strata = "sex"
#'   )
#'   plot(fit1, main = "Weighted KM by Sex")
#'
#'   # Example with random weights
#'   set.seed(123)
#'   lung$ipw <- runif(nrow(lung), 0.5, 1.5)
#'   fit2 <- survfit_weighted(
#'     data = lung,
#'     time = "time",
#'     status = "status01",
#'     strata = "sex",
#'     weights = "ipw"
#'   )
#'   plot(fit2, col = c("blue", "red"), lty = 1:2,
#'        main = "Weighted KM with IPW",
#'        xlab = "Time", ylab = "Survival Probability")
#' }}
#' @export
survfit_weighted <- function(data, time, status, strata = NULL, weights = NULL) {
  # Check if required columns exist
  if (!all(c(time, status) %in% names(data))) {
    stop("Error: One or more required columns (time/status) not found in the data.")
  }

  # Handle weights
  if (is.null(weights)) {
    message("? No weights provided - defaulting to equal weights (all = 1).")
    w <- rep(1, nrow(data))
  } else {
    if (!weights %in% names(data)) {
      stop(paste0("Error: Weight column '", weights, "' not found in data."))
    }
    message(paste0("? Using weights from column: '", weights, "'."))
    w <- data[[weights]]
  }

  # Create the survival object
  surv_obj <- survival::Surv(data[[time]], data[[status]])

  # Fit the model
  if (is.null(strata)) {
    fit <- survival::survfit(surv_obj ~ 1, data = data, weights = w)
  } else {
    f <- as.formula(paste("Surv(", time, ",", status, ") ~", strata))
    fit <- survival::survfit(f, data = data, weights = w)
  }

  return(fit)
}
