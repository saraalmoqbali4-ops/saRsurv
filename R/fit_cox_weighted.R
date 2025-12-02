#' Weighted Cox Proportional Hazards Model
#'
#' Fit a weighted Cox proportional hazards model using user-defined weights.
#'
#' @param data A data.frame containing the variables.
#' @param time Character string. Name of the survival time variable.
#' @param status Character string. Name of the event indicator (1 = event, 0 = censored).
#' @param covariates Character vector of covariate names to include in the model.
#' @param weights Optional numeric vector of case weights. Must match nrow(data).
#'
#' @return A coxph model object.
#' @export
#'
#' @examples
#' # fit_cox_weighted(data = df, time = "time", status = "status",
#' #                  covariates = c("age", "trt"), weights = w)
fit_cox_weighted <- function(data,
                             time,
                             status,
                             covariates,
                             weights = NULL) {

  # --- Input checks ---
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!all(c(time, status, covariates) %in% names(data)))
    stop("Variables not found in data.")
  if (!is.null(weights) && length(weights) != nrow(data))
    stop("weights must be the same length as number of rows in data.")

  # Build formula: Surv(time, status) ~ cov1 + cov2 + ...
  f <- as.formula(
    paste0("survival::Surv(", time, ", ", status, ") ~ ",
           paste(covariates, collapse = " + "))
  )

  # Fit weighted Cox model
  model <- survival::coxph(
    formula = f,
    data    = data,
    weights = weights
  )

  return(model)
}
