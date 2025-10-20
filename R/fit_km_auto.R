#' Automatically Fit Kaplan-Meier Curves (Auto-Detect Columns)
#'
#' @description
#' A smart wrapper around [fit_km()] that automatically detects the time
#' and status columns based on common naming patterns (e.g., "time", "futime",
#' "status", "event", "death", etc.). It also normalizes the event variable
#' to 0/1 automatically.
#'
#' @details
#' This function is useful when working with datasets that use different
#' naming conventions. For example, the `survival::lung` dataset uses
#' "time" and "status" where events are coded as 2. The function can handle
#' this via `event_code = 2`.
#'
#' @param data A data frame containing survival variables.
#' @param time,status Optional character column names. If NULL, guessed automatically.
#' @param strata,weights Optional character column names for stratification and weights.
#' @param event_code Optional numeric value indicating the event (e.g., 2 in `lung` dataset).
#' @param conf.type Type of confidence interval ("log", "log-log", "plain", etc). Default "log-log".
#'
#' @return A `survfit` object (same output as [fit_km()]).
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' lung <- survival::lung
#'
#' # Automatically detect time/status and normalize
#' fit <- fit_km_auto(lung, strata = "sex", event_code = 2)
#' summary(fit)
#'
#' @export
fit_km_auto <- function(data,
                        time = NULL, status = NULL,
                        strata = NULL, weights = NULL,
                        event_code = NULL,
                        conf.type = "log-log") {

  # ---- Input validation ----
  if (!is.data.frame(data)) {
    stop("? Input `data` must be a data.frame, not ", class(data)[1], ".")
  }

  # ---- Helper functions ----
  .guess_col <- function(df, patterns) {
    nms <- names(df)
    for (pt in patterns) {
      hit <- nms[grepl(pt, nms, ignore.case = TRUE)]
      if (length(hit)) return(hit[1])
    }
    NA_character_
  }

  .normalize_status <- function(x, event_code = NULL) {
    if (is.logical(x)) return(as.integer(x))
    if (is.factor(x)) x <- as.integer(x)
    if (is.numeric(x)) {
      ux <- sort(unique(x[!is.na(x)]))
      if (all(ux %in% c(0, 1))) return(as.integer(x))
      if (all(ux %in% c(1, 2))) return(as.integer(x == 2))
      if (!is.null(event_code)) return(as.integer(x == event_code))
    }
    stop("? Cannot normalize `status` to 0/1. Please specify a valid `event_code` or numeric 0/1 column.")
  }

  # ---- Guess columns if missing ----
  if (is.null(time)) {
    time <- .guess_col(data, c("^time$", "futime", "os_time", "dur", "duration", "surv"))
  }
  if (is.null(status)) {
    status <- .guess_col(data, c("^status$", "^event$", "death", "died", "fail", "censor"))
  }

  # ---- Validation of detection ----
  if (is.na(time) || is.na(status)) {
    stop("? Could not detect `time` or `status` columns.\n",
         "Available columns: ", paste(names(data), collapse = ", "))
  }

  # ---- Notify user what was detected ----
  message(paste0("? Auto-detected columns: time = '", time, "', status = '", status, "'."))
  if (!is.null(strata)) message(paste0("? Using strata = '", strata, "'."))
  if (!is.null(weights)) message(paste0("? Using weights = '", weights, "'."))

  # ---- Normalize status ----
  df <- data
  df$..status01 <- .normalize_status(df[[status]], event_code = event_code)
  message("? Normalized status variable to 0/1 format.")

  # ---- Delegate to main KM function ----
  fit_km(data     = df,
         time     = time,
         status   = "..status01",
         strata   = strata,
         weights  = weights,
         conf.type = conf.type)
}

