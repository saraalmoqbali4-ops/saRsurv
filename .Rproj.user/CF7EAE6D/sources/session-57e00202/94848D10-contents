# ===== R/fit_km_auto.R =====
#' Fit KM even if column names differ (auto-detect)
#'
#' Tries to guess `time` and `status` columns by common patterns and
#' normalizes the status to 0/1. Useful when your data doesn't use the
#' exact names (e.g., "futime", "event", "death"...).
#'
#' @param data data.frame
#' @param time,status Optional character column names. If NULL, guessed.
#' @param strata,weights Optional character column names.
#' @param event_code Optional numeric code that indicates an event if
#'   `status` is not 0/1 or 1/2 (e.g., event_code = 2 in `lung`).
#' @param conf.type Confidence interval type for KM. Default "log-log".
#' @return survfit object
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' lung <- survival::lung
#' fit <- fit_km_auto(
#'   data   = lung,
#'   time   = "time",
#'   status = "status",
#'   strata = "sex",
#'   event_code = 2
#' )
#' summary(fit)

#' @export
fit_km_auto <- function(data,
                        time = NULL, status = NULL,
                        strata = NULL, weights = NULL,
                        event_code = NULL,
                        conf.type = "log-log") {
  stopifnot(is.data.frame(data))

  # ---- helpers (internal) ----
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
    if (is.factor(x)) x <- as.integer(x)  # crude, but deterministic

    if (is.numeric(x)) {
      ux <- sort(unique(x[!is.na(x)]))
      if (all(ux %in% c(0,1))) return(as.integer(x))
      if (all(ux %in% c(1,2)))  return(as.integer(x == 2))
      if (!is.null(event_code)) return(as.integer(x == event_code))
    }
    stop("Cannot normalize `status` to 0/1. Provide `event_code` or a numeric 0/1 (or 1/2) column.")
  }

  # ---- guess cols if needed ----
  if (is.null(time)) {
    time <- .guess_col(data, c("^time$", "futime", "os_time", "dur", "duration", "surv"))
  }
  if (is.null(status)) {
    status <- .guess_col(data, c("^status$", "^event$", "death", "died", "fail", "censor"))
  }
  if (is.na(time) || is.na(status)) {
    stop("Could not detect `time`/`status` columns. Please supply them explicitly.\nAvailable columns: ",
         paste(names(data), collapse = ", "))
  }

  # ---- build a small working frame (and normalize status) ----
  df <- data
  df$..status01 <- .normalize_status(df[[status]], event_code = event_code)

  # ---- delegate to the main KM wrapper ----
  fit_km(data   = df,
         time   = time,
         status = "..status01",
         strata = strata,
         weights = weights,
         conf.type = conf.type)
}
