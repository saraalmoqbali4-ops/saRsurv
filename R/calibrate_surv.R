#' Predictive calibration of survival models at a time horizon
#'
#' Calibrates a survival model by grouping subjects into risk quantiles
#' (based on predicted survival at a given `time_point`) and comparing
#' observed versus predicted survival per group.
#'
#' Supports Cox PH models out of the box. For other models, you may pass a
#' custom prediction function via `predict_fun`.
#'
#' @param fit A fitted survival model. For Cox, a `coxph` object.
#' @param data A data frame containing predictions and observed outcomes.
#' @param time_col Name of the time column (character).
#' @param status_col Name of the event status column (`1 = event`, `0 = censor`).
#' @param time_point Numeric value specifying the time horizon at which to calibrate (e.g., `365`).
#' @param ngroups Integer number of risk groups (quantiles). Default is `10`.
#' @param observed_method Either `"binary"` (fraction with `time > time_point`)
#'   or `"km"` (Kaplan-Meier estimate at `time_point`). Default is `"km"`.
#' @param predict_fun Optional function `(fit, data, time_point) -> numeric`
#'   returning predicted survival probabilities in `[0, 1]`. If `NULL` and `fit`
#'   is a Cox model, a baseline-hazard approach is used.
#'
#' @return A data frame with the following columns:
#'   * `RiskGroup`
#'   * `n`
#'   * `ObservedSurvival`
#'   * `PredictedSurvival`
#'
#' @examples
#' \dontrun{
#' library(survival)
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#' cfit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)
#' cal <- calibrate_surv(
#'   fit = cfit,
#'   data = lung,
#'   time_col = "time",
#'   status_col = "status01",
#'   time_point = 365,
#'   ngroups = 10,
#'   observed_method = "km"
#' )
#' head(cal)
#' }
#'
#' @export
calibrate_surv <- function(fit, data, time_col, status_col,
                           time_point, ngroups = 10,
                           observed_method = c("km", "binary"),
                           predict_fun = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(all(c(time_col, status_col) %in% names(data)))
  observed_method <- match.arg(observed_method)

  # ---- Predicted survival at time_point ----
  if (is.null(predict_fun)) {
    # Cox PH: S(t|X) = exp( -H0(t) * exp(lp) )
    if (!inherits(fit, "coxph"))
      stop("Provide `predict_fun` for non-Cox models.")

    lp <- stats::predict(fit, newdata = data, type = "lp")
    bh <- survival::basehaz(fit, centered = FALSE)
    Ht <- stats::approx(bh$time, bh$hazard, xout = time_point, rule = 2)$y
    pred <- exp(-Ht * exp(lp))
  } else {
    pred <- predict_fun(fit, data, time_point)
  }

  # guard
  if (any(!is.finite(pred))) stop("Non-finite predictions encountered.")
  pred <- pmin(pmax(pred, 0), 1)

  # ---- Risk bins by predicted survival ----
  brks <- stats::quantile(pred, probs = seq(0, 1, length.out = ngroups + 1),
                          na.rm = TRUE, type = 7)
  brks_unique <- unique(brks)

  if (length(brks_unique) < 2) {
    # fallback: all predictions identical
    groups <- factor(rep("All", length(pred)))
  } else {
    groups <- cut(pred, breaks = brks_unique, include.lowest = TRUE, dig.lab = 6)
  }

  # ---- Observed survival per bin ----
  obs_val <- function(df) {
    if (observed_method == "binary") {
      mean(df[[time_col]] > time_point)
    } else {
      sf <- survival::survfit(survival::Surv(df[[time_col]], df[[status_col]]) ~ 1)
      ssum <- summary(sf)
      tt <- ssum$time
      ss <- ssum$surv
      if (!length(tt)) return(1)
      # step function S(t): use last value at or below time_point, else 1
      if (any(tt <= time_point)) ss[max(which(tt <= time_point))] else 1
    }
  }

  # ---- Aggregate by group ----
  split_idx <- split(seq_len(nrow(data)), groups)
  out <- lapply(names(split_idx), function(g) {
    idx <- split_idx[[g]]
    df  <- data[idx, , drop = FALSE]
    data.frame(
      RiskGroup = as.character(g),
      n = nrow(df),
      ObservedSurvival = obs_val(df),
      PredictedSurvival = mean(pred[idx]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}
