#' Risk-Adjusted Life Expectancy (RALE)
#'
#' @description
#' Computes the area under the survival curve (AUC) from time `t0` onward.
#' This value represents the risk-adjusted life expectancy - i.e., the expected
#' time remaining until an event occurs, weighted by survival probability.
#'
#' @param survfit_obj A `survfit` object.
#' @param t0 Numeric lower bound for time (default = 0).
#' @return Numeric scalar (unstratified) or a named numeric vector (per stratum).
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#'
#' f0 <- survival::survfit(survival::Surv(time, status01) ~ 1, data = lung)
#' compute_rale(f0, t0 = 0)
#'
#' fs <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
#' compute_rale(fs, t0 = 0)
#'
#' @export
compute_rale <- function(survfit_obj, t0 = 0) {
  stopifnot(inherits(survfit_obj, "survfit"))

  s <- summary(survfit_obj)
  tt <- s$time
  ss <- s$surv
  st <- s$strata

  .rale_area <- function(tt, ss, t0) {
    if (!length(tt)) return(0)
    s0 <- if (any(tt < t0)) ss[max(which(tt < t0))] else 1
    tt_after <- tt[tt >= t0]
    ss_after <- ss[tt >= t0]
    if (!length(tt_after)) return(0)
    area_first <- (tt_after[1] - t0) * s0
    area_rest  <- if (length(tt_after) > 1) sum(diff(tt_after) * head(ss_after, -1)) else 0
    area_first + area_rest
  }

  if (is.null(st)) {
    return(.rale_area(tt, ss, t0))
  }

  # Handle multiple strata
  if (length(st) == length(tt)) {
    idx_list <- split(seq_along(tt), st)
  } else {
    labs <- rep(names(st), st)
    idx_list <- split(seq_along(tt), labs)
  }

  out <- vapply(idx_list, function(ix) .rale_area(tt[ix], ss[ix], t0), numeric(1))
  return(out)
}

