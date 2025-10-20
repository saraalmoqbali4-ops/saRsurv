#' Stratified Weighted Log-Rank Test (Fleming-Harrington + optional IPW)
#'
#' Aggregates per-stratum weighted log-rank scores (U, V)
#' using Mantel-Haenszel pooling.
#'
#' @param data A data frame containing time, status, group, and strata variables.
#' @param time Character. Name of the time variable.
#' @param status Character. Name of the event indicator (1=event, 0=censor).
#' @param group Character. Name of the treatment or group variable.
#' @param strata Character. Name of the stratification variable.
#' @param rho Numeric. Fleming–Harrington rho parameter.
#' @param gamma Numeric. Fleming–Harrington gamma parameter.
#' @param ipw Optional column name for inverse probability weights.
#' @param return_df Logical. If TRUE, returns per-stratum statistics as a data frame.
#'
#' @return A list with components: statistic, p.value, method, rho, gamma, n, n_strata.
#' @export
stratified_weighted_logrank <- function(data, time, status, group, strata,
                                        rho = 0, gamma = 0, ipw = NULL,
                                        return_df = FALSE) {
  stopifnot(is.data.frame(data))
  need <- c(time, status, group, strata)
  stopifnot(all(need %in% names(data)))

  d0 <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]
  g   <- as.factor(d0[[group]])
  s   <- as.factor(d0[[strata]])
  stopifnot(nlevels(g) == 2)

  if (!is.null(ipw)) {
    stopifnot(ipw %in% names(d0))
    if (any(!is.finite(d0[[ipw]]) | d0[[ipw]] < 0))
      stop("`ipw` must be finite and >= 0.")
  }

  .uv_one <- function(d) {
    g <- as.factor(d[[group]]); lvl <- levels(g)
    w_subj <- if (is.null(ipw)) rep(1, nrow(d)) else d[[ipw]]

    km <- survival::survfit(survival::Surv(d[[time]], d[[status]]) ~ 1)
    Sstep <- stats::stepfun(km$time, c(1, km$surv))
    t_ev <- sort(unique(d[[time]][d[[status]] == 1]))

    U <- 0; V <- 0
    for (ti in t_ev) {
      at_risk <- d[[time]] >= ti
      events  <- d[[time]] == ti & d[[status]] == 1

      n0 <- sum(w_subj[at_risk & g == lvl[1]])
      n1 <- sum(w_subj[at_risk & g == lvl[2]])
      d0 <- sum(w_subj[events   & g == lvl[1]])
      d1 <- sum(w_subj[events   & g == lvl[2]])

      ni <- n0 + n1
      di <- d0 + d1
      if (ni <= 1 || di == 0) next

      wt <- (Sstep(ti)^rho) * ((1 - Sstep(ti))^gamma)
      Ui <- wt * (d1 - n1 * di / ni)
      Vi <- (wt^2) * (n0 * n1 * di * (ni - di)) / (ni^2 * (ni - 1))

      U <- U + Ui
      V <- V + Vi
    }
    c(U = U, V = V)
  }

  Utot <- 0; Vtot <- 0
  for (lev in levels(s)) {
    d <- d0[s == lev, , drop = FALSE]
    if (nrow(d) < 2) next
    uv <- .uv_one(d)
    Utot <- Utot + uv["U"]
    Vtot <- Vtot + uv["V"]
  }

  stat <- if (Vtot > 0) as.numeric((Utot^2) / Vtot) else NA_real_
  pval <- if (is.na(stat)) NA_real_ else stats::pchisq(stat, df = 1, lower.tail = FALSE)

  out <- list(
    statistic = stat,
    p.value   = pval,
    rho = rho, gamma = gamma,
    n = nrow(d0),
    n_strata = nlevels(s),
    method = "Stratified weighted log-rank (FH + optional IPW)"
  )

  if (return_df) out <- as.data.frame(out, stringsAsFactors = FALSE)
  out
}
