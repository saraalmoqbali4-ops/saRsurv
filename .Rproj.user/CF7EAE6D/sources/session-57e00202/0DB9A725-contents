# ===== R/stratified_weighted_logrank.R =====
#' Stratified Weighted Log-Rank test (FH + optional IPW)
#'
#' Mantel–Haenszel aggregation of weighted log-rank scores across strata.
#'
#' @param data   data.frame with time, status, group, strata.
#' @param time,status,group,strata Character names of columns.
#' @param rho,gamma FH exponents; default 0,0.
#' @param ipw   Optional subject-level weights (column name).
#' @return list(statistic, p.value, method, rho, gamma, n, n_strata)
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' # Example with the 'lung' dataset from survival
#' lung <- survival::lung
#' # Convert to 1=event (death), 0=censor
#' lung$status01 <- as.integer(lung$status == 2)
#'
#' # Use ECOG performance as a stratification factor (drop rows with NA)
#' df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
#'
#' # 1) Standard stratified log-rank (rho=0, gamma=0)
#' res_std <- stratified_weighted_logrank(
#'   data = df, time = "time", status = "status01",
#'   group = "sex", strata = "ph.ecog", rho = 0, gamma = 0
#' )
#' res_std$p.value
#'
#' # 2) Late-emphasis weights (rho=0, gamma=1)
#' res_late <- stratified_weighted_logrank(
#'   data = df, time = "time", status = "status01",
#'   group = "sex", strata = "ph.ecog", rho = 0, gamma = 1
#' )
#' res_late$p.value
#'
#' # 3) With subject-level IPW (demo random weights)
#' set.seed(123)
#' df$ipw <- stats::runif(nrow(df), 0.5, 1.5)
#' res_ipw <- stratified_weighted_logrank(
#'   data = df, time = "time", status = "status01",
#'   group = "sex", strata = "ph.ecog", rho = 0, gamma = 0, ipw = "ipw"
#' )
#' res_ipw$p.value
#'
#' # 4) (Optional) Compare to survival::survdiff when rho=0
#' sd_fit <- survival::survdiff(
#'   survival::Surv(time, status01) ~ sex + strata(ph.ecog),
#'   data = df, rho = 0
#' )
#' p_ref <- stats::pchisq(as.numeric(sd_fit$chisq), df = 1, lower.tail = FALSE)
#' p_ref
#' @export
stratified_weighted_logrank <- function(data, time, status, group, strata,
                                        rho = 0, gamma = 0, ipw = NULL){
  stopifnot(is.data.frame(data))
  need <- c(time, status, group, strata)
  stopifnot(all(need %in% names(data)))

  d0 <- data[stats::complete.cases(data[, need, drop = FALSE]), , drop = FALSE]
  g   <- as.factor(d0[[group]])
  s   <- as.factor(d0[[strata]])
  stopifnot(nlevels(g) == 2)
  if (!is.null(ipw)) {
    stopifnot(ipw %in% names(d0))
    if (any(!is.finite(d0[[ipw]]) | d0[[ipw]] < 0)) stop("`ipw` must be finite and >= 0.")
  }

  .uv_one <- function(d){
    g <- as.factor(d[[group]]); lvl <- levels(g)
    w_subj <- if (is.null(ipw)) rep(1, nrow(d)) else d[[ipw]]
    km <- survival::survfit(survival::Surv(d[[time]], d[[status]]) ~ 1)
    Sstep <- stats::stepfun(km$time, c(1, km$surv))
    t_ev <- sort(unique(d[[time]][d[[status]] == 1]))

    U <- 0; V <- 0
    for (ti in t_ev) {
      at_risk  <- d[[time]] >= ti
      events   <- d[[time]] == ti & d[[status]] == 1

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

  list(statistic = stat, p.value = pval,
       method = "Stratified weighted log-rank (FH + optional IPW)",
       rho = rho, gamma = gamma, n = nrow(d0), n_strata = nlevels(s))
}
