#' Plot Stratified Weighted Log-Rank Components
#'
#' Displays per-stratum weighted log-rank statistics and/or p-values
#' from the Fleming-Harrington test. Useful for identifying which
#' strata contribute most to the overall result.
#'
#' @param data data.frame with time, status, group, strata.
#' @param time,status,group,strata Character column names.
#' @param rho,gamma FH exponents (default 0, 0).
#' @param ipw Optional subject-level weights (column name).
#' @param metric Character; either `"statistic"`, `"pvalue"`, or `"both"`.
#' @param facet_by Logical; if TRUE and metric = "both", shows both in facets.
#' @return A `ggplot` object.
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#' df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
#' p <- plot_stratified_logrank(df, "time", "status01", "sex", "ph.ecog", metric = "both", facet_by = TRUE)
#' print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_text facet_wrap labs theme_minimal
#' @export
plot_stratified_logrank <- function(data, time, status, group, strata,
                                    rho = 0, gamma = 0, ipw = NULL,
                                    metric = c("statistic", "pvalue", "both"),
                                    facet_by = FALSE) {

  metric <- match.arg(metric)
  stopifnot(is.data.frame(data))
  stopifnot(all(c(time, status, group, strata) %in% names(data)))

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for this function.")

  d0 <- data[stats::complete.cases(data[, c(time, status, group, strata), drop = FALSE]), ]
  s  <- as.factor(d0[[strata]])

  # helper: compute per-stratum (stat, p)
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
      ni <- n0 + n1; di <- d0 + d1
      if (ni <= 1 || di == 0) next
      wt <- (Sstep(ti)^rho) * ((1 - Sstep(ti))^gamma)
      Ui <- wt * (d1 - n1 * di / ni)
      Vi <- (wt^2) * (n0 * n1 * di * (ni - di)) / (ni^2 * (ni - 1))
      U <- U + Ui; V <- V + Vi
    }
    stat <- if (V > 0) (U^2) / V else NA_real_
    pv <- if (is.na(stat)) NA_real_ else stats::pchisq(stat, df = 1, lower.tail = FALSE)
    c(statistic = stat, p.value = pv)
  }

  # per-stratum results
  out <- do.call(rbind, lapply(levels(s), function(lev) {
    d <- d0[s == lev, , drop = FALSE]
    if (nrow(d) < 5) return(data.frame(Stratum = lev, statistic = NA, p.value = NA))
    uv <- .uv_one(d)
    data.frame(Stratum = lev, statistic = uv["statistic"], p.value = uv["p.value"])
  }))
  out <- as.data.frame(out)
  out$Stratum <- factor(out$Stratum, levels = out$Stratum)

  # reshape for facets if needed
  if (metric == "both" && facet_by) {
    out_long <- tidyr::pivot_longer(out, cols = c("statistic", "p.value"), names_to = "Metric", values_to = "Value")
    p <- ggplot2::ggplot(out_long, ggplot2::aes(x = Stratum, y = Value, fill = Stratum)) +
      ggplot2::geom_col(show.legend = FALSE, color = "gray30") +
      ggplot2::geom_text(ggplot2::aes(label = round(Value, 3)), vjust = -0.3, size = 3.3) +
      ggplot2::facet_wrap(~Metric, scales = "free_y") +
      ggplot2::labs(title = "Per-Stratum Weighted Log-Rank", x = "Stratum", y = "Value") +
      ggplot2::theme_minimal()
  } else {
    ycol <- if (metric == "pvalue") "p.value" else "statistic"
    p <- ggplot2::ggplot(out, ggplot2::aes(x = Stratum, y = .data[[ycol]], fill = Stratum)) +
      ggplot2::geom_col(show.legend = FALSE, color = "gray30") +
      ggplot2::geom_text(ggplot2::aes(label = round(.data[[ycol]], 3)), vjust = -0.3, size = 3.3) +
      ggplot2::labs(
        title = paste0("Per-Stratum Weighted Log-Rank (", ycol, ")"),
        x = "Stratum", y = ycol
      ) +
      ggplot2::theme_minimal()
  }

  p
}
