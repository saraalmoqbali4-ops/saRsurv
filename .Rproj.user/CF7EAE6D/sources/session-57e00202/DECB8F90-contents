#' Weighted Log-Rank test (Fleming–Harrington with optional IPW)
#'
#' Computes a two-sample weighted log-rank statistic using FH weights
#' w(t) = S(t)^rho * (1 - S(t))^gamma, where S(t) is the pooled KM.
#'
#' @param data data.frame with columns for time, status, group (and optional weights).
#' @param time Character, time column name.
#' @param status Character, status column (0/1, or 1/2 with 2=event).
#' @param group Character, binary group column.
#' @param rho,gamma Non-negative FH parameters.
#' @param weight Optional character: name of numeric non-negative weights column.
#' @param ipw Optional character: **deprecated alias** for `weight`.
#' @return list(statistic, p.value, rho, gamma, method)
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#' weighted_logrank(lung, "time", "status01", "sex")
#' weighted_logrank(lung, "time", "status01", "sex", rho = 0, gamma = 1)
#' @export
weighted_logrank <- function(data, time, status, group,
                             rho = 0, gamma = 0,
                             weight = NULL, ipw = NULL) {

  stopifnot(is.data.frame(data))
  stopifnot(all(c(time, status, group) %in% names(data)))
  stopifnot(is.numeric(rho) && rho >= 0, is.numeric(gamma) && gamma >= 0)

  # back-compat: ipw alias for weight
  if (!is.null(ipw) && is.null(weight)) {
    weight <- ipw
  } else if (!is.null(ipw) && !is.null(weight) && !identical(ipw, weight)) {
    warning("Both `weight` and deprecated `ipw` were provided; using `weight`.")
  }

  x <- data[, c(time, status, group), drop = FALSE]
  names(x) <- c("time", "status", "group")

  if (length(unique(x$group)) != 2L)
    stop("`group` must have exactly two levels.")

  # subject weights
  if (is.null(weight)) {
    w <- rep(1, nrow(x))
  } else {
    if (!weight %in% names(data)) stop("`weight`/`ipw` column not found.")
    w <- data[[weight]]
    if (!is.numeric(w) || anyNA(w) || any(w < 0))
      stop("`weight`/`ipw` must be non-negative numeric without NAs.")
  }

  # coerce status to 0/1
  st <- x$status
  if (!all(st %in% c(0, 1))) {
    if (all(st %in% c(1, 2))) {
      st <- as.integer(st == 2)
    } else {
      stop("`status` must be coded 0/1 (or 1/2 where 2 means event).")
    }
  }

  df <- data.frame(time = x$time,
                   status = st,
                   group = factor(x$group),
                   wt = w)

  # order by time ascending
  o  <- order(df$time)
  df <- df[o, ]

  # unique event times
  ev_times <- sort(unique(df$time[df$status == 1]))
  if (!length(ev_times)) stop("No events present.")

  g1 <- levels(df$group)[1]; g2 <- levels(df$group)[2]

  at_risk <- function(t) c(
    y1 = sum(df$wt[df$group == g1 & df$time >= t]),
    y2 = sum(df$wt[df$group == g2 & df$time >= t])
  )
  events <- function(t) c(
    d1 = sum(df$wt[df$group == g1 & df$time == t & df$status == 1]),
    d2 = sum(df$wt[df$group == g2 & df$time == t & df$status == 1])
  )

  nT <- length(ev_times)
  Y1 <- D1 <- Y2 <- D2 <- numeric(nT)
  for (i in seq_len(nT)) {
    t <- ev_times[i]
    ar <- at_risk(t); ev <- events(t)
    Y1[i] <- ar["y1"]; Y2[i] <- ar["y2"]
    D1[i] <- ev["d1"]; D2[i] <- ev["d2"]
  }
  Y <- Y1 + Y2; D <- D1 + D2

  # pooled S(t-) step process
  S <- numeric(nT); S_prev <- 1
  for (i in seq_len(nT)) {
    S[i] <- S_prev
    if (Y[i] > 0) S_prev <- S_prev * (1 - D[i]/Y[i])
  }
  wFH <- (S^rho) * ((1 - S)^gamma)

  # expectation & variance (approximate, Greenwood-style)
  E1 <- Y1 * (D / pmax(Y, 1e-12))
  V  <- (Y1 * Y2 * D * (Y - D)) / (pmax(Y^2 * (Y - 1), 1e-12))

  Znum <- sum(wFH * (D1 - E1))
  Zden <- sum((wFH^2) * V)
  if (Zden <= 0) stop("Variance is zero; cannot compute test.")

  stat <- as.numeric((Znum^2) / Zden)
  pv   <- stats::pchisq(stat, df = 1, lower.tail = FALSE)

  list(statistic = stat, p.value = pv, rho = rho, gamma = gamma,
       method = "Fleming-Harrington weighted log-rank (two-sample)")
}
