#' Grid Search for (rho, gamma) in Weighted Log-Rank Test
#'
#' @description
#' Runs the Fleming-Harrington weighted log-rank test over a grid of
#' (rho, gamma) parameter combinations and returns test statistics and p-values.
#'
#' @inheritParams weighted_logrank
#' @param data A data frame containing the survival data.
#' @param time Character. Name of the time-to-event column.
#' @param status Character. Name of the event indicator column (1=event, 0=censor).
#' @param group Character. Name of the grouping variable column.
#' @param grid Optional data frame specifying combinations of `rho` and `gamma`.
#'   Default is `expand.grid(rho = c(0, 0.5, 1), gamma = c(0, 0.5, 1))`.
#' @param sort Logical. If TRUE, results are sorted by p-value ascending.
#' @param quiet Logical. If TRUE, suppresses printed messages during computation.
#'
#' @return A data frame with columns: `rho`, `gamma`, `statistic`, and `p.value`.
#' @export
logrank_grid <- function(data, time, status, group,
                         grid = expand.grid(rho = c(0, 0.5, 1),
                                            gamma = c(0, 0.5, 1)),
                         weight = NULL, ipw = NULL,
                         sort = TRUE, quiet = TRUE) {

  stopifnot(is.data.frame(data))
  stopifnot(all(c(time, status, group) %in% names(data)))
  stopifnot(all(c("rho", "gamma") %in% names(grid)))
  stopifnot(all(grid$rho >= 0), all(grid$gamma >= 0))

  res <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    rr <- grid$rho[i]
    gg <- grid$gamma[i]
    if (!quiet) message(sprintf("Running (rho=%.2f, gamma=%.2f)...", rr, gg))
    z <- weighted_logrank(data, time, status, group,
                          rho = rr, gamma = gg, weight = weight, ipw = ipw)
    res[[i]] <- data.frame(rho = rr, gamma = gg,
                           statistic = z$statistic,
                           p.value = z$p.value)
  }

  out <- do.call(rbind, res)
  rownames(out) <- NULL

  if (sort) out <- out[order(out$p.value), ]
  out
}

