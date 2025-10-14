#' Grid search over FH (rho, gamma) for the weighted log-rank test
#'
#' @inheritParams weighted_logrank
#' @param grid Optional `data.frame` with columns `rho` and `gamma`. If `NULL`,
#'   the default grid is \code{expand.grid(rho = c(0, 0.5, 1), gamma = c(0, 0.5, 1))}.
#' @return A `data.frame` with columns: `rho`, `gamma`, `statistic`, `p.value`.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   data(lung, package = "survival")
#'   lung$status01 <- as.integer(lung$status == 2)
#'
#'   ## Default grid: expand.grid(rho = c(0, 0.5, 1), gamma = c(0, 0.5, 1))
#'   gdf <- logrank_grid(lung, time = "time", status = "status01", group = "sex")
#'   head(gdf)
#'
#'   ## Custom grid
#'   grd <- expand.grid(rho = c(0, 1), gamma = c(0, 1))
#'   gdf2 <- logrank_grid(lung, "time", "status01", "sex", grid = grd)
#'   gdf2[order(gdf2$p.value), ][1L, ]
#' }}
#'
#' @export
logrank_grid <- function(data, time, status, group,
                         grid = expand.grid(rho = c(0, 0.5, 1),
                                            gamma = c(0, 0.5, 1))) {

  stopifnot(all(c("rho", "gamma") %in% names(grid)))

  res <- lapply(seq_len(nrow(grid)), function(i) {
    rr <- grid$rho[i]
    gg <- grid$gamma[i]
    z  <- weighted_logrank(data, time, status, group, rho = rr, gamma = gg)
    data.frame(rho = rr, gamma = gg, statistic = z$statistic, p.value = z$p.value)
  })

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

