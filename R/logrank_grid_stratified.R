#' Grid search over (rho, gamma) for the stratified weighted log-rank test
#'
#' Performs multiple stratified Fleming-Harrington weighted log-rank tests
#' for a grid of (rho, gamma) parameters and returns their statistics and p-values.
#'
#' @inheritParams stratified_weighted_logrank
#' @param data A data frame containing survival data.
#' @param time Character. Name of the time column.
#' @param status Character. Name of the event indicator column (1=event, 0=censor).
#' @param group Character. Name of the treatment or group variable.
#' @param strata Character. Name of the stratification variable.
#' @param grid Optional data frame specifying combinations of `rho` and `gamma`.
#'
#' @return A data frame summarizing per-grid test statistics and p-values.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   data(lung, package = "survival")
#'   lung$status01 <- as.integer(lung$status == 2)
#'   df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
#'
#'   # Default grid
#'   gdf <- logrank_grid_stratified(df, time = "time", status = "status01",
#'                                  group = "sex", strata = "ph.ecog")
#'   head(gdf)
#'
#'   # Custom grid
#'   grd <- expand.grid(rho = c(0, 1), gamma = c(0, 1))
#'   gdf2 <- logrank_grid_stratified(df, "time", "status01", "sex", "ph.ecog", grid = grd)
#'   gdf2[order(gdf2$p.value), ][1, ]  # smallest p-value
#' }}
#' @export
logrank_grid_stratified <- function(data, time, status, group, strata,
                                    grid = expand.grid(rho = c(0, 0.5, 1),
                                                       gamma = c(0, 0.5, 1)),
                                    ipw = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(all(c("rho", "gamma") %in% names(grid)))

  res <- lapply(seq_len(nrow(grid)), function(i) {
    rr <- grid$rho[i]
    gg <- grid$gamma[i]

    z <- stratified_weighted_logrank(
      data   = data,
      time   = time,
      status = status,
      group  = group,
      strata = strata,
      rho    = rr,
      gamma  = gg,
      ipw    = ipw
    )

    data.frame(
      rho = rr,
      gamma = gg,
      statistic = z$statistic,
      p.value = z$p.value
    )
  })

  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

