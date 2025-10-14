#' Grid over (rho, gamma) for the stratified weighted log-rank
#'
#' @inheritParams stratified_weighted_logrank
#' @param grid Optional data.frame with rho, gamma; default {0,0.5,1}x{0,0.5,1}.
#' @return data.frame with rho, gamma, statistic, p.value
#'
#' @examples
#' \dontrun{
#'   if (requireNamespace("survival", quietly = TRUE)) {
#'     data(lung, package = "survival")
#'     lung$status01 <- as.integer(lung$status == 2)
#'     # Use ECOG performance as strata; drop NAs
#'     df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
#'
#'     # Default grid
#'     gdf <- logrank_grid_stratified(df, time = "time", status = "status01",
#'                                    group = "sex", strata = "ph.ecog")
#'     head(gdf)
#'
#'     # Custom grid
#'     grd <- expand.grid(rho = c(0, 1), gamma = c(0, 1))
#'     gdf2 <- logrank_grid_stratified(df, "time", "status01", "sex", "ph.ecog", grid = grd)
#'     # Best (smallest p-value)
#'     gdf2[order(gdf2$p.value), ][1, ]
#'   }
#' }
#' @export
logrank_grid_stratified <- function(data, time, status, group, strata,
                                    grid = expand.grid(rho = c(0, 0.5, 1),
                                                       gamma = c(0, 0.5, 1))){
  stopifnot(all(c("rho","gamma") %in% names(grid)))
  res <- lapply(seq_len(nrow(grid)), function(i){
    rr <- grid$rho[i]; gg <- grid$gamma[i]
    z  <- stratified_weighted_logrank(data, time, status, group, strata,
                                      rho = rr, gamma = gg)
    data.frame(rho = rr, gamma = gg,
               statistic = z$statistic, p.value = z$p.value)
  })
  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}
