#' Heatmap of -log10(p) across FH (rho, gamma)
#'
#' @param grid_df Output of `logrank_grid()` or `logrank_grid_stratified()`.
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#'   if (requireNamespace("survival", quietly = TRUE) &&
#'       requireNamespace("ggplot2", quietly = TRUE)) {
#'     data(lung, package = "survival")
#'     lung$status01 <- as.integer(lung$status == 2)
#'
#'     # Non-stratified heatmap
#'     gdf <- logrank_grid(lung, "time", "status01", "sex")
#'     print(plot_logrank_grid(gdf))
#'
#'     # Stratified heatmap
#'     df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
#'     gdf_s <- logrank_grid_stratified(df, "time", "status01", "sex", "ph.ecog")
#'     print(plot_logrank_grid(gdf_s))
#'   }
#' }
#' @export
plot_logrank_grid <- function(grid_df){
  stopifnot(all(c("rho","gamma","p.value") %in% names(grid_df)))
  grid_df$mlog10p <- -log10(grid_df$p.value)
  ggplot2::ggplot(grid_df, ggplot2::aes(x = rho, y = gamma, fill = mlog10p)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", p.value))) +
    ggplot2::scale_fill_continuous(name = "-log10(p)") +
    ggplot2::labs(title = "FH weighted log-rank grid",
                  x = "rho", y = "gamma") +
    ggplot2::theme_minimal()
}
