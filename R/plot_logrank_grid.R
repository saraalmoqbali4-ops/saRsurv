#' Heatmap of -log10(p) across FH (rho, gamma)
#'
#' Visualizes results of `logrank_grid()` or `logrank_grid_stratified()`
#' as a heatmap of -log10(p-values) over the (rho, gamma) parameter grid.
#'
#' @param grid_df A data.frame output from `logrank_grid()` or
#'   `logrank_grid_stratified()`, containing at least columns `rho`,
#'   `gamma`, and `p.value`.
#' @return A `ggplot` object showing a heatmap of -log10(p).
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   data(lung, package = "survival")
#'   lung$status01 <- as.integer(lung$status == 2)
#'
#'   # Non-stratified example
#'   gdf <- logrank_grid(lung, "time", "status01", "sex")
#'   print(plot_logrank_grid(gdf))
#'
#'   # Stratified example
#'   df <- lung[complete.cases(lung[, c("time","status01","sex","ph.ecog")]), ]
#'   gdf_s <- logrank_grid_stratified(df, "time", "status01", "sex", "ph.ecog")
#'   print(plot_logrank_grid(gdf_s))
#' }}
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_viridis_c
#'   labs theme_minimal
#' @export
plot_logrank_grid <- function(grid_df) {
  stopifnot(is.data.frame(grid_df))
  stopifnot(all(c("rho", "gamma", "p.value") %in% names(grid_df)))

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for this function.")

  grid_df$mlog10p <- -log10(pmax(grid_df$p.value, 1e-16)) # avoid Inf

  ggplot2::ggplot(grid_df, ggplot2::aes(x = factor(rho), y = factor(gamma), fill = mlog10p)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", p.value)),
                       size = 3.3, color = "black") +
    ggplot2::scale_fill_viridis_c(name = expression(-log[10](p)), option = "C") +
    ggplot2::labs(
      title = "Fleming-Harrington Weighted Log-Rank Grid",
      x = expression(rho),
      y = expression(gamma)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      axis.text = ggplot2::element_text(size = 10, color = "black"),
      panel.grid = ggplot2::element_blank()
    )
}
