#' Plot RALE by Strata for a Stratified survfit Object
#'
#' @description
#' Computes the Risk-Adjusted Life Expectancy (RALE) for each stratum
#' in a stratified Kaplan-Meier fit and displays the results as a bar chart.
#'
#' @param survfit_obj A stratified `survfit` object.
#' @param t0 Numeric. Lower bound for integration (default = 0).
#' @param fill_color Character. Optional fill color palette for bars (default "steelblue").
#' @return A `ggplot` object showing RALE per stratum.
#'
#' @examplesIf (requireNamespace("survival", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE))
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#' fs <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
#' p <- plot_rale_by_strata(fs, t0 = 0)
#' print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_col labs geom_text theme_minimal
#' @export
plot_rale_by_strata <- function(survfit_obj, t0 = 0, fill_color = "steelblue") {
  # --- validation ---
  stopifnot(inherits(survfit_obj, "survfit"))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }

  strata_info <- summary(survfit_obj)$strata
  if (is.null(strata_info))
    stop("? survfit object is not stratified. Fit with a strata/group first.")

  # --- compute RALE per stratum ---
  rales <- compute_rale(survfit_obj, t0 = t0)
  df <- data.frame(Stratum = names(rales),
                   RALE = as.numeric(rales),
                   row.names = NULL)

  # --- order descending by RALE ---
  df <- df[order(df$RALE, decreasing = TRUE), ]
  df$Stratum <- factor(df$Stratum, levels = df$Stratum)

  # --- build plot ---
  ggplot2::ggplot(df, ggplot2::aes(x = Stratum, y = RALE, fill = Stratum)) +
    ggplot2::geom_col(show.legend = FALSE, color = "gray20") +
    ggplot2::geom_text(ggplot2::aes(label = round(RALE, 1)),
                       vjust = -0.3, size = 3.5, color = "black") +
    ggplot2::labs(
      title = "Risk-Adjusted Life Expectancy (RALE) by Stratum",
      subtitle = paste0("Integrated from t0 = ", t0),
      x = "Stratum",
      y = "Expected remaining time"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      axis.text.x = ggplot2::element_text(size = 10, color = "black"),
      axis.text.y = ggplot2::element_text(size = 10, color = "black")
    ) +
    ggplot2::scale_fill_manual(values = rep(fill_color, length(unique(df$Stratum))))
}
