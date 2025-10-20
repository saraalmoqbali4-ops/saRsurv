#' Forest Plot for Cox Model (Hazard Ratios with 95% CI)
#'
#' Creates a clean forest plot of hazard ratios from a Cox model,
#' including 95% confidence intervals and significance markers.
#'
#' @param fit A Cox model object (class `coxph`).
#' @param title Optional title for the plot.
#' @param xlab Label for the x-axis (default = "Hazard Ratio (HR)").
#' @param show_pvalue Logical; if TRUE, adds p-values as labels.
#' @return A `ggplot` object.
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)
#' data(heart, package = "survival")
#' fit <- survival::coxph(survival::Surv(start, stop, event) ~ age + year + surgery, data = heart)
#' p <- plot_cox_forest(fit, title = "Cox Time-Dependent Model")
#' print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_vline scale_x_log10 coord_flip labs theme_minimal
#' @export
plot_cox_forest <- function(fit, title = NULL, xlab = "Hazard Ratio (HR)", show_pvalue = TRUE) {
  stopifnot(inherits(fit, "coxph"))

  # Extract summary
  sm <- summary(fit)
  df <- data.frame(
    Variable = rownames(sm$coefficients),
    HR = exp(sm$coefficients[, "coef"]),
    Lower = exp(sm$conf.int[, "lower .95"]),
    Upper = exp(sm$conf.int[, "upper .95"]),
    pvalue = sm$coefficients[, "Pr(>|z|)"]
  )

  # Sort by HR descending
  df <- df[order(df$HR, decreasing = TRUE), ]
  df$Variable <- factor(df$Variable, levels = df$Variable)

  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Variable, y = HR, ymin = Lower, ymax = Upper)) +
    ggplot2::geom_pointrange(color = "steelblue", size = 0.7) +
    ggplot2::geom_vline(xintercept = NA) +  # just placeholder for syntax clarity
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 0.6) +
    ggplot2::scale_y_log10() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = title %||% "Cox Model Hazard Ratios",
      x = NULL,
      y = xlab
    ) +
    ggplot2::theme_minimal()

  # Optional p-values as text labels
  if (isTRUE(show_pvalue)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0("p = ", sprintf("%.3f", pvalue))),
      hjust = -0.1,
      size = 3
    )
  }

  return(p)
}
