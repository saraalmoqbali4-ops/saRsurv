# ===== R/plot_km_alpha.R =====
#' Stratified KM plot with risk-based transparency
#'
#' Applies per-stratum transparency (alpha) proportional to group size.
#'
#' @param fit A `survfit` object (with or without strata).
#' @param data A data frame used to fit `fit`.
#' @param group_var Character. Column name in `data` indicating strata or groups.
#' @param transform Transformation method. One of `"log"`, `"sqrt"`, or `"linear"`.
#' @param min_alpha Lower bound for alpha, in `[0, 1]`.
#' @param max_alpha Upper bound for alpha, in `[0, 1]`.
#' @param ... Additional arguments passed to `survminer::ggsurvplot()`.
#' @return A `ggsurvplot` object with alpha mapped per stratum.
#'
#' @examples
#' \dontrun{
#' # Example using the 'lung' dataset
#' if (requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("survminer", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'
#'   data("lung", package = "survival")
#'
#'   # Convert status to 1 = event (death), 0 = censor
#'   lung$status01 <- as.integer(lung$status == 2)
#'
#'   # Fit stratified KM by sex (1/2)
#'   fit_sex <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
#'
#'   # Default (log transform): alpha scaled to group sizes
#'   gp1 <- plot_km_alpha(fit = fit_sex, data = lung, group_var = "sex")
#'   print(gp1$plot)
#'
#'   # Try other transforms and bounds
#'   gp2 <- plot_km_alpha(
#'     fit = fit_sex,
#'     data = lung,
#'     group_var = "sex",
#'     transform = "sqrt",
#'     min_alpha = 0.2,
#'     max_alpha = 0.9
#'   )
#'   print(gp2$plot)
#' }
#' }
#'
#' @export
plot_km_alpha <- function(fit, data, group_var,
                          transform = c("log","sqrt","linear"),
                          min_alpha = 0.3, max_alpha = 1, ...) {
  stopifnot(inherits(fit, "survfit"))
  stopifnot(is.data.frame(data), group_var %in% names(data))

  transform <- match.arg(transform)

  al_map <- compute_strata_alpha(data[[group_var]],
                                 transform = transform,
                                 min_alpha = min_alpha,
                                 max_alpha = max_alpha)

  p <- survminer::ggsurvplot(fit, data = data, ...)

  d <- p$plot$data
  if (!("strata" %in% names(d))) {
    d$alpha_v <- max_alpha
  } else {
    strata_chr <- as.character(d$strata)
    nm <- names(al_map)
    cleaned_levels <- sub("^.*?=", "", strata_chr)
    key <- ifelse(strata_chr %in% nm, strata_chr,
                  ifelse(cleaned_levels %in% nm, cleaned_levels, strata_chr))
    d$alpha_v <- al_map[key]
    d$alpha_v[is.na(d$alpha_v)] <- max_alpha
  }

  p$plot$data <- d
  p$plot <- p$plot +
    ggplot2::aes(alpha = .data$alpha_v) +
    ggplot2::scale_alpha_identity(guide = "none")

  p
}
