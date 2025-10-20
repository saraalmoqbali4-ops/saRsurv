# ===== R/plot_km_alpha.R =====
#' Stratified Kaplan-Meier Plot with Risk-Based Transparency
#'
#' @description
#' Produces a Kaplan-Meier plot where each stratum (group) is displayed
#' with transparency proportional to its sample size.
#' Larger groups appear more opaque, smaller ones more transparent.
#'
#' @param fit A `survfit` object (with or without strata).
#' @param data A data frame used to fit `fit`.
#' @param group_var Character. Column name in `data` indicating strata or groups.
#' @param transform Transformation method for alpha scaling.
#'   One of `"log"`, `"sqrt"`, `"linear"`, `"power"`, or `"reverse"`.
#' @param power Numeric exponent used when `transform = "power"`. Default = 0.5.
#' @param min_alpha Lower bound for alpha, in `[0, 1]`.
#' @param max_alpha Upper bound for alpha, in `[0, 1]`.
#' @param return_plot Logical; if TRUE, returns only the ggplot object.
#' @param ... Additional arguments passed to [survminer::ggsurvplot()].
#'
#' @return A `ggsurvplot` object (or `ggplot` if `return_plot = TRUE`),
#' with per-stratum alpha mapped according to group sizes.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("survival", quietly = TRUE) &&
#'     requireNamespace("survminer", quietly = TRUE)) {
#'
#'   data("lung", package = "survival")
#'   lung$status01 <- as.integer(lung$status == 2)
#'   fit <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
#'
#'   # Log scaling (default)
#'   gp <- plot_km_alpha(fit, lung, "sex")
#'   print(gp$plot)
#' }
#' }
#' @export
plot_km_alpha <- function(fit, data, group_var,
                          transform = c("log", "sqrt", "linear", "power", "reverse"),
                          power = 0.5,
                          min_alpha = 0.3,
                          max_alpha = 1,
                          return_plot = FALSE,
                          ...) {
  # --- Validation ---
  stopifnot(inherits(fit, "survfit"))
  stopifnot(is.data.frame(data))
  if (!group_var %in% names(data)) {
    stop(paste0("? group_var '", group_var, "' not found in data."))
  }
  if (!requireNamespace("survminer", quietly = TRUE)) {
    stop("Package 'survminer' is required for this function.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.")
  }

  transform <- match.arg(transform)

  # --- Compute alpha per stratum ---
  al_map <- compute_strata_alpha(data[[group_var]],
                                 transform = transform,
                                 power = power,
                                 min_alpha = min_alpha,
                                 max_alpha = max_alpha)
  message("? Computed alpha per group: ",
          paste0(names(al_map), "=", sprintf("%.2f", al_map), collapse = ", "))

  # --- Create initial ggsurvplot ---
  p <- survminer::ggsurvplot(fit, data = data, ...)

  # --- Extract plotted data ---
  d <- p$plot$data
  if (!("strata" %in% names(d))) {
    d$alpha_v <- max_alpha
  } else {
    strata_chr <- as.character(d$strata)
    nm <- names(al_map)

    # handle patterns like "sex=1" or "group=Male"
    cleaned_levels <- sub("^.*?=", "", strata_chr)
    key <- ifelse(strata_chr %in% nm, strata_chr,
                  ifelse(cleaned_levels %in% nm, cleaned_levels, strata_chr))

    d$alpha_v <- al_map[key]
    d$alpha_v[is.na(d$alpha_v)] <- max_alpha
  }

  # --- Update ggplot data and aesthetics ---
  p$plot$data <- d
  p$plot <- p$plot +
    ggplot2::aes(alpha = .data$alpha_v) +
    ggplot2::scale_alpha_identity(guide = "none")

  if (return_plot) return(p$plot)
  p
}

