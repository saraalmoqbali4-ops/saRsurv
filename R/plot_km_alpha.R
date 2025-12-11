#' Stratified Kaplan-Meier Plot with Risk-Based Transparency (Weighted or Unweighted)
#'
#' @description
#' Produces a Kaplan-Meier plot where each group is displayed with alpha
#' proportional to its sample size. If weights are provided, the function
#' automatically computes *weighted* KM curves and plots them.
#'
#' @param fit A survfit object or NULL (ignored when weights are supplied).
#' @param data Data frame.
#' @param group_var Character, column name in data.
#' @param weights Optional numeric vector of case weights.
#' @param transform Method for alpha scaling.
#' @param power Exponent for "power" transform.
#' @param min_alpha Minimum transparency.
#' @param max_alpha Maximum transparency.
#' @param return_plot Return ggplot only.
#' @param ...
#'
#' @export
plot_km_alpha <- function(fit = NULL,
                          data,
                          group_var,
                          weights = NULL,
                          transform = c("log", "sqrt", "linear", "power", "reverse"),
                          power = 0.5,
                          min_alpha = 0.3,
                          max_alpha = 1,
                          return_plot = FALSE,
                          ...) {

  library(ggplot2)
  library(dplyr)

  # -----------------------
  # 1) Compute alpha values
  # -----------------------
  transform <- match.arg(transform)

  al_map <- compute_strata_alpha(
    data[[group_var]],
    transform = transform,
    power = power,
    min_alpha = min_alpha,
    max_alpha = max_alpha
  )

  message("? Computed alpha per group: ",
          paste0(names(al_map), "=", sprintf("%.2f", al_map), collapse = ", "))

  # --------------------------------------------------------
  # CASE 1: Weighted KM — manual computation (CUSTOM LOGIC)
  # --------------------------------------------------------
  if (!is.null(weights)) {

    message("? Drawing weighted KM curve (custom saRsurv method).")

    df <- data %>%
      mutate(
        group = .data[[group_var]],
        time  = .data[[attr(fit, "call")$time]],
        status = .data[[attr(fit, "call")$status]],
        w = weights
      )

    groups <- unique(df$group)
    collector <- list()

    # Manual KM weighted for each group
    for (g in groups) {
      sub <- df %>% filter(group == g)

      # Unique event times
      event_times <- sort(unique(sub$time[sub$status == 1]))
      S <- 1
      surv_list <- numeric(length(event_times))

      for (i in seq_along(event_times)) {
        t <- event_times[i]
        d_t <- sum(sub$w[sub$time == t & sub$status == 1])   # weighted deaths
        Y_t <- sum(sub$w[sub$time >= t])                     # weighted risk set
        S <- S * (1 - d_t / Y_t)
        surv_list[i] <- S
      }

      collector[[g]] <- data.frame(
        time = event_times,
        surv = surv_list,
        group = g,
        alpha_v = al_map[g]
      )
    }

    res <- bind_rows(collector)

    p <- ggplot(res, aes(x = time, y = surv, color = factor(group),
                         alpha = alpha_v)) +
      geom_step(linewidth = 1.0) +
      scale_alpha_identity() +
      labs(
        title = "Weighted Kaplan–Meier Curves with Alpha Transparency",
        x = "Time",
        y = "Survival Probability",
        color = group_var
      ) +
      theme_minimal(base_size = 14)

    if (return_plot) return(p)
    return(p)
  }

  # --------------------------------------------------------
  # CASE 2: Normal (unweighted) KM — use ggsurvplot()
  # --------------------------------------------------------
  message("? Drawing unweighted KM using survminer.")

  if (is.null(fit)) stop("fit must be supplied when weights = NULL.")

  if (!requireNamespace("survminer", quietly = TRUE))
    stop("Package 'survminer' required.")

  p <- survminer::ggsurvplot(fit, data = data, ...)

  d <- p$plot$data
  strata_chr <- as.character(d$strata)
  cleaned <- sub("^.*?=", "", strata_chr)

  key <- ifelse(cleaned %in% names(al_map), cleaned, strata_chr)

  d$alpha_v <- al_map[key]
  d$alpha_v[is.na(d$alpha_v)] <- max_alpha

  p$plot$data <- d
  p$plot <- p$plot +
    aes(alpha = .data$alpha_v) +
    scale_alpha_identity(guide = "none")

  if (return_plot) return(p$plot)
  p
}
