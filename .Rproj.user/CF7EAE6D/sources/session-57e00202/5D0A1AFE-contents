#' Plot observed vs predicted survival (calibration curve)
#'
#' @param cal_df Data frame returned by `calibrate_surv()`.
#' @return A `ggplot` object showing observed vs predicted survival per group.
#'
#' @examples
#' \dontrun{
#'   data(lung, package = "survival")
#'   lung$status01 <- as.integer(lung$status == 2)
#'   cfit <- survival::coxph(survival::Surv(time, status01) ~ age + sex, data = lung)
#'   cal <- calibrate_surv(cfit, lung, "time", "status01", time_point = 365)
#'   p <- plot_calibration_surv(cal); print(p)
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_abline
#'   scale_x_continuous scale_y_continuous labs theme_minimal
#' @export
plot_calibration_surv <- function(cal_df){
  stopifnot(is.data.frame(cal_df),
            all(c("ObservedSurvival","PredictedSurvival") %in% names(cal_df)))

  ggplot2::ggplot(
    cal_df,
    ggplot2::aes(x = PredictedSurvival, y = ObservedSurvival, group = 1)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(0,1)) +
    ggplot2::scale_y_continuous(limits = c(0,1)) +
    ggplot2::labs(
      x = "Predicted survival",
      y = "Observed survival",
      title = "Predictive calibration of survival"
    ) +
    ggplot2::theme_minimal()
}
