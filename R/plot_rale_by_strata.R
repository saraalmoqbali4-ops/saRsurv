#' Plot RALE by strata for a stratified survfit
#'
#' Computes RALE per stratum and displays a bar chart.
#'
#' @param survfit_obj A stratified `survfit` object.
#' @param t0 Lower bound for integration (default 0).
#' @return A `ggplot` object.
#'
#' @examplesIf (requireNamespace("survival", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE))
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#' fs <- survival::survfit(survival::Surv(time, status01) ~ sex, data = lung)
#' p <- plot_rale_by_strata(fs, t0 = 0)
#' # print(p)
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @export
plot_rale_by_strata <- function(survfit_obj, t0 = 0){
  if (is.null(summary(survfit_obj)$strata))
    stop("survfit object is not stratified. Fit with a strata/group first.")

  rales <- compute_rale(survfit_obj, t0 = t0)        # named vector
  df <- data.frame(Stratum = names(rales), RALE = as.numeric(rales), row.names = NULL)

  ggplot2::ggplot(df, ggplot2::aes(x = Stratum, y = RALE)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "RALE by Stratum", x = NULL, y = "Expected remaining time") +
    ggplot2::theme_minimal()
}
