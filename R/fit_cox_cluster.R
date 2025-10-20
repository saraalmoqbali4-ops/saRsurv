#' Fit Cox model with optional strata and clustering (robust / frailty / coxme)
#'
#' Fits a Cox proportional hazards model that can include stratification
#' and clustering. Supported engines:
#' - "coxph_robust": Uses `survival::coxph` with `cluster()` (robust sandwich SE)
#' - "frailty": Uses `survival::coxph` with `frailty()`
#' - "coxme": Uses `coxme::coxme` (random effect for cluster)
#'
#' The function accepts both **modern** and **legacy aliases** for parameters:
#' - `status` ? alias: `event`
#' - `strata` ? alias: `strata_var`
#' - `cluster` ? alias: `cluster_id`
#'
#' @param data A data.frame containing the survival data.
#' @param time Character. Name of the time-to-event column.
#' @param status Character. Name of the event indicator column (0/1 or 1/2 where 2 = event).
#' @param event Deprecated alias for `status`. Used only if `status` is missing.
#' @param covars Character vector of covariate column names (e.g., c("age","sex")).
#' @param strata Character or NULL. Name of stratification variable.
#' @param strata_var Deprecated alias for `strata`.
#' @param cluster Character or NULL. Name of the clustering ID variable.
#' @param cluster_id Deprecated alias for `cluster`.
#' @param engine Character. One of c("coxph_robust","frailty","coxme"). Default is "coxph_robust".
#' @param ties Character. Method for handling ties in `coxph` ("efron" by default).
#' @param event_code Optional integer. Code of event if not 0/1 (e.g., 2 if coded 1/2).
#' @return A fitted model object (from `coxph` or `coxme`) with relevant attributes.
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' lung <- survival::lung
#' lung$status01 <- as.integer(lung$status == 2)
#' lung$id <- seq_len(nrow(lung))
#'
#' # Using modern argument names
#' m1 <- fit_cox_cluster(
#'   data = lung, time = "time", status = "status01",
#'   covars = c("age","sex"), cluster = "id",
#'   engine = "coxph_robust"
#' )
#' summary(m1)
#'
#' @importFrom stats as.formula
#' @export
fit_cox_cluster <- function(data, time,
                            status = NULL, event = NULL,
                            covars = character(),
                            strata = NULL, strata_var = NULL,
                            cluster = NULL, cluster_id = NULL,
                            engine = c("coxph_robust","frailty","coxme"),
                            ties = "efron",
                            event_code = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(time %in% names(data))

  # --- Handle aliases ---
  if (is.null(status) && !is.null(event)) {
    status <- event
  } else if (!is.null(status) && !is.null(event) && !identical(status, event)) {
    warning("Both `status` and deprecated `event` provided; using `status`.")
  }
  if (is.null(strata) && !is.null(strata_var)) {
    strata <- strata_var
  } else if (!is.null(strata) && !is.null(strata_var) && !identical(strata, strata_var)) {
    warning("Both `strata` and deprecated `strata_var` provided; using `strata`.")
  }
  if (is.null(cluster) && !is.null(cluster_id)) {
    cluster <- cluster_id
  } else if (!is.null(cluster) && !is.null(cluster_id) && !identical(cluster, cluster_id)) {
    warning("Both `cluster` and deprecated `cluster_id` provided; using `cluster`.")
  }

  if (is.null(status) || !(status %in% names(data))) {
    stop("`status` (or alias `event`) must be a valid column in `data`.")
  }

  # ---- Validate covariates ----
  if (length(covars) && !all(covars %in% names(data))) {
    missing_covars <- setdiff(covars, names(data))
    stop(
      "One or more covariates not found in data: ",
      paste(missing_covars, collapse = ", ")
    )
  }

  if (!is.null(strata) && !(strata %in% names(data))) {
    stop("Stratification variable not found in data: ", strata)
  }

  if (!is.null(cluster) && !(cluster %in% names(data))) {
    stop("Cluster variable not found in data: ", cluster)
  }

  engine <- match.arg(engine)

  # --- Convert status to 0/1 ---
  st <- data[[status]]
  if (!all(st %in% c(0,1))) {
    if (!is.null(event_code)) {
      st <- as.integer(st == event_code)
    } else if (all(st %in% c(1,2))) {
      st <- as.integer(st == 2)
    } else {
      stop("`status` must be 0/1, or 1/2 (2 = event), or specify `event_code`.")
    }
  }

  # --- Build model RHS ---
  rhs <- if (length(covars)) paste(covars, collapse = " + ") else "1"
  if (!is.null(strata))  rhs <- paste(rhs, sprintf("+ strata(%s)", strata))
  if (!is.null(cluster)) {
    if (engine == "coxph_robust") rhs <- paste(rhs, sprintf("+ cluster(%s)", cluster))
    if (engine == "frailty")      rhs <- paste(rhs, sprintf("+ frailty(%s)", cluster))
  }

  # --- Build Surv object and fit ---
  data$.__status__. <- st
  surv_expr <- sprintf("survival::Surv(%s, %s)", time, ".__status__.")

  if (engine == "coxph_robust" || engine == "frailty") {
    form <- stats::as.formula(sprintf("%s ~ %s", surv_expr, rhs))
    fit  <- survival::coxph(form, data = data, ties = ties,
                            robust = (engine == "coxph_robust"),
                            model = TRUE, x = TRUE, y = TRUE)
  } else if (engine == "coxme") {
    if (!requireNamespace("coxme", quietly = TRUE))
      stop("engine='coxme' requires the 'coxme' package.")
    if (!is.null(cluster)) rhs <- paste(rhs, sprintf("+ (1|%s)", cluster))
    form <- stats::as.formula(sprintf("%s ~ %s", surv_expr, rhs))
    fit  <- coxme::coxme(form, data = data)
  } else {
    stop("`engine` must be one of 'coxph_robust', 'frailty', or 'coxme'.")
  }

  attr(fit, "saRsurv_call") <- list(fun = "fit_cox_cluster",
                                    time = time, status = status,
                                    covars = covars, strata = strata,
                                    cluster = cluster, engine = engine)
  fit
}

#' Legacy alias for backward compatibility
#' @rdname fit_cox_cluster
#' @export
survfit_strata_cluster <- function(data, time,
                                   status = NULL, event = NULL,
                                   covars = character(),
                                   strata = NULL, strata_var = NULL,
                                   cluster = NULL, cluster_id = NULL,
                                   engine = c("coxph_robust","frailty","coxme"),
                                   ties = "efron",
                                   event_code = NULL) {
  fit_cox_cluster(data = data, time = time,
                  status = status, event = event,
                  covars = covars,
                  strata = strata, strata_var = strata_var,
                  cluster = cluster, cluster_id = cluster_id,
                  engine = engine, ties = ties,
                  event_code = event_code)
}
