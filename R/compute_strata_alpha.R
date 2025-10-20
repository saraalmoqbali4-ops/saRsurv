#' Compute alpha (transparency) values per stratum based on group sizes
#'
#' @description
#' Computes alpha (transparency) values for each stratum based on its relative
#' group size. Larger groups receive higher alpha (more opaque), while smaller
#' ones are rendered more transparent. This helps balance the visibility of
#' groups with unequal sample sizes in survival plots or other visualizations.
#'
#' @details
#' This function supports multiple transformation methods (`"log"`, `"sqrt"`,
#' `"linear"`, `"power"`, and `"reverse"`) and allows reversed scaling or user-defined power.
#' Missing values are ignored with a warning.
#'
#' @param x A vector (factor, character, or numeric) of group labels.
#' @param transform Transformation method. One of `"log"`, `"sqrt"`,
#'   `"linear"`, `"power"`, or `"reverse"`.
#' @param power Numeric exponent used when `transform = "power"`. Default = 0.5.
#' @param min_alpha,max_alpha Lower and upper alpha bounds in `[0, 1]`.
#' @param return_df Logical; if `TRUE`, returns a data frame instead of a named vector.
#' @return Either a named numeric vector or a data frame with columns:
#'   \describe{
#'     \item{stratum}{Group names}
#'     \item{alpha}{Alpha (transparency) values}
#'   }
#' @examples
#' groups <- c(rep("A", 20), rep("B", 50), rep("C", 200))
#' compute_strata_alpha(groups)
#' compute_strata_alpha(groups, transform = "sqrt")
#' compute_strata_alpha(groups, transform = "power", power = 0.25)
#' @export
compute_strata_alpha <- function(x,
                                 transform = c("log", "sqrt", "linear", "power", "reverse"),
                                 power = 0.5,
                                 min_alpha = 0.3,
                                 max_alpha = 1,
                                 return_df = FALSE) {
  # --- Input validation ---
  if (min_alpha < 0 || min_alpha > 1 || max_alpha < 0 || max_alpha > 1) {
    stop("? `min_alpha` and `max_alpha` must be within [0, 1].")
  }
  if (min_alpha >= max_alpha) {
    stop("? `min_alpha` must be smaller than `max_alpha`.")
  }

  transform <- match.arg(transform)

  # --- Handle missing values ---
  if (any(is.na(x))) {
    warning("?? Missing values detected in `x`; they were ignored.")
    x <- x[!is.na(x)]
  }

  # --- Handle single group edge case ---
  if (length(unique(x)) == 1) {
    alpha_single <- setNames(max_alpha, unique(x))
    if (return_df)
      return(data.frame(stratum = names(alpha_single), alpha = as.numeric(alpha_single)))
    return(alpha_single)
  }

  # --- Compute frequencies ---
  tab <- table(x)
  n <- as.numeric(tab)

  # --- Apply transformation ---
  n_tr <- switch(transform,
                 log = log(pmax(n, 1)),
                 sqrt = sqrt(n),
                 linear = n,
                 power = n^power,
                 reverse = n)

  # --- Normalize (scaling to [0,1]) ---
  if (length(unique(n_tr)) == 1) {
    s <- rep(1, length(n_tr))
  } else {
    s <- (n_tr - min(n_tr)) / (max(n_tr) - min(n_tr))
  }

  # --- Reverse scaling if requested ---
  if (transform == "reverse") {
    s <- 1 - s
  }

  # --- Compute final alpha values ---
  alphas <- min_alpha + s * (max_alpha - min_alpha)
  names(alphas) <- names(tab)

  # --- Return as vector or data frame ---
  if (return_df) {
    return(data.frame(stratum = names(alphas),
                      alpha = as.numeric(alphas),
                      row.names = NULL))
  }

  alphas
}

