#' Compute alpha values per stratum based on group sizes
#'
#' @param x A vector (factor or character) of group labels.
#' @param transform Transformation method. One of `"log"`, `"sqrt"`, or `"linear"`.
#' @param min_alpha,max_alpha Lower and upper bounds in `[0, 1]`.
#' @return A named numeric vector of alpha values, with names corresponding to group levels.
#' @export
compute_strata_alpha <- function(x,
                                 transform = c("log","sqrt","linear"),
                                 min_alpha = 0.3,
                                 max_alpha = 1) {
  stopifnot(min_alpha >= 0, max_alpha <= 1, min_alpha < max_alpha)
  transform <- match.arg(transform)
  tab <- table(x)
  n <- as.numeric(tab)

  n_tr <- switch(transform,
                 log    = log(pmax(n, 1)),
                 sqrt   = sqrt(n),
                 linear = n)

  if (length(unique(n_tr)) == 1) {
    s <- rep(1, length(n_tr))
  } else {
    s <- (n_tr - min(n_tr)) / (max(n_tr) - min(n_tr))
  }

  alphas <- min_alpha + s * (max_alpha - min_alpha)
  names(alphas) <- names(tab)
  alphas
}
