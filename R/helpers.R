# helpers.R
#
# Internal helper functions shared across the package.
# These extract duplicated patterns to promote DRY code and maintainability.

# =============================================================================
# Parameter Extraction Helpers
# =============================================================================

#' Extract a Single Parameter Estimate with SE and CI
#'
#' Internal helper to extract estimate, standard error, and confidence interval
#' bounds from a single column of a mirt coefficient matrix.
#'
#' @param mat A coefficient matrix from `mirt::coef()` with rows "par", "CI_2.5",
#'   "CI_97.5" and column names matching mirt's parameter naming.
#' @param col_name Character string: the column name to extract (e.g., "a1", "d", "d2").
#'
#' @return A list with elements:
#'   \describe{
#'     \item{est}{The parameter estimate (from "par" row).}
#'     \item{se}{The standard error derived from CI width, or NA if CI rows absent.}
#'     \item{ci_lower}{Lower CI bound (from "CI_2.5" row), or NA if absent.}
#'     \item{ci_upper}{Upper CI bound (from "CI_97.5" row), or NA if absent.}
#'   }
#'
#' @keywords internal
extract_one_param <- function(mat, col_name) {
  z_crit <- stats::qnorm(0.975)

  est <- mat["par", col_name]
  if ("CI_2.5" %in% rownames(mat) && "CI_97.5" %in% rownames(mat)) {
    ci_lo <- mat["CI_2.5", col_name]
    ci_hi <- mat["CI_97.5", col_name]
    se <- (ci_hi - ci_lo) / (2 * z_crit)
  } else {
    ci_lo <- NA_real_
    ci_hi <- NA_real_
    se <- NA_real_
  }
  list(est = est, se = se, ci_lower = ci_lo, ci_upper = ci_hi)
}


#' Convert Mirt's d Parameter to IRT Difficulty b
#'
#' Applies the delta method to convert d (intercept) to b (difficulty) under
#' the 2PL/GRM parameterization: b = -d / a. Propagates standard errors and
#' confidence intervals correctly.
#'
#' @param a_info A list with elements `est`, `se`, `ci_lower`, `ci_upper` for
#'   the discrimination parameter a. Typically from `extract_one_param()`.
#' @param d_info A list with elements `est`, `se`, `ci_lower`, `ci_upper` for
#'   the intercept parameter d. Typically from `extract_one_param()`.
#'
#' @return A list with elements `est`, `se`, `ci_lower`, `ci_upper` for the
#'   converted difficulty parameter b, using delta method variance propagation.
#'
#' @details
#' The delta method variance for b = -d/a is:
#' \deqn{\text{Var}(b) = \frac{\text{Var}(d)}{a^2} + \frac{d^2 \text{Var}(a)}{a^4}}
#'
#' CI bounds are transformed directly: b_ci = -d_ci / a.
#'
#' @keywords internal
convert_d_to_b <- function(a_info, d_info) {
  b_est <- -d_info$est / a_info$est
  b_se <- sqrt((d_info$se / a_info$est)^2 + (d_info$est * a_info$se / a_info$est^2)^2)
  b_ci_lower <- -d_info$ci_upper / a_info$est
  b_ci_upper <- -d_info$ci_lower / a_info$est

  list(est = b_est, se = b_se, ci_lower = b_ci_lower, ci_upper = b_ci_upper)
}

# =============================================================================
# Design Matrix Validation
# =============================================================================

#' Validate a Binary Design Matrix
#'
#' Internal helper to validate that a matrix meets standard requirements for
#' booklet and linking design matrices: is a matrix, has correct column count,
#' and contains only binary values.
#'
#' @param mat An object to validate (should be a matrix).
#' @param n_items Integer: the expected number of columns.
#' @param matrix_name Character string: the name of the matrix type for error
#'   messages (e.g., "booklet_matrix", "linking_matrix").
#'
#' @return Invisibly returns `NULL` if all checks pass. Throws an error
#'   (via `stop()`) if any check fails.
#'
#' @keywords internal
validate_design_matrix <- function(mat, n_items, matrix_name) {
  if (!is.matrix(mat)) {
    stop(
      "`test_design$", matrix_name, "` must be a matrix.",
      call. = FALSE
    )
  }
  if (ncol(mat) != n_items) {
    stop(
      "Number of columns in `", matrix_name, "` (", ncol(mat),
      ") must equal `n_items` (", n_items, ").",
      call. = FALSE
    )
  }
  if (!all(mat %in% c(0L, 1L, 0, 1))) {
    stop(
      "`", matrix_name, "` must contain only binary values (0 and 1).",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# =============================================================================
# Item Parameter Generation
# =============================================================================

#' Generate Discrimination Parameters
#'
#' Internal helper to generate discrimination (a) parameters under a specified
#' distribution. Currently supports log-normal.
#'
#' @param n_items Positive integer: the number of items (and thus the number
#'   of discrimination values to generate).
#' @param a_dist Character string: the distribution name. Currently only
#'   `"lnorm"` is supported.
#' @param a_mean Numeric: the mean parameter for the log-normal distribution
#'   (interpreted as `meanlog`).
#' @param a_sd Numeric: the standard deviation parameter for the log-normal
#'   distribution (interpreted as `sdlog`).
#'
#' @return A numeric vector of length `n_items` containing discrimination values.
#'
#' @keywords internal
generate_discrimination <- function(n_items, a_dist, a_mean, a_sd) {
  switch(a_dist,
    lnorm = stats::rlnorm(n_items, meanlog = a_mean, sdlog = a_sd),
    stop("`a_dist` must be 'lnorm'. Got: '", a_dist, "'.", call. = FALSE)
  )
}
