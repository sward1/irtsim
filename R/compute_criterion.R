# compute_criterion.R
# Internal helper: compute performance criteria from per-iteration estimates.
# Morris et al. (2019) definitions throughout.
#
# Called by summary.irt_results() for each sample_size × item × param group.
# Not exported.


#' Compute Performance Criteria for a Single Parameter (Internal)
#'
#' Given a vector of per-iteration estimates and the true parameter value,
#' computes bias, empirical SE, MSE, RMSE, coverage, and Monte Carlo SEs
#' following Morris et al. (2019).
#'
#' @param estimates Numeric vector of per-iteration parameter estimates.
#'   May contain NAs (non-converged iterations), which are excluded.
#' @param true_value Single numeric value. The data-generating (true)
#'   parameter value.
#' @param ci_lower Optional numeric vector (same length as `estimates`).
#'   Lower bounds of confidence intervals. If `NULL`, coverage is not computed.
#' @param ci_upper Optional numeric vector (same length as `estimates`).
#'   Upper bounds of confidence intervals. If `NULL`, coverage is not computed.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{bias}{Mean estimate minus true value.}
#'     \item{empirical_se}{Sample standard deviation of estimates (n-1 denominator).}
#'     \item{mse}{Mean squared error: `mean((estimate - true_value)^2)`.}
#'     \item{rmse}{Root mean squared error: `sqrt(mse)`.}
#'     \item{coverage}{Proportion of CIs containing the true value, or `NULL`
#'       if CIs not provided. NAs in CIs are excluded from the denominator.}
#'     \item{mcse_bias}{Monte Carlo SE of bias: `empirical_se / sqrt(K)`.}
#'     \item{mcse_mse}{Monte Carlo SE of MSE: `sd((est - true)^2) / sqrt(K)`.}
#'   }
#'
#' @references
#' Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
#' studies to evaluate statistical methods. *Statistics in Medicine*, 38(11),
#' 2074--2102. \doi{10.1002/sim.8086}
#'
#' @keywords internal
compute_criterion <- function(estimates, true_value,
                              ci_lower = NULL, ci_upper = NULL) {

  # Remove NAs (non-converged iterations)
  valid <- !is.na(estimates)
  est <- estimates[valid]
  K <- length(est)

  # All-NA case
  if (K == 0L) {
    return(list(
      bias         = NA_real_,
      empirical_se = NA_real_,
      mse          = NA_real_,
      rmse         = NA_real_,
      coverage     = if (is.null(ci_lower)) NULL else NA_real_,
      mcse_bias    = NA_real_,
      mcse_mse     = NA_real_
    ))
  }

  # --- Bias ---
  bias <- mean(est) - true_value

  # --- Empirical SE (sample SD, n-1 denominator) ---
  empirical_se <- if (K > 1L) sd(est) else NA_real_

  # --- MSE and RMSE ---
  sq_errors <- (est - true_value)^2
  mse <- mean(sq_errors)
  rmse <- sqrt(mse)

  # --- Coverage ---
  if (!is.null(ci_lower) && !is.null(ci_upper)) {
    lo <- ci_lower[valid]
    hi <- ci_upper[valid]
    ci_valid <- !is.na(lo) & !is.na(hi)
    if (sum(ci_valid) > 0L) {
      coverage <- mean(lo[ci_valid] <= true_value & true_value <= hi[ci_valid])
    } else {
      coverage <- NA_real_
    }
  } else {
    coverage <- NULL
  }

  # --- Monte Carlo SEs ---
  mcse_bias <- if (K > 1L) empirical_se / sqrt(K) else NA_real_
  mcse_mse  <- if (K > 1L) sd(sq_errors) / sqrt(K) else NA_real_

  list(
    bias         = bias,
    empirical_se = empirical_se,
    mse          = mse,
    rmse         = rmse,
    coverage     = coverage,
    mcse_bias    = mcse_bias,
    mcse_mse     = mcse_mse
  )
}
