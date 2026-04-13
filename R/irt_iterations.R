# irt_iterations.R
# Exported function: compute required Monte Carlo replications
# using the Burton (2003) formula.


#' Compute Required Monte Carlo Replications
#'
#' Uses the Burton (2003) formula to determine the minimum number of
#' simulation replications needed to achieve a desired level of
#' Monte Carlo precision.
#'
#' The formula is:
#' \deqn{R = \lceil (z_{\alpha/2} \cdot \sigma / \delta)^2 \rceil}
#'
#' where \eqn{\sigma} is the empirical standard error of the estimand,
#' \eqn{\delta} is the acceptable Monte Carlo error, and
#' \eqn{z_{\alpha/2}} is the critical value for the desired confidence level.
#'
#' @param sigma Positive numeric. The empirical standard error of the
#'   estimand across replications (or a pilot estimate thereof).
#' @param delta Positive numeric. The acceptable Monte Carlo error
#'   (half-width of the MC confidence interval for the estimand).
#' @param alpha Numeric in (0, 1). Two-sided significance level.
#'   Default `0.05` (i.e., 95 percent MC confidence).
#'
#' @return An integer: the minimum number of replications.
#'
#' @references
#' Burton, A., Altman, D. G., Royston, P., & Holder, R. L. (2006).
#' The design of simulation studies in medical statistics.
#' *Statistics in Medicine*, 25(24), 4279--4292.
#' \doi{10.1002/sim.2673}
#'
#' @examples
#' # How many replications for MC SE of bias < 0.1
#' # when empirical SE of the estimand is 0.5?
#' irt_iterations(sigma = 0.5, delta = 0.1)
#'
#' # Tighter tolerance with 99% MC confidence
#' irt_iterations(sigma = 0.5, delta = 0.05, alpha = 0.01)
#'
#' @seealso [irt_simulate()] for running the simulation with the computed
#'   number of replications.
#' @export
irt_iterations <- function(sigma, delta, alpha = 0.05) {


  # --- Validate ---
  if (!is.numeric(sigma) || length(sigma) != 1L || sigma <= 0) {
    stop("`sigma` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(delta) || length(delta) != 1L || delta <= 0) {
    stop("`delta` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number in (0, 1).", call. = FALSE)
  }

  z <- stats::qnorm(1 - alpha / 2)
  R <- ceiling((z * sigma / delta)^2)

  # Ensure at least 1
  as.integer(max(R, 1L))
}
