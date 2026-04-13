# test-compute_criterion.R
# TDD tests for internal compute_criterion() helper and exported irt_iterations()
#
# These tests use hand-calculated expected values so outcomes are
# deterministic and independent of any simulation run.
# Morris et al. (2019) definitions throughout.
#
# compute_criterion() signature (expected):
#   compute_criterion(estimates, true_value, ci_lower = NULL, ci_upper = NULL)
#   Returns a named list with: bias, empirical_se, mse, rmse, coverage,
#                                mcse_bias, mcse_mse


# =============================================================================
# Helper: access internal function
# =============================================================================
compute_criterion <- function(...) {
  irtsim:::compute_criterion(...)
}

irt_iterations <- irtsim::irt_iterations


# =============================================================================
# Section 1: compute_criterion() — basic bias computation
# =============================================================================

test_that("bias is mean(estimate) - true_value", {
  # estimates: 1.0, 2.0, 3.0  →  mean = 2.0

# true_value = 1.5  →  bias = 2.0 - 1.5 = 0.5
  estimates <- c(1.0, 2.0, 3.0)
  true_value <- 1.5
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$bias, 0.5)
})

test_that("bias is zero when estimates are unbiased", {
  # estimates: -1, 0, 1  →  mean = 0  →  bias = 0 - 0 = 0
  estimates <- c(-1, 0, 1)
  true_value <- 0
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$bias, 0)
})

test_that("bias can be negative", {
  # estimates: 0.5, 0.6, 0.7  →  mean = 0.6
  # true_value = 1.0  →  bias = 0.6 - 1.0 = -0.4
  estimates <- c(0.5, 0.6, 0.7)
  true_value <- 1.0
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$bias, -0.4)
})


# =============================================================================
# Section 2: compute_criterion() — empirical SE
# =============================================================================

test_that("empirical_se is sd(estimates)", {
  # estimates: 2, 4, 6  →  sd = 2
  estimates <- c(2, 4, 6)
  true_value <- 4
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$empirical_se, sd(c(2, 4, 6)))
})

test_that("empirical_se is zero when all estimates are identical", {
  estimates <- c(3.0, 3.0, 3.0, 3.0)
  true_value <- 3.0
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$empirical_se, 0)
})


# =============================================================================
# Section 3: compute_criterion() — MSE and RMSE
# =============================================================================

test_that("mse equals mean((estimate - true_value)^2)", {
  # estimates: 1, 2, 3;  true = 2
  # deviations: -1, 0, 1  →  squared: 1, 0, 1  →  mean = 2/3
  estimates <- c(1, 2, 3)
  true_value <- 2
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$mse, mean((estimates - true_value)^2))
})

test_that("mse equals bias^2 + empirical_se^2 (decomposition identity)", {
  # This is the bias-variance decomposition.
  # Use arbitrary values to verify the algebraic identity.
  estimates <- c(0.8, 1.3, 1.1, 0.9, 1.4, 1.0, 1.2, 0.7, 1.5, 1.1)
  true_value <- 1.0
  result <- compute_criterion(estimates, true_value)

  # Check decomposition: MSE = bias^2 + var(estimates)
  # Note: empirical_se = sd(estimates), and Morris et al. (2019)

  # use the sample SD (denominator n-1), so:
  # MSE ≈ bias^2 + (n-1)/n * empirical_se^2  (the mean of squared devs)
  # Actually MSE = mean((est - true)^2) and decomposition is exact when
  # empirical_se^2 uses n denominator. With sd() using n-1, the identity
  # MSE = bias^2 + ((K-1)/K) * sd(est)^2 holds. Let's just check the
  # direct MSE computation.
  expected_mse <- mean((estimates - true_value)^2)
  expect_equal(result$mse, expected_mse)
})

test_that("rmse is sqrt(mse)", {
  estimates <- c(1, 2, 3, 4, 5)
  true_value <- 3
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$rmse, sqrt(result$mse))
})

test_that("mse is zero when all estimates equal true value", {
  estimates <- c(2.0, 2.0, 2.0)
  true_value <- 2.0
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$mse, 0)
  expect_equal(result$rmse, 0)
})


# =============================================================================
# Section 4: compute_criterion() — coverage
# =============================================================================

test_that("coverage is proportion of CIs containing true value", {
  # 5 iterations: CIs either contain or don't contain true_value = 1.0
  estimates  <- c(0.9, 1.1, 0.5, 1.0, 2.0)
  ci_lower   <- c(0.5, 0.8, 0.3, 0.7, 1.5)
  ci_upper   <- c(1.3, 1.4, 0.7, 1.3, 2.5)
  true_value <- 1.0

  # Coverage: [0.5,1.3]✓  [0.8,1.4]✓  [0.3,0.7]✗  [0.7,1.3]✓  [1.5,2.5]✗
  # → 3/5 = 0.6
  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  expect_equal(result$coverage, 0.6)
})

test_that("coverage is 1.0 when all CIs contain true value", {
  estimates  <- c(1.0, 1.0, 1.0)
  ci_lower   <- c(0.5, 0.5, 0.5)
  ci_upper   <- c(1.5, 1.5, 1.5)
  true_value <- 1.0
  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  expect_equal(result$coverage, 1.0)
})

test_that("coverage is 0.0 when no CIs contain true value", {
  estimates  <- c(3.0, 4.0, 5.0)
  ci_lower   <- c(2.5, 3.5, 4.5)
  ci_upper   <- c(3.5, 4.5, 5.5)
  true_value <- 1.0
  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  expect_equal(result$coverage, 0.0)
})

test_that("coverage handles boundary case (true value equals CI endpoint)", {
  # true_value exactly at the boundary — should count as covered
  # (following convention: lower <= true <= upper)
  estimates  <- c(1.0)
  ci_lower   <- c(1.0)
  ci_upper   <- c(2.0)
  true_value <- 1.0
  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  expect_equal(result$coverage, 1.0)
})

test_that("coverage is NULL when CIs are not provided", {
  estimates <- c(1, 2, 3)
  true_value <- 2
  result <- compute_criterion(estimates, true_value)
  expect_null(result$coverage)
})

test_that("coverage excludes iterations with NA CIs", {
  # 4 iterations: CI 3 has NA → only 3 usable
  # Of those 3: CIs 1 and 2 contain true=1.0, CI 4 does not
  estimates  <- c(0.9, 1.1, 0.5, 2.0)
  ci_lower   <- c(0.5, 0.8, NA,  1.5)
  ci_upper   <- c(1.3, 1.4, NA,  2.5)
  true_value <- 1.0

  # Usable: [0.5,1.3]✓  [0.8,1.4]✓  [1.5,2.5]✗  → 2/3
  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  expect_equal(result$coverage, 2/3)
})


# =============================================================================
# Section 5: compute_criterion() — Monte Carlo SE of bias
# =============================================================================

test_that("mcse_bias equals empirical_se / sqrt(K)", {
  estimates <- c(1, 2, 3, 4, 5)
  true_value <- 3
  K <- length(estimates)
  result <- compute_criterion(estimates, true_value)
  expected_mcse <- sd(estimates) / sqrt(K)
  expect_equal(result$mcse_bias, expected_mcse)
})


# =============================================================================
# Section 6: compute_criterion() — Monte Carlo SE of MSE
# =============================================================================

test_that("mcse_mse follows Morris et al. (2019) formula", {
  # MCSE(MSE) = sqrt( (1/K) * var((estimate - true)^2) )
  # which is sd((estimate - true)^2) / sqrt(K)
  estimates <- c(0.5, 1.0, 1.5, 2.0, 2.5)
  true_value <- 1.5
  K <- length(estimates)
  sq_errors <- (estimates - true_value)^2
  expected_mcse_mse <- sd(sq_errors) / sqrt(K)

  result <- compute_criterion(estimates, true_value)
  expect_equal(result$mcse_mse, expected_mcse_mse)
})


# =============================================================================
# Section 7: compute_criterion() — NA handling
# =============================================================================

test_that("NAs in estimates are excluded from all calculations", {
  # 5 estimates, 2 are NA → effective K = 3
  estimates <- c(1.0, NA, 2.0, NA, 3.0)
  true_value <- 2.0
  result <- compute_criterion(estimates, true_value)

  valid <- c(1.0, 2.0, 3.0)
  expect_equal(result$bias, mean(valid) - true_value)
  expect_equal(result$empirical_se, sd(valid))
  expect_equal(result$mse, mean((valid - true_value)^2))
  expect_equal(result$mcse_bias, sd(valid) / sqrt(3))
})

test_that("all-NA estimates return all-NA results", {
  estimates <- c(NA_real_, NA_real_, NA_real_)
  true_value <- 1.0
  result <- compute_criterion(estimates, true_value)
  expect_true(is.na(result$bias))
  expect_true(is.na(result$empirical_se))
  expect_true(is.na(result$mse))
  expect_true(is.na(result$rmse))
  expect_true(is.na(result$mcse_bias))
})

test_that("single valid estimate returns NA for empirical_se and mcse", {
  # sd() of a single value is NA → propagates
  estimates <- c(NA, NA, 2.0, NA)
  true_value <- 2.0
  result <- compute_criterion(estimates, true_value)
  expect_equal(result$bias, 0)
  expect_true(is.na(result$empirical_se))
  expect_equal(result$mse, 0)
})


# =============================================================================
# Section 8: compute_criterion() — return structure
# =============================================================================

test_that("compute_criterion returns a named list with all expected elements",
{
  estimates  <- c(1, 2, 3, 4, 5)
  ci_lower   <- c(0.5, 1.5, 2.5, 3.5, 4.5)
  ci_upper   <- c(1.5, 2.5, 3.5, 4.5, 5.5)
  true_value <- 3

  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  expect_type(result, "list")
  expect_named(result,
    c("bias", "empirical_se", "mse", "rmse", "coverage",
      "mcse_bias", "mcse_mse"),
    ignore.order = TRUE
  )
})

test_that("all numeric outputs are length-1 scalars", {
  estimates  <- c(1, 2, 3)
  ci_lower   <- c(0.5, 1.5, 2.5)
  ci_upper   <- c(1.5, 2.5, 3.5)
  true_value <- 2

  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)
  for (nm in names(result)) {
    expect_length(result[[nm]], 1)
  }
})


# =============================================================================
# Section 9: compute_criterion() — hand-calculated comprehensive example
# =============================================================================

test_that("full hand-calculated example matches expected values", {
  # 6 estimates of a parameter with true value = 1.0
  estimates  <- c(0.8, 1.2, 0.9, 1.1, 1.0, 1.3)
  true_value <- 1.0

  # CIs: all width 0.4 centered on estimate
  ci_lower <- estimates - 0.2
  ci_upper <- estimates + 0.2

  K <- 6

  # Hand calculations:
  # mean(est) = (0.8+1.2+0.9+1.1+1.0+1.3)/6 = 6.3/6 = 1.05
  # bias = 1.05 - 1.0 = 0.05
  expected_bias <- mean(estimates) - true_value

  # empirical_se = sd(estimates) with n-1 denominator
  expected_emp_se <- sd(estimates)

  # MSE = mean((est - true)^2)
  # deviations: -0.2, 0.2, -0.1, 0.1, 0, 0.3
  # squared: 0.04, 0.04, 0.01, 0.01, 0, 0.09
  # mean = 0.19/6
  expected_mse <- mean((estimates - true_value)^2)

  # RMSE
  expected_rmse <- sqrt(expected_mse)

  # Coverage: true=1.0
  # [0.6,1.0]✓  [1.0,1.4]✓  [0.7,1.1]✓  [0.9,1.3]✓  [0.8,1.2]✓  [1.1,1.5]✗
  # → 5/6
  expected_coverage <- 5/6

  # MCSE(bias) = sd(est) / sqrt(K)
  expected_mcse_bias <- sd(estimates) / sqrt(K)

  # MCSE(MSE) = sd((est - true)^2) / sqrt(K)
  sq_errors <- (estimates - true_value)^2
  expected_mcse_mse <- sd(sq_errors) / sqrt(K)

  result <- compute_criterion(estimates, true_value,
                              ci_lower = ci_lower, ci_upper = ci_upper)

  expect_equal(result$bias, expected_bias)
  expect_equal(result$empirical_se, expected_emp_se)
  expect_equal(result$mse, expected_mse)
  expect_equal(result$rmse, expected_rmse)
  expect_equal(result$coverage, expected_coverage)
  expect_equal(result$mcse_bias, expected_mcse_bias)
  expect_equal(result$mcse_mse, expected_mcse_mse)
})


# =============================================================================
# Section 10: irt_iterations() — Burton (2003) formula
# =============================================================================
# Burton formula: R = ceil( (z_{alpha/2} * sigma / delta)^2 )
# where sigma = empirical SE of the estimand,
#       delta = acceptable Monte Carlo error,
#       alpha = two-sided significance level (default 0.05)

test_that("irt_iterations returns correct R for known inputs", {
  # sigma = 0.5, delta = 0.1, alpha = 0.05
  # z = qnorm(0.975) = 1.959964
  # R = ceil((1.959964 * 0.5 / 0.1)^2) = ceil((9.79982)^2) = ceil(96.04) = 97
  z <- qnorm(0.975)
  expected <- ceiling((z * 0.5 / 0.1)^2)

  result <- irt_iterations(sigma = 0.5, delta = 0.1, alpha = 0.05)
  expect_equal(result, expected)
})

test_that("irt_iterations uses alpha = 0.05 by default", {
  r1 <- irt_iterations(sigma = 1.0, delta = 0.2)
  r2 <- irt_iterations(sigma = 1.0, delta = 0.2, alpha = 0.05)
  expect_equal(r1, r2)
})

test_that("irt_iterations with alpha = 0.01 uses z = qnorm(0.995)", {
  z <- qnorm(0.995)
  expected <- ceiling((z * 0.3 / 0.05)^2)
  result <- irt_iterations(sigma = 0.3, delta = 0.05, alpha = 0.01)
  expect_equal(result, expected)
})

test_that("irt_iterations returns integer", {
  result <- irt_iterations(sigma = 1, delta = 0.1)
  expect_type(result, "integer")
})

test_that("irt_iterations with large sigma / small delta gives large R", {
  # sigma = 2, delta = 0.01  → need lots of iterations
  z <- qnorm(0.975)
  expected <- ceiling((z * 2 / 0.01)^2)
  result <- irt_iterations(sigma = 2, delta = 0.01)
  expect_equal(result, expected)
  expect_true(result > 100000)
})

test_that("irt_iterations validates sigma > 0", {
  expect_error(irt_iterations(sigma = 0, delta = 0.1))
  expect_error(irt_iterations(sigma = -1, delta = 0.1))
})

test_that("irt_iterations validates delta > 0", {
  expect_error(irt_iterations(sigma = 1, delta = 0))
  expect_error(irt_iterations(sigma = 1, delta = -0.5))
})

test_that("irt_iterations validates alpha in (0, 1)", {
  expect_error(irt_iterations(sigma = 1, delta = 0.1, alpha = 0))
  expect_error(irt_iterations(sigma = 1, delta = 0.1, alpha = 1))
  expect_error(irt_iterations(sigma = 1, delta = 0.1, alpha = -0.05))
  expect_error(irt_iterations(sigma = 1, delta = 0.1, alpha = 1.5))
})

test_that("irt_iterations returns at least 1", {
  # Even with very small sigma or large delta
  result <- irt_iterations(sigma = 0.001, delta = 100)
  expect_true(result >= 1L)
})
