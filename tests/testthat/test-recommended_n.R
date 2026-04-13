# test-recommended_n.R
# TDD tests for recommended_n() generic and recommended_n.summary_irt_results()
#
# recommended_n() is a standalone generic that dispatches on summary_irt_results.
# Given a summary object and a threshold, it returns the minimum sample_size
# at which the criterion meets the threshold for each item × param combination.
#
# Design decisions:
#   - Standalone generic (not a field on summary), following extractor convention
#   - Dispatches on summary_irt_results (user calls summary() first, then queries)
#   - Signature: recommended_n(object, criterion, threshold, param, item, ...)
#   - Returns a data.frame with columns: item, param, recommended_n
#   - "Meets threshold" means the criterion value <= threshold for criteria where
#     smaller is better (bias, mse, rmse, empirical_se, mcse_bias, mcse_mse)
#     and >= threshold for coverage
#   - If no sample_size meets the threshold, recommended_n is NA
#   - If multiple items/params, returns one row per item × param
#   - param and item args filter before searching (optional)


# =============================================================================
# Helper: build a mock summary_irt_results object with known criterion values
# =============================================================================
make_mock_summary <- function(
  item_summary,
  theta_summary = NULL,
  model = "2PL",
  iterations = 100,

  seed = 42
) {
  if (is.null(theta_summary)) {
    sample_sizes <- sort(unique(item_summary$sample_size))
    theta_summary <- data.frame(
      sample_size = sample_sizes,
      mean_cor    = seq(0.85, 0.95, length.out = length(sample_sizes)),
      sd_cor      = rep(0.02, length(sample_sizes)),
      mean_rmse   = seq(0.40, 0.25, length.out = length(sample_sizes)),
      sd_rmse     = rep(0.03, length(sample_sizes)),
      n_converged = rep(iterations, length(sample_sizes)),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      item_summary  = item_summary,
      theta_summary = theta_summary,
      iterations    = iterations,
      seed          = seed,
      model         = model
    ),
    class = "summary_irt_results"
  )
}


# =============================================================================
# Section 1: recommended_n — basic functionality
# =============================================================================

test_that("recommended_n generic exists and dispatches on summary_irt_results", {
  # Should find the function (generic) even before the method exists

  expect_true(is.function(recommended_n))
})

test_that("recommended_n returns a data.frame", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = c(0.50, 0.30, 0.15),
    mse         = c(0.29, 0.10, 0.02),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(0.80, 0.90, 0.95),
    mcse_bias   = c(0.05, 0.03, 0.015),
    mcse_mse    = c(0.02, 0.01, 0.005),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)
  result <- recommended_n(s, criterion = "rmse", threshold = 0.20)

  expect_s3_class(result, "data.frame")
})

test_that("recommended_n returns correct minimum sample size for rmse", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = c(0.50, 0.30, 0.15),
    mse         = c(0.29, 0.10, 0.02),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(0.80, 0.90, 0.95),
    mcse_bias   = c(0.05, 0.03, 0.015),
    mcse_mse    = c(0.02, 0.01, 0.005),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.20: rmse values are 0.54, 0.32, 0.15
  # 0.15 <= 0.20 ✓ → first sample size where rmse <= 0.20 is 500
  result <- recommended_n(s, criterion = "rmse", threshold = 0.20)
  expect_equal(result$recommended_n[1], 500)
})

test_that("recommended_n picks the smallest qualifying sample size", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500, 1000),
    item        = rep(1, 4),
    param       = rep("b", 4),
    true_value  = rep(0.5, 4),
    bias        = c(0.20, 0.10, 0.03, 0.01),
    empirical_se = c(0.50, 0.30, 0.15, 0.10),
    mse         = c(0.29, 0.10, 0.02, 0.01),
    rmse        = c(0.54, 0.32, 0.15, 0.10),
    coverage    = c(0.80, 0.90, 0.95, 0.96),
    mcse_bias   = c(0.05, 0.03, 0.015, 0.01),
    mcse_mse    = c(0.02, 0.01, 0.005, 0.001),
    n_converged = rep(100, 4),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.35: rmse 0.54, 0.32, 0.15, 0.10
  # 0.32 <= 0.35 ✓ → smallest qualifying is 200
  result <- recommended_n(s, criterion = "rmse", threshold = 0.35)
  expect_equal(result$recommended_n[1], 200)
})

test_that("recommended_n returns NA when no sample size meets threshold", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.08),
    empirical_se = c(0.50, 0.30, 0.25),
    mse         = c(0.29, 0.10, 0.07),
    rmse        = c(0.54, 0.32, 0.26),
    coverage    = c(0.80, 0.90, 0.92),
    mcse_bias   = c(0.05, 0.03, 0.025),
    mcse_mse    = c(0.02, 0.01, 0.007),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.01 for bias: none of 0.20, 0.10, 0.08 are <= 0.01
  result <- recommended_n(s, criterion = "bias", threshold = 0.01)
  expect_true(is.na(result$recommended_n[1]))
})


# =============================================================================
# Section 2: recommended_n — coverage (higher is better)
# =============================================================================

test_that("recommended_n uses >= for coverage criterion", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = c(0.50, 0.30, 0.15),
    mse         = c(0.29, 0.10, 0.02),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(0.80, 0.90, 0.95),
    mcse_bias   = c(0.05, 0.03, 0.015),
    mcse_mse    = c(0.02, 0.01, 0.005),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.90: coverage >= 0.90 first at ss = 200
  result <- recommended_n(s, criterion = "coverage", threshold = 0.90)
  expect_equal(result$recommended_n[1], 200)
})

test_that("recommended_n returns NA for coverage when threshold unmet", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = c(0.50, 0.30, 0.15),
    mse         = c(0.29, 0.10, 0.02),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(0.80, 0.90, 0.92),
    mcse_bias   = c(0.05, 0.03, 0.015),
    mcse_mse    = c(0.02, 0.01, 0.005),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.95: none of 0.80, 0.90, 0.92 are >= 0.95
  result <- recommended_n(s, criterion = "coverage", threshold = 0.95)
  expect_true(is.na(result$recommended_n[1]))
})


# =============================================================================
# Section 3: recommended_n — multiple items/params
# =============================================================================

test_that("recommended_n returns one row per item × param combination", {
  # 2 items, 2 params (a, b), 3 sample sizes = 12 rows in summary
  item_summary <- data.frame(
    sample_size = rep(c(100, 200, 500), each = 4),
    item        = rep(c(1, 1, 2, 2), 3),
    param       = rep(c("a", "b", "a", "b"), 3),
    true_value  = rep(c(1.0, 0.5, 1.2, -0.5), 3),
    bias        = c(0.30, 0.20, 0.25, 0.18,   # ss=100
                    0.15, 0.08, 0.12, 0.09,   # ss=200
                    0.05, 0.03, 0.04, 0.02),  # ss=500
    empirical_se = rep(0.2, 12),
    mse         = rep(0.1, 12),
    rmse        = c(0.50, 0.40, 0.45, 0.35,
                    0.30, 0.20, 0.28, 0.22,
                    0.15, 0.10, 0.12, 0.08),
    coverage    = rep(0.90, 12),
    mcse_bias   = rep(0.02, 12),
    mcse_mse    = rep(0.01, 12),
    n_converged = rep(100, 12),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  result <- recommended_n(s, criterion = "rmse", threshold = 0.25)
  expect_equal(nrow(result), 4)  # 2 items × 2 params
  expect_true(all(c("item", "param", "recommended_n") %in% names(result)))
})

test_that("recommended_n gives different N for different items", {
  item_summary <- data.frame(
    sample_size = rep(c(100, 200, 500), each = 2),
    item        = rep(c(1, 2), 3),
    param       = rep("b", 6),
    true_value  = rep(c(0.5, -0.5), 3),
    bias        = c(0.20, 0.25, 0.08, 0.12, 0.03, 0.04),
    empirical_se = rep(0.2, 6),
    mse         = rep(0.1, 6),
    rmse        = c(0.40, 0.50, 0.18, 0.28, 0.10, 0.12),
    coverage    = rep(0.90, 6),
    mcse_bias   = rep(0.02, 6),
    mcse_mse    = rep(0.01, 6),
    n_converged = rep(100, 6),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.20: item 1 rmse hits 0.18 at 200; item 2 rmse hits 0.12 at 500
  result <- recommended_n(s, criterion = "rmse", threshold = 0.20)
  item1 <- result$recommended_n[result$item == 1]
  item2 <- result$recommended_n[result$item == 2]
  expect_equal(item1, 200)
  expect_equal(item2, 500)
})


# =============================================================================
# Section 4: recommended_n — filtering by param and item
# =============================================================================

test_that("param argument filters output to specified parameters", {
  item_summary <- data.frame(
    sample_size = rep(c(100, 200, 500), each = 4),
    item        = rep(c(1, 1, 2, 2), 3),
    param       = rep(c("a", "b", "a", "b"), 3),
    true_value  = rep(c(1.0, 0.5, 1.2, -0.5), 3),
    bias        = rep(0.1, 12),
    empirical_se = rep(0.2, 12),
    mse         = rep(0.1, 12),
    rmse        = c(rep(0.5, 4), rep(0.3, 4), rep(0.1, 4)),
    coverage    = rep(0.90, 12),
    mcse_bias   = rep(0.02, 12),
    mcse_mse    = rep(0.01, 12),
    n_converged = rep(100, 12),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  result <- recommended_n(s, criterion = "rmse", threshold = 0.35,
                          param = "b")
  expect_true(all(result$param == "b"))
  expect_equal(nrow(result), 2)  # 2 items, filtered to param = "b"
})

test_that("item argument filters output to specified items", {
  item_summary <- data.frame(
    sample_size = rep(c(100, 200, 500), each = 2),
    item        = rep(c(1, 2), 3),
    param       = rep("b", 6),
    true_value  = rep(c(0.5, -0.5), 3),
    bias        = rep(0.1, 6),
    empirical_se = rep(0.2, 6),
    mse         = rep(0.1, 6),
    rmse        = c(0.50, 0.55, 0.30, 0.35, 0.10, 0.15),
    coverage    = rep(0.90, 6),
    mcse_bias   = rep(0.02, 6),
    mcse_mse    = rep(0.01, 6),
    n_converged = rep(100, 6),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  result <- recommended_n(s, criterion = "rmse", threshold = 0.35,
                          item = 1)
  expect_equal(nrow(result), 1)
  expect_equal(result$item, 1)
})


# =============================================================================
# Section 5: recommended_n — absolute value for bias
# =============================================================================

test_that("recommended_n uses absolute value for bias comparison", {
  # Negative bias should still be compared as |bias| <= threshold
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(-0.30, -0.15, -0.04),
    empirical_se = rep(0.2, 3),
    mse         = rep(0.1, 3),
    rmse        = rep(0.2, 3),
    coverage    = rep(0.90, 3),
    mcse_bias   = rep(0.02, 3),
    mcse_mse    = rep(0.01, 3),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # threshold = 0.05: |bias| values are 0.30, 0.15, 0.04
  # |−0.04| = 0.04 <= 0.05 ✓ → recommended N = 500
  result <- recommended_n(s, criterion = "bias", threshold = 0.05)
  expect_equal(result$recommended_n[1], 500)
})


# =============================================================================
# Section 6: recommended_n — input validation
# =============================================================================

test_that("recommended_n errors on non-summary_irt_results input", {
  expect_error(recommended_n(list(a = 1), criterion = "rmse", threshold = 0.1))
})

test_that("recommended_n errors on invalid criterion name", {
  item_summary <- data.frame(
    sample_size = c(100, 200),
    item        = rep(1, 2),
    param       = rep("b", 2),
    true_value  = rep(0.5, 2),
    bias        = c(0.10, 0.05),
    empirical_se = c(0.30, 0.15),
    mse         = c(0.10, 0.03),
    rmse        = c(0.32, 0.17),
    coverage    = c(0.88, 0.94),
    mcse_bias   = c(0.03, 0.015),
    mcse_mse    = c(0.01, 0.005),
    n_converged = rep(100, 2),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  expect_error(recommended_n(s, criterion = "r_squared", threshold = 0.1))
})

test_that("recommended_n errors when criterion is missing", {
  item_summary <- data.frame(
    sample_size = c(100, 200),
    item        = rep(1, 2),
    param       = rep("b", 2),
    true_value  = rep(0.5, 2),
    bias        = c(0.10, 0.05),
    empirical_se = c(0.30, 0.15),
    mse         = c(0.10, 0.03),
    rmse        = c(0.32, 0.17),
    coverage    = c(0.88, 0.94),
    mcse_bias   = c(0.03, 0.015),
    mcse_mse    = c(0.01, 0.005),
    n_converged = rep(100, 2),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  expect_error(recommended_n(s, threshold = 0.1))
})

test_that("recommended_n errors when threshold is missing", {
  item_summary <- data.frame(
    sample_size = c(100, 200),
    item        = rep(1, 2),
    param       = rep("b", 2),
    true_value  = rep(0.5, 2),
    bias        = c(0.10, 0.05),
    empirical_se = c(0.30, 0.15),
    mse         = c(0.10, 0.03),
    rmse        = c(0.32, 0.17),
    coverage    = c(0.88, 0.94),
    mcse_bias   = c(0.03, 0.015),
    mcse_mse    = c(0.01, 0.005),
    n_converged = rep(100, 2),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  expect_error(recommended_n(s, criterion = "rmse"))
})

test_that("recommended_n errors when threshold is non-positive", {
  item_summary <- data.frame(
    sample_size = c(100, 200),
    item        = rep(1, 2),
    param       = rep("b", 2),
    true_value  = rep(0.5, 2),
    bias        = c(0.10, 0.05),
    empirical_se = c(0.30, 0.15),
    mse         = c(0.10, 0.03),
    rmse        = c(0.32, 0.17),
    coverage    = c(0.88, 0.94),
    mcse_bias   = c(0.03, 0.015),
    mcse_mse    = c(0.01, 0.005),
    n_converged = rep(100, 2),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  expect_error(recommended_n(s, criterion = "rmse", threshold = -0.1))
  expect_error(recommended_n(s, criterion = "rmse", threshold = 0))
})

test_that("recommended_n errors on invalid param name", {
  item_summary <- data.frame(
    sample_size = c(100, 200),
    item        = rep(1, 2),
    param       = rep("b", 2),
    true_value  = rep(0.5, 2),
    bias        = c(0.10, 0.05),
    empirical_se = c(0.30, 0.15),
    mse         = c(0.10, 0.03),
    rmse        = c(0.32, 0.17),
    coverage    = c(0.88, 0.94),
    mcse_bias   = c(0.03, 0.015),
    mcse_mse    = c(0.01, 0.005),
    n_converged = rep(100, 2),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  expect_error(recommended_n(s, criterion = "rmse", threshold = 0.2,
                             param = "z"))
})

test_that("recommended_n errors when criterion column not in summary", {
  # Simulate a summary created with criterion = c("bias") — no rmse column
  item_summary <- data.frame(
    sample_size = c(100, 200),
    item        = rep(1, 2),
    param       = rep("b", 2),
    true_value  = rep(0.5, 2),
    bias        = c(0.10, 0.05),
    n_converged = rep(100, 2),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  expect_error(recommended_n(s, criterion = "rmse", threshold = 0.2))
})


# =============================================================================
# Section 7: recommended_n — edge cases
# =============================================================================

test_that("recommended_n works when all sample sizes meet threshold", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.01, 0.005, 0.002),
    empirical_se = rep(0.2, 3),
    mse         = rep(0.1, 3),
    rmse        = rep(0.2, 3),
    coverage    = rep(0.90, 3),
    mcse_bias   = rep(0.02, 3),
    mcse_mse    = rep(0.01, 3),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # All bias values <= 0.05 → smallest sample size
  result <- recommended_n(s, criterion = "bias", threshold = 0.05)
  expect_equal(result$recommended_n[1], 100)
})

test_that("recommended_n works with single sample size", {
  item_summary <- data.frame(
    sample_size = 500,
    item        = 1,
    param       = "b",
    true_value  = 0.5,
    bias        = 0.03,
    empirical_se = 0.15,
    mse         = 0.02,
    rmse        = 0.15,
    coverage    = 0.95,
    mcse_bias   = 0.015,
    mcse_mse    = 0.005,
    n_converged = 100,
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  result <- recommended_n(s, criterion = "rmse", threshold = 0.20)
  expect_equal(result$recommended_n[1], 500)

  result2 <- recommended_n(s, criterion = "rmse", threshold = 0.10)
  expect_true(is.na(result2$recommended_n[1]))
})

test_that("recommended_n handles NA criterion values gracefully", {
  # Some criterion values may be NA (e.g., coverage when CIs unavailable)
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = rep(0.2, 3),
    mse         = rep(0.1, 3),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(NA, NA, NA),
    mcse_bias   = rep(0.02, 3),
    mcse_mse    = rep(0.01, 3),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)

  # All coverage = NA → no sample size meets any coverage threshold
  result <- recommended_n(s, criterion = "coverage", threshold = 0.90)
  expect_true(is.na(result$recommended_n[1]))
})


# =============================================================================
# Section 8: recommended_n — output structure
# =============================================================================

test_that("recommended_n output includes item and param columns", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = rep(0.2, 3),
    mse         = rep(0.1, 3),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(0.80, 0.90, 0.95),
    mcse_bias   = rep(0.02, 3),
    mcse_mse    = rep(0.01, 3),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)
  result <- recommended_n(s, criterion = "rmse", threshold = 0.20)

  expect_true("item" %in% names(result))
  expect_true("param" %in% names(result))
  expect_true("recommended_n" %in% names(result))
})

test_that("recommended_n output includes criterion and threshold metadata", {
  item_summary <- data.frame(
    sample_size = c(100, 200, 500),
    item        = rep(1, 3),
    param       = rep("b", 3),
    true_value  = rep(0.5, 3),
    bias        = c(0.20, 0.10, 0.03),
    empirical_se = rep(0.2, 3),
    mse         = rep(0.1, 3),
    rmse        = c(0.54, 0.32, 0.15),
    coverage    = c(0.80, 0.90, 0.95),
    mcse_bias   = rep(0.02, 3),
    mcse_mse    = rep(0.01, 3),
    n_converged = rep(100, 3),
    stringsAsFactors = FALSE
  )

  s <- make_mock_summary(item_summary)
  result <- recommended_n(s, criterion = "rmse", threshold = 0.20)

  # Metadata columns so the user knows what criterion/threshold produced this
  expect_true("criterion" %in% names(result))
  expect_true("threshold" %in% names(result))
  expect_equal(result$criterion[1], "rmse")
  expect_equal(result$threshold[1], 0.20)
})
