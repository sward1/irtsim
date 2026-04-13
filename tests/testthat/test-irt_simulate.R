# test-irt_simulate.R
# TDD tests for irt_simulate() — Objective 8
# These tests define the expected behavior BEFORE implementation exists.
#
# irt_simulate() is the core simulation engine. It takes an irt_study object
# and runs a Monte Carlo simulation loop:
#   for each iteration:
#     for each sample_size:
#       generate data → apply missing → fit model → extract parameters
#
# Design decision (Approach A): irt_simulate() stores RAW per-iteration
# parameter estimates (point estimates, SEs, CIs, convergence). Criterion
# computation (MSE, bias, RMSE, coverage, etc.) happens at summary() time,
# not inside irt_simulate(). This follows the simhelpers/SimDesign/Morris
# et al. (2019) convention.
#
# Per-iteration storage:
#   iteration | sample_size | item | param | true_value | estimate | se |
#     ci_lower | ci_upper | converged
#
# For theta-level summaries (correlation with true theta, RMSE), a separate
# theta_results data.frame stores per-iteration summary stats:
#   iteration | sample_size | theta_cor | theta_rmse | converged


# --- Helper: reusable designs and studies ------------------------------------

make_1pl_study <- function(n_items = 10,
                           sample_sizes = c(100, 250),
                           missing = "none") {
  design <- irt_design(
    model = "1PL",
    n_items = n_items,
    item_params = list(b = seq(-2, 2, length.out = n_items))
  )
  irt_study(design, sample_sizes = sample_sizes, missing = missing)
}

make_2pl_study <- function(n_items = 10,
                           sample_sizes = c(100, 250),
                           missing = "none") {
  design <- irt_design(
    model = "2PL",
    n_items = n_items,
    item_params = list(
      a = rep(1.2, n_items),
      b = seq(-2, 2, length.out = n_items)
    )
  )
  irt_study(design, sample_sizes = sample_sizes, missing = missing)
}

make_grm_study <- function(n_items = 8,
                           n_categories = 4,
                           sample_sizes = c(200, 500)) {
  n_thresh <- n_categories - 1L
  b_mat <- matrix(
    seq(-2, 2, length.out = n_items * n_thresh),
    nrow = n_items, ncol = n_thresh
  )
  if (n_thresh > 1L) {
    b_mat <- t(apply(b_mat, 1, sort))
  }
  design <- irt_design(
    model = "GRM",
    n_items = n_items,
    item_params = list(a = rep(1.0, n_items), b = b_mat)
  )
  irt_study(design, sample_sizes = sample_sizes)
}


# =============================================================================
# 1. Return Type and S3 Class
# =============================================================================

test_that("irt_simulate returns an irt_results S3 object", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_s3_class(res, "irt_results")
})

test_that("irt_results contains required top-level elements", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  # Must contain: item_results, theta_results, study, iterations, seed, timings

  expect_true("item_results" %in% names(res))
  expect_true("theta_results" %in% names(res))
  expect_true("study" %in% names(res))
  expect_true("iterations" %in% names(res))
  expect_true("seed" %in% names(res))
})

test_that("irt_results$study is the original irt_study object", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_identical(res$study, study)
})


# =============================================================================
# 2. item_results Structure
# =============================================================================

test_that("item_results is a data.frame with expected columns", {
  study <- make_1pl_study(n_items = 10, sample_sizes = c(100, 250))
  res <- irt_simulate(study, iterations = 3, seed = 42)

  ir <- res$item_results
  expect_s3_class(ir, "data.frame")

  expected_cols <- c(
    "iteration", "sample_size", "item", "param",
    "true_value", "estimate", "se", "ci_lower", "ci_upper", "converged"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(ir), info = paste("Missing column:", col))
  }
})

test_that("item_results has correct number of rows for 1PL", {
  n_items <- 10
  sample_sizes <- c(100, 250)
  n_ss <- length(sample_sizes)
  iterations <- 3

  study <- make_1pl_study(n_items = n_items, sample_sizes = sample_sizes)
  res <- irt_simulate(study, iterations = iterations, seed = 42)

  ir <- res$item_results

  # 1PL: one parameter per item (b/difficulty)
  # Rows = iterations × sample_sizes × items × params_per_item
  # 1PL has 1 param (b) per item
  n_params_per_item <- 1L
  expected_rows <- iterations * n_ss * n_items * n_params_per_item
  expect_equal(nrow(ir), expected_rows)
})

test_that("item_results has correct number of rows for 2PL", {
  n_items <- 10
  sample_sizes <- c(100, 250)
  n_ss <- length(sample_sizes)
  iterations <- 3

  study <- make_2pl_study(n_items = n_items, sample_sizes = sample_sizes)
  res <- irt_simulate(study, iterations = iterations, seed = 42)

  ir <- res$item_results

  # 2PL: two parameters per item (a, b)
  n_params_per_item <- 2L
  expected_rows <- iterations * n_ss * n_items * n_params_per_item
  expect_equal(nrow(ir), expected_rows)
})

test_that("item_results iteration column contains 1:iterations", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 5, seed = 42)

  expect_equal(sort(unique(res$item_results$iteration)), 1:5)
})

test_that("item_results sample_size column matches study sample_sizes", {
  study <- make_1pl_study(sample_sizes = c(100, 250))
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_equal(sort(unique(res$item_results$sample_size)), c(100L, 250L))
})

test_that("item_results item column indexes all items", {
  n_items <- 10
  study <- make_1pl_study(n_items = n_items)
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_equal(sort(unique(res$item_results$item)), 1:n_items)
})

test_that("item_results param column contains 'b' for 1PL", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_equal(unique(res$item_results$param), "b")
})

test_that("item_results param column contains 'a' and 'b' for 2PL", {
  study <- make_2pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_true(setequal(unique(res$item_results$param), c("a", "b")))
})

test_that("item_results true_value matches design parameters", {
  n_items <- 10
  b_true <- seq(-2, 2, length.out = n_items)

  study <- make_1pl_study(n_items = n_items)
  res <- irt_simulate(study, iterations = 3, seed = 42)

  ir <- res$item_results
  # For each item, the true b value should match the design
  b_rows <- ir[ir$param == "b", ]
  for (i in seq_len(n_items)) {
    item_true <- unique(b_rows$true_value[b_rows$item == i])
    expect_equal(item_true, b_true[i], tolerance = 1e-10,
                 info = paste("Item", i, "true_value mismatch"))
  }
})

test_that("item_results estimate values are numeric and finite (converged)", {

  study <- make_1pl_study(sample_sizes = c(250))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  ir <- res$item_results
  converged_rows <- ir[ir$converged, ]

  # Estimates should be numeric and finite for converged replications
  expect_true(is.numeric(converged_rows$estimate))
  expect_true(all(is.finite(converged_rows$estimate)))
})

test_that("item_results se values are non-negative for converged iterations", {
  study <- make_1pl_study(sample_sizes = c(250))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  ir <- res$item_results
  converged_rows <- ir[ir$converged, ]

  expect_true(all(converged_rows$se >= 0))
})

test_that("item_results ci_lower <= estimate <= ci_upper for converged rows", {
  study <- make_2pl_study(sample_sizes = c(250))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  ir <- res$item_results
  # Filter to converged rows that have non-NA CIs (SEM SE computation

  # can fail for individual parameters even when the model converges)
  cr <- ir[ir$converged & !is.na(ir$ci_lower) & !is.na(ir$ci_upper), ]

  expect_true(nrow(cr) > 0)  # at least some rows have CIs
  expect_true(all(cr$ci_lower <= cr$estimate))
  expect_true(all(cr$estimate <= cr$ci_upper))
})


# =============================================================================
# 3. theta_results Structure
# =============================================================================

test_that("theta_results is a data.frame with expected columns", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  tr <- res$theta_results
  expect_s3_class(tr, "data.frame")

  expected_cols <- c(
    "iteration", "sample_size", "theta_cor", "theta_rmse", "converged"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(tr), info = paste("Missing column:", col))
  }
})

test_that("theta_results has one row per iteration × sample_size", {
  sample_sizes <- c(100, 250)
  iterations <- 5

  study <- make_1pl_study(sample_sizes = sample_sizes)
  res <- irt_simulate(study, iterations = iterations, seed = 42)

  tr <- res$theta_results
  expected_rows <- iterations * length(sample_sizes)
  expect_equal(nrow(tr), expected_rows)
})

test_that("theta_cor is between -1 and 1 for converged iterations", {
  study <- make_1pl_study(sample_sizes = c(500))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  tr <- res$theta_results
  cr <- tr[tr$converged, ]

  expect_true(all(cr$theta_cor >= -1 & cr$theta_cor <= 1))
})

test_that("theta_rmse is non-negative for converged iterations", {
  study <- make_1pl_study(sample_sizes = c(500))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  tr <- res$theta_results
  cr <- tr[tr$converged, ]

  expect_true(all(cr$theta_rmse >= 0))
})


# =============================================================================
# 4. Seed Reproducibility
# =============================================================================

test_that("irt_simulate is reproducible with the same seed", {
  study <- make_1pl_study(sample_sizes = c(100))
  res1 <- irt_simulate(study, iterations = 3, seed = 42)
  res2 <- irt_simulate(study, iterations = 3, seed = 42)

  expect_identical(res1$item_results, res2$item_results)
  expect_identical(res1$theta_results, res2$theta_results)
})

test_that("irt_simulate produces different results with different seeds", {
  study <- make_1pl_study(sample_sizes = c(100))
  res1 <- irt_simulate(study, iterations = 3, seed = 1)
  res2 <- irt_simulate(study, iterations = 3, seed = 2)

  expect_false(identical(res1$item_results, res2$item_results))
})


# =============================================================================
# 5. Non-Convergence Handling
# =============================================================================

test_that("non-convergence is tracked in item_results$converged", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 5, seed = 42)

  expect_true(is.logical(res$item_results$converged))
})

test_that("non-convergence is tracked in theta_results$converged", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 5, seed = 42)

  expect_true(is.logical(res$theta_results$converged))
})

test_that("non-converged iterations have NA estimates", {
  # We can't force non-convergence easily, but we can test the structural
  # contract: if converged == FALSE, estimate/se/ci should be NA
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 5, seed = 42)

  ir <- res$item_results
  non_conv <- ir[!ir$converged, ]

  if (nrow(non_conv) > 0) {
    expect_true(all(is.na(non_conv$estimate)))
    expect_true(all(is.na(non_conv$se)))
    expect_true(all(is.na(non_conv$ci_lower)))
    expect_true(all(is.na(non_conv$ci_upper)))
  } else {
    # All converged — that's fine for a well-specified simple model
    expect_true(TRUE)
  }
})

test_that("non-converged iterations are never silently dropped", {
  # Total row count must equal iterations × sample_sizes × items × params
  # regardless of convergence
  n_items <- 10
  sample_sizes <- c(100, 250)
  iterations <- 5

  study <- make_1pl_study(n_items = n_items, sample_sizes = sample_sizes)
  res <- irt_simulate(study, iterations = iterations, seed = 42)

  n_params_per_item <- 1L  # 1PL
  expected_rows <- iterations * length(sample_sizes) * n_items * n_params_per_item
  expect_equal(nrow(res$item_results), expected_rows)

  expected_theta_rows <- iterations * length(sample_sizes)
  expect_equal(nrow(res$theta_results), expected_theta_rows)
})


# =============================================================================
# 6. Input Validation
# =============================================================================

test_that("irt_simulate errors for non-irt_study input", {
  expect_error(
    irt_simulate("not a study", iterations = 3, seed = 42),
    "irt_study"
  )
})

test_that("irt_simulate errors for non-positive iterations", {
  study <- make_1pl_study()

  expect_error(irt_simulate(study, iterations = 0, seed = 42), "iterations")
  expect_error(irt_simulate(study, iterations = -5, seed = 42), "iterations")
})

test_that("irt_simulate errors for non-integer iterations", {
  study <- make_1pl_study()

  expect_error(irt_simulate(study, iterations = 3.5, seed = 42), "iterations")
})

test_that("irt_simulate errors when seed is missing", {
  study <- make_1pl_study()

  # seed is required for reproducibility
  expect_error(irt_simulate(study, iterations = 3), "seed")
})


# =============================================================================
# 7. Model-Specific Behavior
# =============================================================================

test_that("irt_simulate works end-to-end for 1PL", {
  study <- make_1pl_study(n_items = 10, sample_sizes = c(200))
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_s3_class(res, "irt_results")
  expect_equal(unique(res$item_results$param), "b")
  expect_true(nrow(res$item_results) > 0)
})

test_that("irt_simulate works end-to-end for 2PL", {
  study <- make_2pl_study(n_items = 10, sample_sizes = c(200))
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_s3_class(res, "irt_results")
  expect_true(setequal(unique(res$item_results$param), c("a", "b")))
  expect_true(nrow(res$item_results) > 0)
})

test_that("irt_simulate works end-to-end for GRM", {
  study <- make_grm_study(n_items = 8, n_categories = 4, sample_sizes = c(300))
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_s3_class(res, "irt_results")
  # GRM has a and indexed threshold parameters (b1, b2, b3, ...)
  expect_true("a" %in% unique(res$item_results$param))
  b_params <- grep("^b\\d+$", unique(res$item_results$param), value = TRUE)
  expect_true(length(b_params) >= 1)
  expect_true(nrow(res$item_results) > 0)
})


# =============================================================================
# 8. Missing Data Integration
# =============================================================================

test_that("irt_simulate works with MCAR missing data", {
  design <- irt_design(
    model = "1PL",
    n_items = 10,
    item_params = list(b = seq(-2, 2, length.out = 10))
  )
  study <- irt_study(design, sample_sizes = c(200),
                     missing = "mcar", missing_rate = 0.1)

  res <- irt_simulate(study, iterations = 3, seed = 42)
  expect_s3_class(res, "irt_results")
  expect_true(nrow(res$item_results) > 0)
})


# =============================================================================
# 9. Parameter Recovery Sanity Checks
# =============================================================================
# These are NOT precision tests — they're sanity checks that the simulation
# loop is wired correctly (generating, fitting, extracting in the right order).
# Actual criterion precision testing belongs in objectives 10-11.

test_that("1PL b estimates are in a reasonable range", {
  # True b in [-2, 2], estimates should be in roughly [-4, 4]
  study <- make_1pl_study(n_items = 10, sample_sizes = c(500))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  ir <- res$item_results
  cr <- ir[ir$converged & ir$param == "b", ]

  expect_true(all(cr$estimate > -6))
  expect_true(all(cr$estimate < 6))
})

test_that("2PL a estimates are positive", {
  study <- make_2pl_study(n_items = 10, sample_sizes = c(500))
  res <- irt_simulate(study, iterations = 5, seed = 42)

  ir <- res$item_results
  cr <- ir[ir$converged & ir$param == "a", ]

  # Discrimination estimates should be positive
  expect_true(all(cr$estimate > 0))
})

test_that("mean b estimates are close to true values with large N", {
  # With N = 1000, mean estimates across iterations should be near truth
  n_items <- 10
  b_true <- seq(-2, 2, length.out = n_items)

  study <- make_1pl_study(n_items = n_items, sample_sizes = c(1000))
  res <- irt_simulate(study, iterations = 10, seed = 42)

  ir <- res$item_results
  cr <- ir[ir$converged & ir$param == "b", ]

  # Check mean estimate per item — should be within 0.5 of truth
  for (i in seq_len(n_items)) {
    item_ests <- cr$estimate[cr$item == i]
    if (length(item_ests) > 0) {
      mean_est <- mean(item_ests)
      expect_lt(abs(mean_est - b_true[i]), 0.5,
                label = paste("Item", i, ": |mean_est - true| =",
                              round(abs(mean_est - b_true[i]), 3)))
    }
  }
})


# =============================================================================
# 10. Print Method
# =============================================================================

test_that("print.irt_results produces output without error", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_output(print(res))
})

test_that("print.irt_results shows iterations and convergence rate", {
  study <- make_1pl_study()
  res <- irt_simulate(study, iterations = 3, seed = 42)

  output <- capture.output(print(res))
  combined <- paste(output, collapse = "\n")

  # Should mention iterations count
  expect_match(combined, "3", fixed = TRUE)
  # Should mention convergence
  expect_match(combined, "[Cc]onverg")
})


# =============================================================================
# 11. Timing Information
# =============================================================================

test_that("irt_results contains elapsed time information", {
  study <- make_1pl_study(sample_sizes = c(100))
  res <- irt_simulate(study, iterations = 3, seed = 42)

  expect_true("elapsed" %in% names(res))
  expect_true(is.numeric(res$elapsed))
  expect_true(res$elapsed >= 0)
})
