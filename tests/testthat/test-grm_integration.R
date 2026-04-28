# test-grm_integration.R
# Objective 14: Full pipeline integration test for GRM
# Exercises: irt_design → irt_study → irt_simulate → summary → plot → recommended_n

# ---- Helper: build a standard GRM study for integration tests ----
make_grm_integration_study <- function(n_items = 5,
                                       n_categories = 4,
                                       sample_sizes = c(200, 500),
                                       missing = "none",
                                       missing_rate = NULL) {
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
    item_params = list(a = rep(1.2, n_items), b = b_mat)
  )
  irt_study(design,
            sample_sizes = sample_sizes,
            missing = missing,
            missing_rate = missing_rate)
}


# ===========================================================================
# 1. Full pipeline — clean data (no missingness)
# ===========================================================================

test_that("GRM full pipeline runs without error (no missing)", {
  study <- make_grm_integration_study()
  expect_no_error(
    res <- irt_simulate(study, iterations = 3, seed = 101)
  )
  expect_s3_class(res, "irt_results")
})

test_that("GRM item_results has correct structure", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 3, seed = 101)

  ir <- res$item_results
  expect_true(is.data.frame(ir))
  expected_cols <- c("iteration", "sample_size", "item", "param",
                     "true_value", "estimate", "se",
                     "ci_lower", "ci_upper", "converged")
  expect_true(all(expected_cols %in% names(ir)))
})

test_that("GRM item_results contains correct parameter names", {
  study <- make_grm_integration_study(n_items = 5, n_categories = 4)
  res <- irt_simulate(study, iterations = 3, seed = 101)

  params <- unique(res$item_results$param)
  expect_true("a" %in% params)
  expect_true("b1" %in% params)
  expect_true("b2" %in% params)
  expect_true("b3" %in% params)
  # 4 categories → 3 thresholds (b1, b2, b3) + a = 4 param types
  expect_equal(length(params), 4)
})

test_that("GRM item_results has correct row count", {
  n_items <- 5
  n_categories <- 4
  n_thresh <- n_categories - 1  # 3
  n_params_per_item <- 1 + n_thresh  # a + b1,b2,b3
  sample_sizes <- c(200, 500)
  iterations <- 3

  study <- make_grm_integration_study(
    n_items = n_items, n_categories = n_categories,
    sample_sizes = sample_sizes
  )
  res <- irt_simulate(study, iterations = iterations, seed = 101)

  # Expected rows (assuming all converge):
  # n_items * n_params_per_item * length(sample_sizes) * iterations
  expected_rows <- n_items * n_params_per_item * length(sample_sizes) * iterations
  converged <- res$item_results[res$item_results$converged, ]
  expect_equal(nrow(converged), expected_rows)
})

test_that("GRM theta_results has correct structure and row count", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 3, seed = 101)

  tr <- res$theta_results
  expect_true(is.data.frame(tr))
  expect_true(all(c("iteration", "sample_size", "theta_cor",
                     "theta_rmse", "converged") %in% names(tr)))
  # 2 sample sizes × 3 iterations = 6 rows

  expect_equal(nrow(tr), 6)
})

test_that("GRM true_value matches design parameters", {
  n_items <- 5
  a_vals <- rep(1.2, n_items)
  b_mat <- matrix(
    seq(-2, 2, length.out = n_items * 3),
    nrow = n_items, ncol = 3
  )
  b_mat <- t(apply(b_mat, 1, sort))

  design <- irt_design(
    model = "GRM", n_items = n_items,
    item_params = list(a = a_vals, b = b_mat)
  )
  study <- irt_study(design, sample_sizes = c(300))
  res <- irt_simulate(study, iterations = 2, seed = 202)

  ir <- res$item_results

  # Check a true values
  a_rows <- ir[ir$param == "a" & ir$iteration == 1, ]
  expect_equal(a_rows$true_value, a_vals)

  # Check b1 true values
  b1_rows <- ir[ir$param == "b1" & ir$iteration == 1, ]
  expect_equal(b1_rows$true_value, b_mat[, 1])

  # Check b3 true values
  b3_rows <- ir[ir$param == "b3" & ir$iteration == 1, ]
  expect_equal(b3_rows$true_value, b_mat[, 3])
})

test_that("GRM seed reproducibility", {
  study <- make_grm_integration_study(sample_sizes = c(300))
  res1 <- irt_simulate(study, iterations = 3, seed = 42)
  res2 <- irt_simulate(study, iterations = 3, seed = 42)

  expect_identical(res1$item_results, res2$item_results)
  expect_identical(res1$theta_results, res2$theta_results)
})


# ===========================================================================
# 2. Summary method
# ===========================================================================

test_that("summary() on GRM results returns summary_irt_results", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)

  s <- summary(res)
  expect_s3_class(s, "summary_irt_results")
  expect_true("item_summary" %in% names(s))
  expect_true("theta_summary" %in% names(s))
})

test_that("summary() item_summary has rows for all GRM params", {
  study <- make_grm_integration_study(n_items = 5, n_categories = 4,
                                      sample_sizes = c(200, 500))
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res)

  is_df <- s$item_summary
  # 5 items × 4 params (a, b1, b2, b3) × 2 sample sizes = 40 rows

  expect_equal(nrow(is_df), 40)
  expect_true(all(c("a", "b1", "b2", "b3") %in% unique(is_df$param)))
})

test_that("summary() with param filter works for GRM thresholds", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res, param = c("b1", "b3"))

  expect_true(all(s$item_summary$param %in% c("b1", "b3")))
})

test_that("summary() criterion values are finite for converged GRM sims", {
  study <- make_grm_integration_study(sample_sizes = c(500))
  res <- irt_simulate(study, iterations = 10, seed = 404)
  s <- summary(res)

  is_df <- s$item_summary
  # All criterion columns should be numeric
  for (col in c("bias", "empirical_se", "mse", "rmse")) {
    if (col %in% names(is_df)) {
      vals <- is_df[[col]]
      # Should have finite values (some may be NA if SE was NA)
      expect_true(any(is.finite(vals)),
                  info = paste("No finite values in", col))
    }
  }
})

test_that("summary() theta_summary has correct structure for GRM", {
  study <- make_grm_integration_study(sample_sizes = c(200, 500))
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res)

  ts_df <- s$theta_summary
  expect_true(is.data.frame(ts_df))
  expect_true("sample_size" %in% names(ts_df))
  expect_equal(nrow(ts_df), 2)  # 2 sample sizes
})


# ===========================================================================
# 3. Plot method
# ===========================================================================

test_that("plot() on GRM results returns a ggplot", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)

  p <- plot(res)
  expect_s3_class(p, "ggplot")
})

test_that("plot() on GRM results with threshold line works", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)

  p <- plot(res, criterion = "rmse", threshold = 0.3)
  expect_s3_class(p, "ggplot")
})

test_that("plot() on GRM summary works", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res)

  p <- plot(s, criterion = "bias")
  expect_s3_class(p, "ggplot")
})

test_that("plot() on GRM with param filter works", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)

  p <- plot(res, param = "a")
  expect_s3_class(p, "ggplot")
})


# ===========================================================================
# 4. recommended_n()
# ===========================================================================

test_that("recommended_n() works on GRM summary", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res)

  rec <- recommended_n(s, criterion = "rmse", threshold = 1.0,
                       aggregate = "none")
  expect_true(is.data.frame(rec))
  expect_true(all(c("item", "param", "recommended_n") %in% names(rec)))
})

test_that("recommended_n() returns results for all GRM params", {
  study <- make_grm_integration_study(n_items = 5, n_categories = 4)
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res)

  rec <- recommended_n(s, criterion = "rmse", threshold = 1.0,
                       aggregate = "none")
  expect_true(all(c("a", "b1", "b2", "b3") %in% unique(rec$param)))
})

test_that("recommended_n() with param filter on GRM", {
  study <- make_grm_integration_study()
  res <- irt_simulate(study, iterations = 5, seed = 303)
  s <- summary(res)

  rec <- recommended_n(s, criterion = "rmse", threshold = 1.0, param = "b2",
                       aggregate = "none")
  expect_true(all(rec$param == "b2"))
})


# ===========================================================================
# 5. GRM with MCAR missingness
# ===========================================================================

test_that("GRM pipeline works with MCAR missingness", {
  study <- make_grm_integration_study(
    sample_sizes = c(300),
    missing = "mcar",
    missing_rate = 0.1
  )
  res <- irt_simulate(study, iterations = 3, seed = 505)

  expect_s3_class(res, "irt_results")
  expect_true(nrow(res$item_results) > 0)

  # Summary should also work
  s <- summary(res)
  expect_s3_class(s, "summary_irt_results")
})


# ===========================================================================
# 6. GRM with 2 categories (binary — edge case)
# ===========================================================================

test_that("GRM pipeline works with 2 categories (binary edge case)", {
  n_items <- 5
  # 2 categories → 1 threshold
  b_mat <- matrix(seq(-1.5, 1.5, length.out = n_items), nrow = n_items, ncol = 1)
  design <- irt_design(
    model = "GRM",
    n_items = n_items,
    item_params = list(a = rep(1.0, n_items), b = b_mat)
  )
  study <- irt_study(design, sample_sizes = c(300))

  res <- irt_simulate(study, iterations = 3, seed = 606)
  expect_s3_class(res, "irt_results")

  params <- unique(res$item_results$param)
  expect_true("a" %in% params)
  expect_true("b1" %in% params)
  # Only 1 threshold, so no b2 or higher
  expect_false("b2" %in% params)

  # Summary and plot should still work
  s <- summary(res)
  expect_s3_class(s, "summary_irt_results")

  p <- plot(res)
  expect_s3_class(p, "ggplot")
})


# ===========================================================================
# 7. GRM with irt_params_grm() helper
# ===========================================================================

test_that("GRM pipeline works with irt_params_grm() generated parameters", {
  params <- irt_params_grm(n_items = 6, n_categories = 5, seed = 99)
  design <- irt_design(
    model = "GRM",
    n_items = 6,
    item_params = params
  )
  study <- irt_study(design, sample_sizes = c(300))
  res <- irt_simulate(study, iterations = 3, seed = 707)

  expect_s3_class(res, "irt_results")
  # 5 categories → 4 thresholds
  params_found <- unique(res$item_results$param)
  expect_true(all(c("a", "b1", "b2", "b3", "b4") %in% params_found))
})


# ===========================================================================
# 8. Parameter recovery sanity (large N)
# ===========================================================================

test_that("GRM parameter estimates are in reasonable range at large N", {
  study <- make_grm_integration_study(
    n_items = 5, n_categories = 4, sample_sizes = c(1000)
  )
  res <- irt_simulate(study, iterations = 5, seed = 808)
  s <- summary(res)

  is_df <- s$item_summary
  # At N=1000 with 5 iterations, bias should be modest (not wildly off)
  bias_vals <- is_df$bias[is.finite(is_df$bias)]
  expect_true(all(abs(bias_vals) < 1.0),
              info = "Bias exceeds 1.0 — parameter recovery may be broken")

  # RMSE should be bounded
  rmse_vals <- is_df$rmse[is.finite(is_df$rmse)]
  expect_true(all(rmse_vals < 2.0),
              info = "RMSE exceeds 2.0 — parameter recovery may be broken")
})


# ===========================================================================
# 9. Print methods don't error
# ===========================================================================

test_that("print methods work for GRM pipeline objects", {
  study <- make_grm_integration_study(sample_sizes = c(300))
  res <- irt_simulate(study, iterations = 3, seed = 909)
  s <- summary(res)

  expect_output(print(res))
  expect_output(print(s))
})
