# test-perf_simulate_opts.R
# Tests for se and compute_theta optimization parameters in irt_simulate()
#
# Uses minimal test fixtures (5 items, N=100-200, 3 iterations) for speed.

test_that("se = FALSE produces NA se/ci columns", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    se         = FALSE
  )

  # All item_results$se should be NA when se = FALSE
  expect_true(all(is.na(results$item_results$se)))

  # All item_results$ci_lower should be NA when se = FALSE
  expect_true(all(is.na(results$item_results$ci_lower)))

  # All item_results$ci_upper should be NA when se = FALSE
  expect_true(all(is.na(results$item_results$ci_upper)))

  # But point estimates should still be present for converged iterations
  converged_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$estimate[converged_idx])))

  # Check that se=FALSE is stored on the object
  expect_equal(results$se, FALSE)
})

test_that("se = TRUE (default) produces non-NA se/ci columns for converged iterations", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    se         = TRUE
  )

  # For converged rows, se/ci should NOT be all NA
  converged_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$se[converged_idx])))
  expect_true(any(!is.na(results$item_results$ci_lower[converged_idx])))
  expect_true(any(!is.na(results$item_results$ci_upper[converged_idx])))

  # Check that se=TRUE is stored
  expect_equal(results$se, TRUE)
})

test_that("se parameter with default TRUE (omitted) produces non-NA se/ci", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  # Omit se parameter to test default
  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE
  )

  # Should default to se=TRUE
  expect_equal(results$se, TRUE)

  # Converged rows should have non-NA se/ci
  converged_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$se[converged_idx])))
})

test_that("se parameter validation rejects non-logical input", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  # se = "yes" should fail
  expect_error(
    irt_simulate(study, iterations = 3L, seed = 1L, se = "yes"),
    "logical"
  )

  # se = NA should fail
  expect_error(
    irt_simulate(study, iterations = 3L, seed = 1L, se = NA),
    "logical"
  )

  # se = c(TRUE, FALSE) should fail (length > 1)
  expect_error(
    irt_simulate(study, iterations = 3L, seed = 1L, se = c(TRUE, FALSE)),
    "logical"
  )
})

test_that("compute_theta = FALSE produces NA theta columns but tracks convergence", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    compute_theta = FALSE
  )

  # All theta_results$theta_cor should be NA when compute_theta = FALSE
  expect_true(all(is.na(results$theta_results$theta_cor)))

  # All theta_results$theta_rmse should be NA when compute_theta = FALSE
  expect_true(all(is.na(results$theta_results$theta_rmse)))

  # But converged should still be tracked (TRUE for successful fits)
  expect_true(any(results$theta_results$converged))

  # Check that compute_theta=FALSE is stored
  expect_equal(results$compute_theta, FALSE)
})

test_that("compute_theta = TRUE (default) produces non-NA theta columns for converged", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    compute_theta = TRUE
  )

  # For converged rows, theta_cor and theta_rmse should NOT be all NA
  converged_idx <- results$theta_results$converged
  expect_true(any(!is.na(results$theta_results$theta_cor[converged_idx])))
  expect_true(any(!is.na(results$theta_results$theta_rmse[converged_idx])))

  # Check that compute_theta=TRUE is stored
  expect_equal(results$compute_theta, TRUE)
})

test_that("compute_theta parameter with default TRUE (omitted) produces non-NA theta", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  # Omit compute_theta to test default
  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE
  )

  # Should default to compute_theta=TRUE
  expect_equal(results$compute_theta, TRUE)

  # Converged rows should have non-NA theta metrics
  converged_idx <- results$theta_results$converged
  expect_true(any(!is.na(results$theta_results$theta_cor[converged_idx])))
})

test_that("compute_theta parameter validation rejects non-logical input", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  # compute_theta = "yes" should fail
  expect_error(
    irt_simulate(study, iterations = 3L, seed = 1L, compute_theta = "yes"),
    "logical"
  )

  # compute_theta = NA should fail
  expect_error(
    irt_simulate(study, iterations = 3L, seed = 1L, compute_theta = NA),
    "logical"
  )

  # compute_theta = c(TRUE, FALSE) should fail (length > 1)
  expect_error(
    irt_simulate(study, iterations = 3L, seed = 1L, compute_theta = c(TRUE, FALSE)),
    "logical"
  )
})

test_that("se = FALSE and compute_theta = FALSE work together", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    se         = FALSE,
    compute_theta = FALSE
  )

  # item_results: estimates present for converged, se/ci are NA
  converged_item_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$estimate[converged_item_idx])))
  expect_true(all(is.na(results$item_results$se)))
  expect_true(all(is.na(results$item_results$ci_lower)))
  expect_true(all(is.na(results$item_results$ci_upper)))

  # theta_results: theta metrics are NA but convergence is tracked
  expect_true(all(is.na(results$theta_results$theta_cor)))
  expect_true(all(is.na(results$theta_results$theta_rmse)))
  expect_true(any(results$theta_results$converged))

  # Check stored flags
  expect_equal(results$se, FALSE)
  expect_equal(results$compute_theta, FALSE)
})

test_that("default parameters preserve existing output schema", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  # Call with all defaults (no se or compute_theta specified)
  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE
  )

  # Check item_results schema
  expected_cols <- c("iteration", "sample_size", "item", "param", "true_value",
                     "estimate", "se", "ci_lower", "ci_upper", "converged")
  expect_equal(colnames(results$item_results), expected_cols)

  # Check theta_results schema
  expected_theta_cols <- c("iteration", "sample_size", "theta_cor", "theta_rmse", "converged")
  expect_equal(colnames(results$theta_results), expected_theta_cols)

  # Check result object fields
  expect_true(all(c("se", "compute_theta") %in% names(results)))
  expect_equal(results$se, TRUE)
  expect_equal(results$compute_theta, TRUE)
})

test_that("se = FALSE works with parallel = TRUE", {
  skip_if_not_installed("future")

  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  # Use a sequential plan to avoid multisession complexity in testing
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    parallel   = TRUE,
    se         = FALSE,
    compute_theta = FALSE
  )

  # Check NA patterns match serial mode
  expect_true(all(is.na(results$item_results$se)))
  expect_true(all(is.na(results$item_results$ci_lower)))
  expect_true(all(is.na(results$item_results$ci_upper)))
  expect_true(all(is.na(results$theta_results$theta_cor)))
  expect_true(all(is.na(results$theta_results$theta_rmse)))

  # But estimates should still be present
  converged_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$estimate[converged_idx])))
})

test_that("2PL model with se = FALSE produces NA se/ci", {
  design <- irt_design(
    model       = "2PL",
    n_items     = 4L,
    item_params = list(
      a = rep(1.5, 4L),
      b = seq(-1, 1, length.out = 4L)
    ),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 2L,
    seed       = 1L,
    progress   = FALSE,
    se         = FALSE
  )

  # All se/ci should be NA for 2PL as well
  expect_true(all(is.na(results$item_results$se)))
  expect_true(all(is.na(results$item_results$ci_lower)))
  expect_true(all(is.na(results$item_results$ci_upper)))

  # But estimates should be present for converged rows (both a and b)
  converged_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$estimate[converged_idx])))
})

test_that("GRM model with se = FALSE produces NA se/ci", {
  # Use 5 items with sorted thresholds and N=500 for reliable GRM convergence
  # (matches test-grm_integration.R fixture pattern)
  n_items_grm <- 5L
  n_thresh <- 3L
  b_mat <- matrix(
    seq(-2, 2, length.out = n_items_grm * n_thresh),
    nrow = n_items_grm, ncol = n_thresh
  )
  b_mat <- t(apply(b_mat, 1, sort))

  design <- irt_design(
    model       = "GRM",
    n_items     = n_items_grm,
    item_params = list(a = rep(1.2, n_items_grm), b = b_mat),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 500L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 101L,
    progress   = FALSE,
    se         = FALSE
  )

  # All se/ci should be NA for GRM as well
  expect_true(all(is.na(results$item_results$se)))
  expect_true(all(is.na(results$item_results$ci_lower)))
  expect_true(all(is.na(results$item_results$ci_upper)))

  # But estimates should be present for converged rows
  converged_idx <- results$item_results$converged
  expect_true(any(!is.na(results$item_results$estimate[converged_idx])))
})

test_that("se = TRUE uses fast Oakes SE.type (SEs are computed and valid)", {
  design <- irt_design(
    model       = "1PL",
    n_items     = 5L,
    item_params = list(b = seq(-1, 1, length.out = 5L)),
    theta_dist  = "normal"
  )

  study <- irt_study(design, sample_sizes = 100L)

  results <- irt_simulate(
    study,
    iterations = 3L,
    seed       = 1L,
    progress   = FALSE,
    se         = TRUE
  )

  # For converged rows, SEs should be finite and positive
  converged_idx <- results$item_results$converged
  se_values <- results$item_results$se[converged_idx]
  expect_true(all(is.finite(se_values)))
  expect_true(all(se_values > 0))
})
