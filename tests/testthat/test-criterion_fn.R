# test-criterion_fn.R
# TDD tests for custom criterion callback feature (Objective 27)
#
# Covers custom criterion_fn parameter in summary.irt_results() that allows
# users to compute item-level custom performance criteria, augmenting the
# built-in Morris et al. (2019) criteria.

# =============================================================================
# Helper: Build a minimal irt_results fixture for fast tests
# =============================================================================

make_fixture_results <- function(
  n_items = 3,
  sample_sizes = c(100, 200),
  iterations = 5,
  model = "1PL",
  seed = 42
) {
  # Build item_results with deterministic estimates
  rows <- list()
  set.seed(seed)
  for (ss in sample_sizes) {
    for (item in seq_len(n_items)) {
      for (param in "b") {
        tv <- seq(-2, 2, length.out = n_items)[item]
        ests <- tv + rnorm(iterations, mean = 0.01, sd = 0.1)
        rows[[length(rows) + 1]] <- data.frame(
          iteration   = seq_len(iterations),
          sample_size = rep(ss, iterations),
          item        = rep(item, iterations),
          param       = rep(param, iterations),
          true_value  = rep(tv, iterations),
          estimate    = ests,
          se          = rep(0.1, iterations),
          ci_lower    = ests - 0.2,
          ci_upper    = ests + 0.2,
          converged   = rep(TRUE, iterations),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  item_results <- do.call(rbind, rows)
  rownames(item_results) <- NULL

  # Build theta_results
  theta_results <- data.frame(
    iteration   = rep(seq_len(iterations), each = length(sample_sizes)),
    sample_size = rep(sample_sizes, times = iterations),
    theta_cor   = runif(iterations * length(sample_sizes), 0.8, 0.99),
    theta_rmse  = runif(iterations * length(sample_sizes), 0.2, 0.5),
    converged   = TRUE,
    stringsAsFactors = FALSE
  )

  # Build design and study stubs
  design_stub <- list(
    model = model,
    n_items = n_items,
    item_params = list(b = seq(-2, 2, length.out = n_items))
  )
  class(design_stub) <- "irt_design"

  study_stub <- list(
    design = design_stub,
    sample_sizes = sample_sizes,
    missing_type = "none"
  )
  class(study_stub) <- "irt_study"

  structure(
    list(
      item_results  = item_results,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = iterations,
      seed          = seed,
      elapsed       = 0.1
    ),
    class = "irt_results"
  )
}


# =============================================================================
# Test 1: Happy path — single named scalar
# =============================================================================

test_that("criterion_fn returning single named scalar creates correct column", {
  results <- make_fixture_results()

  # Simple callback: return constant value
  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(my_stat = 0.5)
  }

  s <- summary(results, criterion_fn = my_fn)

  # Should have my_stat column
  expect_true("my_stat" %in% names(s$item_summary))

  # All values should be 0.5 (since callback always returns 0.5)
  expect_true(all(s$item_summary$my_stat == 0.5, na.rm = TRUE))

  # Standard columns should still exist
  expect_true("bias" %in% names(s$item_summary))
  expect_true("sample_size" %in% names(s$item_summary))
})


# =============================================================================
# Test 2: Happy path — multiple named scalars
# =============================================================================

test_that("criterion_fn returning multiple named scalars creates all columns", {
  results <- make_fixture_results()

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(stat_a = 1.0, stat_b = 2.5)
  }

  s <- summary(results, criterion_fn = my_fn)

  # Both columns should exist
  expect_true("stat_a" %in% names(s$item_summary))
  expect_true("stat_b" %in% names(s$item_summary))

  # Check values
  expect_true(all(s$item_summary$stat_a == 1.0, na.rm = TRUE))
  expect_true(all(s$item_summary$stat_b == 2.5, na.rm = TRUE))
})


# =============================================================================
# Test 3: Backward compatibility — no criterion_fn produces identical output
# =============================================================================

test_that("summary with no criterion_fn identical to criterion_fn = NULL", {
  results <- make_fixture_results()

  s1 <- summary(results)
  s2 <- summary(results, criterion_fn = NULL)

  # Same columns
  expect_equal(names(s1$item_summary), names(s2$item_summary))

  # Same data
  expect_equal(s1$item_summary, s2$item_summary)
})

test_that("backward compat: no custom columns without criterion_fn", {
  results <- make_fixture_results()

  s <- summary(results)

  # Should have only built-in columns (no custom columns)
  expected_cols <- c(
    "sample_size", "item", "param", "true_value",
    "bias", "empirical_se", "mse", "rmse", "coverage",
    "mcse_bias", "mcse_mse", "n_converged"
  )
  expect_equal(sort(names(s$item_summary)), sort(expected_cols))
})


# =============================================================================
# Test 4: Callback receives correct data per cell
# =============================================================================

test_that("callback receives correct estimates, true_value, CIs, converged", {
  results <- make_fixture_results(n_items = 1, sample_sizes = 100, iterations = 3)

  # Capture inputs in a parent-scope list
  captured <- list()

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    # Verify structure
    expect_true(is.numeric(estimates))
    expect_true(is.numeric(true_value))
    expect_length(true_value, 1)
    expect_true(is.numeric(ci_lower))
    expect_true(is.numeric(ci_upper))
    expect_true(is.logical(converged))

    # Verify lengths match
    expect_length(estimates, length(converged))
    expect_length(ci_lower, length(converged))
    expect_length(ci_upper, length(converged))

    c(test_stat = 0)
  }

  s <- summary(results, criterion_fn = my_fn)

  # If we got here, all expect_true calls inside the callback passed
  expect_true(TRUE)
})


# =============================================================================
# Test 5: Error — non-function criterion_fn
# =============================================================================

test_that("criterion_fn = string raises informative error", {
  results <- make_fixture_results()

  expect_error(
    summary(results, criterion_fn = "not_a_function"),
    "criterion_fn.*function"
  )
})

test_that("criterion_fn = list raises informative error", {
  results <- make_fixture_results()

  expect_error(
    summary(results, criterion_fn = list(a = 1)),
    "criterion_fn.*function"
  )
})


# =============================================================================
# Test 6: Error — callback returns non-numeric
# =============================================================================

test_that("callback returning character vector raises error with cell context", {
  results <- make_fixture_results(n_items = 1, sample_sizes = 100)

  bad_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(x = "hello")
  }

  expect_error(
    summary(results, criterion_fn = bad_fn),
    "numeric.*sample_size.*item.*param"
  )
})

test_that("callback returning list raises error", {
  results <- make_fixture_results(n_items = 1, sample_sizes = 100)

  bad_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    list(x = 1)
  }

  expect_error(
    summary(results, criterion_fn = bad_fn),
    "numeric"
  )
})


# =============================================================================
# Test 7: Error — callback returns unnamed vector
# =============================================================================

test_that("callback returning unnamed numeric vector raises error", {
  results <- make_fixture_results(n_items = 1, sample_sizes = 100)

  bad_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(1, 2, 3)
  }

  expect_error(
    summary(results, criterion_fn = bad_fn),
    "named"
  )
})

test_that("callback with some empty names raises error", {
  results <- make_fixture_results(n_items = 1, sample_sizes = 100)

  bad_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(a = 1, 2)
  }

  expect_error(
    summary(results, criterion_fn = bad_fn),
    "named"
  )
})


# =============================================================================
# Test 8: Error — inconsistent names across cells
# =============================================================================

test_that("callback returns inconsistent names across cells raises error", {
  results <- make_fixture_results(n_items = 2, sample_sizes = c(100, 200))

  call_count <- 0
  bad_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    call_count <<- call_count + 1
    # First few calls return 'a', later calls return 'b'
    if (call_count <= 3) {
      c(a = 1)
    } else {
      c(b = 2)
    }
  }

  expect_error(
    summary(results, criterion_fn = bad_fn),
    "consistent names|reference"
  )
})


# =============================================================================
# Test 9: Error — callback throws
# =============================================================================

test_that("callback throwing error wrapped with cell context", {
  results <- make_fixture_results(n_items = 1, sample_sizes = 100)

  bad_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    stop("oh no", call. = FALSE)
  }

  expect_error(
    summary(results, criterion_fn = bad_fn),
    "oh no"
  )
  # Error should also include cell identifier
  expect_error(
    summary(results, criterion_fn = bad_fn),
    "sample_size.*item.*param"
  )
})


# =============================================================================
# Test 10: Compatibility with criterion filter
# =============================================================================

test_that("criterion filter keeps custom columns even when filtering built-ins", {
  results <- make_fixture_results()

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(my_custom = 99.9)
  }

  s <- summary(results, criterion = "bias", criterion_fn = my_fn)

  # Built-in columns should be filtered
  expect_true("bias" %in% names(s$item_summary))
  expect_false("mse" %in% names(s$item_summary))

  # But custom column should still be present
  expect_true("my_custom" %in% names(s$item_summary))
})


# =============================================================================
# Test 11: Compatibility with param filter
# =============================================================================

test_that("param filter works with criterion_fn", {
  results <- make_fixture_results(n_items = 3)

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(custom_stat = 42)
  }

  # Only summarize "b" parameter (which is all we have in this fixture)
  s <- summary(results, param = "b", criterion_fn = my_fn)

  # All rows should be param "b"
  expect_true(all(s$item_summary$param == "b"))

  # Custom column should still exist
  expect_true("custom_stat" %in% names(s$item_summary))
})


# =============================================================================
# Test 12: Compatibility with model misspecification (cross-fit)
# =============================================================================

test_that("criterion_fn works with cross-fit (gen=1PL, est=2PL)", {
  # Build a 1PL design and 2PL estimation model
  design <- irt_design(
    model = "1PL",
    n_items = 3,
    item_params = list(b = c(-1, 0, 1))
  )

  study <- irt_study(
    design,
    sample_sizes = 100,
    estimation_model = "2PL"
  )

  results <- irt_simulate(study, iterations = 3, seed = 42, progress = FALSE)

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(cross_fit_stat = mean(estimates, na.rm = TRUE))
  }

  # Should not error and should produce custom column
  s <- summary(results, criterion_fn = my_fn)

  expect_true("cross_fit_stat" %in% names(s$item_summary))
  # Estimation model is 2PL, so extract_params() returns both a and b for each
  # of the 3 items — 6 rows per sample_size. Obj 26's
  # build_true_params_for_estimation() adds a=1 rows (Rasch constraint) so true
  # values exist for all estimated params.
  expect_equal(nrow(s$item_summary), 6)  # 3 items × 2 params (a + b)
})


# =============================================================================
# Test 13: theta_summary unchanged regardless of criterion_fn
# =============================================================================

test_that("theta_summary unaffected by criterion_fn", {
  results <- make_fixture_results()

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(custom = 1.5)
  }

  s1 <- summary(results)
  s2 <- summary(results, criterion_fn = my_fn)

  # theta_summary should be identical
  expect_equal(s1$theta_summary, s2$theta_summary)
})


# =============================================================================
# Test 14: Callback can use ... to accept future parameters
# =============================================================================

test_that("callback with ... accepts named args without error", {
  results <- make_fixture_results()

  # Function with ... for future-proofing
  flexible_fn <- function(estimates, true_value, ...) {
    c(flexible_stat = 3.14)
  }

  expect_no_error(
    summary(results, criterion_fn = flexible_fn)
  )
})


# =============================================================================
# Test 15: Custom columns positioned correctly in output
# =============================================================================

test_that("custom columns appended after n_converged", {
  results <- make_fixture_results()

  my_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
    c(custom_a = 1, custom_b = 2)
  }

  s <- summary(results, criterion_fn = my_fn)

  col_names <- names(s$item_summary)

  # Find positions
  n_converged_pos <- match("n_converged", col_names)
  custom_a_pos <- match("custom_a", col_names)
  custom_b_pos <- match("custom_b", col_names)

  # Custom columns should come after n_converged
  expect_true(custom_a_pos > n_converged_pos)
  expect_true(custom_b_pos > n_converged_pos)
})
