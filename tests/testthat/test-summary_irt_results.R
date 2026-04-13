# test-summary_irt_results.R
# TDD tests for summary.irt_results() S3 method
#
# summary.irt_results() should:
#   - Call compute_criterion() for each sample_size × item × param group
#   - Return an S3 object of class "summary_irt_results" with a data.frame
#   - Support optional criterion and param filtering arguments
#   - Include theta recovery summaries
#   - Have a print method
#
# These tests construct mock irt_results objects directly (no simulation)
# so they are fast and deterministic.


# =============================================================================
# Helper: build a minimal mock irt_results object
# =============================================================================
make_mock_results <- function(
  item_results,
  theta_results = NULL,
  model = "1PL",
  n_items = 5,
  sample_sizes = c(100, 200),
  iterations = 10,
  seed = 42
) {
  # Build minimal design and study stubs
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

  if (is.null(theta_results)) {
    theta_results <- data.frame(
      iteration   = rep(seq_len(iterations), each = length(sample_sizes)),
      sample_size = rep(sample_sizes, times = iterations),
      theta_cor   = runif(iterations * length(sample_sizes), 0.8, 0.99),
      theta_rmse  = runif(iterations * length(sample_sizes), 0.2, 0.5),
      converged   = TRUE,
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      item_results  = item_results,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = iterations,
      seed          = seed,
      elapsed       = 1.0
    ),
    class = "irt_results"
  )
}

# Helper: build deterministic item_results for one param at one sample_size
make_item_results_simple <- function(
  n_items = 5,
  sample_sizes = c(100, 200),
  iterations = 10,
  params = "b",
  true_values = NULL
) {
  rows <- list()
  for (ss in sample_sizes) {
    for (item in seq_len(n_items)) {
      for (param in params) {
        tv <- if (!is.null(true_values)) {
          true_values[[param]][item]
        } else {
          seq(-2, 2, length.out = n_items)[item]
        }

        # Deterministic estimates: true_value + small systematic bias + noise
        set.seed(item * 100 + ss + match(param, params))
        ests <- tv + rnorm(iterations, mean = 0.02, sd = 0.15)
        ses  <- rep(0.15, iterations)

        rows[[length(rows) + 1]] <- data.frame(
          iteration   = seq_len(iterations),
          sample_size = rep(ss, iterations),
          item        = rep(item, iterations),
          param       = rep(param, iterations),
          true_value  = rep(tv, iterations),
          estimate    = ests,
          se          = ses,
          ci_lower    = ests - 1.96 * ses,
          ci_upper    = ests + 1.96 * ses,
          converged   = rep(TRUE, iterations),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}


# =============================================================================
# Section 1: summary.irt_results — return class and structure
# =============================================================================

test_that("summary.irt_results returns an object of class summary_irt_results",
{
  ir <- make_item_results_simple()
  results <- make_mock_results(ir)
  s <- summary(results)
  expect_s3_class(s, "summary_irt_results")
})

test_that("summary result contains item_summary data.frame", {
  ir <- make_item_results_simple()
  results <- make_mock_results(ir)
  s <- summary(results)
  expect_true("item_summary" %in% names(s))
  expect_s3_class(s$item_summary, "data.frame")
})

test_that("item_summary has expected columns", {
  ir <- make_item_results_simple()
  results <- make_mock_results(ir)
  s <- summary(results)

  expected_cols <- c(
    "sample_size", "item", "param", "true_value",
    "bias", "empirical_se", "mse", "rmse", "coverage",
    "mcse_bias", "mcse_mse", "n_converged"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(s$item_summary),
                info = paste("Missing column:", col))
  }
})

test_that("item_summary has correct number of rows", {
  # 5 items × 1 param × 2 sample sizes = 10 rows
  ir <- make_item_results_simple(n_items = 5, sample_sizes = c(100, 200),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 5, sample_sizes = c(100, 200))
  s <- summary(results)
  expect_equal(nrow(s$item_summary), 10)
})

test_that("item_summary rows match all sample_size × item × param combos", {
  ir <- make_item_results_simple(n_items = 3, sample_sizes = c(100, 200, 500),
                                  params = c("a", "b"),
                                  true_values = list(
                                    a = c(1.0, 1.2, 0.8),
                                    b = c(-1, 0, 1)
                                  ))
  results <- make_mock_results(ir, model = "2PL", n_items = 3,
                               sample_sizes = c(100, 200, 500))
  s <- summary(results)
  # 3 items × 2 params × 3 sample sizes = 18 rows
  expect_equal(nrow(s$item_summary), 18)
})


# =============================================================================
# Section 2: summary.irt_results — criterion values are correct
# =============================================================================

test_that("summary bias matches hand-calculated value for a specific cell", {
  # Build a simple case: 1 item, 1 param, 1 sample_size, 4 iterations
  ir <- data.frame(
    iteration   = 1:4,
    sample_size = rep(100, 4),
    item        = rep(1, 4),
    param       = rep("b", 4),
    true_value  = rep(0.5, 4),
    estimate    = c(0.6, 0.7, 0.4, 0.5),
    se          = rep(0.2, 4),
    ci_lower    = c(0.6, 0.7, 0.4, 0.5) - 0.4,
    ci_upper    = c(0.6, 0.7, 0.4, 0.5) + 0.4,
    converged   = rep(TRUE, 4),
    stringsAsFactors = FALSE
  )

  results <- make_mock_results(ir, n_items = 1, sample_sizes = 100,
                               iterations = 4)
  s <- summary(results)

  # bias = mean(0.6, 0.7, 0.4, 0.5) - 0.5 = 0.55 - 0.5 = 0.05
  expect_equal(s$item_summary$bias[1], 0.05)
})

test_that("summary coverage matches hand-calculated value", {
  # 4 iterations; true = 0.5
  # CIs: [0.2,1.0]✓  [0.3,1.1]✓  [0.0,0.8]✓  [0.1,0.9]✓  → 4/4 = 1.0
  ir <- data.frame(
    iteration   = 1:4,
    sample_size = rep(100, 4),
    item        = rep(1, 4),
    param       = rep("b", 4),
    true_value  = rep(0.5, 4),
    estimate    = c(0.6, 0.7, 0.4, 0.5),
    se          = rep(0.2, 4),
    ci_lower    = c(0.2, 0.3, 0.0, 0.1),
    ci_upper    = c(1.0, 1.1, 0.8, 0.9),
    converged   = rep(TRUE, 4),
    stringsAsFactors = FALSE
  )

  results <- make_mock_results(ir, n_items = 1, sample_sizes = 100,
                               iterations = 4)
  s <- summary(results)
  expect_equal(s$item_summary$coverage[1], 1.0)
})


# =============================================================================
# Section 3: summary.irt_results — non-converged iteration handling
# =============================================================================

test_that("n_converged column reflects actual convergence count", {
  ir <- data.frame(
    iteration   = 1:5,
    sample_size = rep(100, 5),
    item        = rep(1, 5),
    param       = rep("b", 5),
    true_value  = rep(1.0, 5),
    estimate    = c(1.1, NA, 0.9, NA, 1.0),
    se          = c(0.1, NA, 0.1, NA, 0.1),
    ci_lower    = c(0.9, NA, 0.7, NA, 0.8),
    ci_upper    = c(1.3, NA, 1.1, NA, 1.2),
    converged   = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  results <- make_mock_results(ir, n_items = 1, sample_sizes = 100,
                               iterations = 5)
  s <- summary(results)
  expect_equal(s$item_summary$n_converged[1], 3)
})

test_that("criteria computed from converged iterations only", {
  # 4 iterations, iteration 2 is non-converged (NA estimate)
  ir <- data.frame(
    iteration   = 1:4,
    sample_size = rep(100, 4),
    item        = rep(1, 4),
    param       = rep("b", 4),
    true_value  = rep(1.0, 4),
    estimate    = c(1.1, NA, 0.9, 1.0),
    se          = c(0.1, NA, 0.1, 0.1),
    ci_lower    = c(0.9, NA, 0.7, 0.8),
    ci_upper    = c(1.3, NA, 1.1, 1.2),
    converged   = c(TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  results <- make_mock_results(ir, n_items = 1, sample_sizes = 100,
                               iterations = 4)
  s <- summary(results)

  valid_est <- c(1.1, 0.9, 1.0)
  expected_bias <- mean(valid_est) - 1.0
  expect_equal(s$item_summary$bias[1], expected_bias)
})


# =============================================================================
# Section 4: summary.irt_results — criterion filter argument
# =============================================================================

test_that("criterion argument subsets output columns", {
  ir <- make_item_results_simple(n_items = 3, sample_sizes = c(100),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 3, sample_sizes = 100)

  s <- summary(results, criterion = c("bias", "rmse"))

  # Should have bias and rmse but NOT mse, empirical_se, coverage
  expect_true("bias" %in% names(s$item_summary))
  expect_true("rmse" %in% names(s$item_summary))
  expect_false("mse" %in% names(s$item_summary))
  expect_false("empirical_se" %in% names(s$item_summary))
  expect_false("coverage" %in% names(s$item_summary))
})

test_that("criterion = 'all' or NULL returns all criteria", {
  ir <- make_item_results_simple(n_items = 2, sample_sizes = c(100),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 2, sample_sizes = 100)

  s1 <- summary(results, criterion = NULL)
  s2 <- summary(results)

  expect_true("bias" %in% names(s1$item_summary))
  expect_true("coverage" %in% names(s1$item_summary))
  expect_equal(names(s1$item_summary), names(s2$item_summary))
})

test_that("invalid criterion name raises error", {
  ir <- make_item_results_simple(n_items = 2, sample_sizes = c(100),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 2, sample_sizes = 100)

  expect_error(summary(results, criterion = "r_squared"))
})


# =============================================================================
# Section 5: summary.irt_results — param filter argument
# =============================================================================

test_that("param argument filters to specified parameters", {
  ir <- make_item_results_simple(n_items = 3, sample_sizes = c(100),
                                  params = c("a", "b"),
                                  true_values = list(
                                    a = c(1, 1.2, 0.8),
                                    b = c(-1, 0, 1)
                                  ))
  results <- make_mock_results(ir, model = "2PL", n_items = 3,
                               sample_sizes = 100)

  s <- summary(results, param = "b")
  expect_true(all(s$item_summary$param == "b"))
  expect_equal(nrow(s$item_summary), 3)  # 3 items × 1 param
})

test_that("param argument accepts vector of param names", {
  ir <- make_item_results_simple(n_items = 2, sample_sizes = c(100),
                                  params = c("a", "b1", "b2"),
                                  true_values = list(
                                    a = c(1, 1.2),
                                    b1 = c(-1, 0),
                                    b2 = c(0, 1)
                                  ))
  results <- make_mock_results(ir, model = "GRM", n_items = 2,
                               sample_sizes = 100)

  s <- summary(results, param = c("b1", "b2"))
  expect_true(all(s$item_summary$param %in% c("b1", "b2")))
  expect_equal(nrow(s$item_summary), 4)  # 2 items × 2 params
})

test_that("invalid param name raises error", {
  ir <- make_item_results_simple(n_items = 2, sample_sizes = c(100),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 2, sample_sizes = 100)

  expect_error(summary(results, param = "z"))
})


# =============================================================================
# Section 6: summary.irt_results — theta recovery summary
# =============================================================================

test_that("summary result contains theta_summary data.frame", {
  ir <- make_item_results_simple()
  results <- make_mock_results(ir)
  s <- summary(results)
  expect_true("theta_summary" %in% names(s))
  expect_s3_class(s$theta_summary, "data.frame")
})

test_that("theta_summary has one row per sample_size", {
  ir <- make_item_results_simple(sample_sizes = c(100, 200, 500))
  results <- make_mock_results(ir, sample_sizes = c(100, 200, 500))
  s <- summary(results)
  expect_equal(nrow(s$theta_summary), 3)
})

test_that("theta_summary contains mean_cor and mean_rmse columns", {
  ir <- make_item_results_simple()
  results <- make_mock_results(ir)
  s <- summary(results)
  expect_true("mean_cor" %in% names(s$theta_summary))
  expect_true("mean_rmse" %in% names(s$theta_summary))
})

test_that("theta_summary mean_cor is correct for known values", {
  theta_results <- data.frame(
    iteration   = c(1, 2, 3, 1, 2, 3),
    sample_size = c(100, 100, 100, 200, 200, 200),
    theta_cor   = c(0.90, 0.92, 0.94, 0.95, 0.96, 0.97),
    theta_rmse  = c(0.40, 0.38, 0.36, 0.30, 0.28, 0.26),
    converged   = TRUE,
    stringsAsFactors = FALSE
  )

  ir <- make_item_results_simple(sample_sizes = c(100, 200), iterations = 3)
  results <- make_mock_results(ir, sample_sizes = c(100, 200),
                               iterations = 3)
  results$theta_results <- theta_results
  s <- summary(results)

  ss100 <- s$theta_summary[s$theta_summary$sample_size == 100, ]
  ss200 <- s$theta_summary[s$theta_summary$sample_size == 200, ]

  expect_equal(ss100$mean_cor, mean(c(0.90, 0.92, 0.94)))
  expect_equal(ss200$mean_cor, mean(c(0.95, 0.96, 0.97)))
  expect_equal(ss100$mean_rmse, mean(c(0.40, 0.38, 0.36)))
  expect_equal(ss200$mean_rmse, mean(c(0.30, 0.28, 0.26)))
})


# =============================================================================
# Section 7: summary.irt_results — print method
# =============================================================================

test_that("print.summary_irt_results produces output without error", {
  ir <- make_item_results_simple(n_items = 3, sample_sizes = c(100),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 3, sample_sizes = 100)
  s <- summary(results)

  expect_output(print(s))
})

test_that("print.summary_irt_results returns object invisibly", {
  ir <- make_item_results_simple(n_items = 3, sample_sizes = c(100),
                                  params = "b")
  results <- make_mock_results(ir, n_items = 3, sample_sizes = 100)
  s <- summary(results)

  out <- withVisible(print(s))
  expect_false(out$visible)
})


# =============================================================================
# Section 8: summary.irt_results — input validation
# =============================================================================

test_that("summary errors on non-irt_results input", {
  expect_error(summary.irt_results(list(a = 1)))
})

test_that("summary preserves original results metadata", {
  ir <- make_item_results_simple()
  results <- make_mock_results(ir)
  s <- summary(results)

  expect_equal(s$iterations, results$iterations)
  expect_equal(s$seed, results$seed)
  expect_equal(s$model, results$study$design$model)
})
