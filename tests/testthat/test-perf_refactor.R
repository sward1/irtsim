# test-perf_refactor.R
# TDD tests for performance refactor (Objective 24):
# - Row order invariant for summary
# - Schema invariant for item_summary
# - No scaffolding columns leak into final output
# - Pre-indexed true_params in extract_params (validation via integration test)

test_that("summary() returns item_summary with correct lex-order rows", {
  # Create a minimal mock irt_results with multiple sample sizes, items, params
  design_stub <- list(
    model = "2PL",
    n_items = 3,
    item_params = list(
      a = rep(1.0, 3),
      b = seq(-1, 1, length.out = 3)
    )
  )
  class(design_stub) <- "irt_design"

  study_stub <- list(
    design = design_stub,
    sample_sizes = c(100, 200),
    missing_type = "none"
  )
  class(study_stub) <- "irt_study"

  # Build item_results with deterministic values (unsorted)
  ir <- data.frame(
    iteration   = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
    sample_size = c(200L, 100L, 200L, 100L, 200L, 100L,
                    200L, 100L, 200L, 100L, 200L, 100L),
    item        = c(1L, 1L, 2L, 2L, 3L, 3L,
                    1L, 1L, 2L, 2L, 3L, 3L),
    param       = c("b", "a", "b", "a", "b", "a",
                    "b", "a", "b", "a", "b", "a"),
    true_value  = c(-1, 1.0, 0, 1.0, 1, 1.0,
                    -1, 1.0, 0, 1.0, 1, 1.0),
    estimate    = c(-0.95, 0.99, 0.05, 1.01, 0.95, 0.98,
                    -1.05, 1.02, -0.05, 0.99, 1.05, 1.01),
    se          = rep(0.15, 12),
    ci_lower    = rep(-0.3, 12),
    ci_upper    = rep(0.3, 12),
    converged   = rep(TRUE, 12),
    stringsAsFactors = FALSE
  )

  theta_results <- data.frame(
    iteration   = c(1, 2),
    sample_size = c(100, 100),
    theta_cor   = c(0.95, 0.96),
    theta_rmse  = c(0.25, 0.24),
    converged   = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  results <- structure(
    list(
      item_results  = ir,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = 2,
      seed          = 42,
      elapsed       = 0.1
    ),
    class = "irt_results"
  )

  s <- summary(results)
  is_df <- s$item_summary

  # Verify lex order: sorted by sample_size ASC, then item ASC, then param ASC
  expected_order <- is_df[order(is_df$sample_size, is_df$item, is_df$param), ]
  expect_equal(is_df, expected_order)

  # Verify the first row is lowest sample_size and param="a" (lex order: "a" < "b")
  expect_equal(is_df$sample_size[1], 100L)
  expect_equal(is_df$item[1], 1L)
  expect_equal(is_df$param[1], "a")
})


test_that("summary() item_summary has expected schema and no extra columns", {
  design_stub <- list(
    model = "1PL",
    n_items = 2,
    item_params = list(b = c(-1, 1))
  )
  class(design_stub) <- "irt_design"

  study_stub <- list(
    design = design_stub,
    sample_sizes = c(100),
    missing_type = "none"
  )
  class(study_stub) <- "irt_study"

  ir <- data.frame(
    iteration   = c(1, 2, 1, 2),
    sample_size = c(100, 100, 100, 100),
    item        = c(1, 1, 2, 2),
    param       = c("b", "b", "b", "b"),
    true_value  = c(-1, -1, 1, 1),
    estimate    = c(-0.95, -1.05, 0.95, 1.05),
    se          = rep(0.15, 4),
    ci_lower    = rep(-0.3, 4),
    ci_upper    = rep(0.3, 4),
    converged   = rep(TRUE, 4),
    stringsAsFactors = FALSE
  )

  theta_results <- data.frame(
    iteration   = c(1, 2),
    sample_size = c(100, 100),
    theta_cor   = c(0.95, 0.96),
    theta_rmse  = c(0.25, 0.24),
    converged   = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  results <- structure(
    list(
      item_results  = ir,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = 2,
      seed          = 42,
      elapsed       = 0.1
    ),
    class = "irt_results"
  )

  s <- summary(results)

  # Verify schema
  expected_cols <- c("sample_size", "item", "param", "true_value", "bias",
                     "empirical_se", "mse", "rmse", "coverage", "mcse_bias",
                     "mcse_mse", "n_converged")
  expect_named(s$item_summary, expected_cols)

  # Verify no ".key", ".group", or scaffolding columns
  actual_cols <- names(s$item_summary)
  expect_false(any(grepl("^\\..*", actual_cols)))
})


test_that("summary() item_summary with coverage=NULL gives NA_real_", {
  design_stub <- list(
    model = "1PL",
    n_items = 1,
    item_params = list(b = c(0))
  )
  class(design_stub) <- "irt_design"

  study_stub <- list(
    design = design_stub,
    sample_sizes = c(100),
    missing_type = "none"
  )
  class(study_stub) <- "irt_study"

  # Item results with no CI columns so coverage will be NULL
  ir <- data.frame(
    iteration   = c(1, 2),
    sample_size = c(100, 100),
    item        = c(1, 1),
    param       = c("b", "b"),
    true_value  = c(0, 0),
    estimate    = c(0.05, -0.05),
    se          = rep(NA_real_, 2),
    ci_lower    = rep(NA_real_, 2),
    ci_upper    = rep(NA_real_, 2),
    converged   = rep(TRUE, 2),
    stringsAsFactors = FALSE
  )

  theta_results <- data.frame(
    iteration   = c(1, 2),
    sample_size = c(100, 100),
    theta_cor   = c(0.95, 0.96),
    theta_rmse  = c(0.25, 0.24),
    converged   = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  results <- structure(
    list(
      item_results  = ir,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = 2,
      seed          = 42,
      elapsed       = 0.1
    ),
    class = "irt_results"
  )

  s <- summary(results)

  # coverage should be NA_real_ when CIs are not available
  expect_true(is.na(s$item_summary$coverage[1]))
  expect_true(is.numeric(s$item_summary$coverage))
})


test_that("1PL extract_params pre-indexed lookup returns correct true_values", {
  skip_if_not_installed("mirt")

  design <- irt_design(
    model = "1PL",
    n_items = 3,
    item_params = list(b = c(-1, 0, 1))
  )

  # Tiny simulation: 1 iteration, 1 sample size, just to test extract_params
  results <- irt_simulate(
    irt_study(design, sample_sizes = c(50)),
    iterations = 1,
    seed = 42,
    progress = FALSE
  )

  # Verify that true_value column matches expected design values
  ir <- results$item_results
  expect_equal(ir$param, c("b", "b", "b"))
  expect_equal(ir$item, c(1L, 2L, 3L))
  expect_equal(ir$true_value, c(-1, 0, 1))
})


test_that("2PL extract_params pre-indexed lookup returns correct true_values", {
  skip_if_not_installed("mirt")

  design <- irt_design(
    model = "2PL",
    n_items = 2,
    item_params = list(
      a = c(0.8, 1.2),
      b = c(-1, 1)
    )
  )

  results <- irt_simulate(
    irt_study(design, sample_sizes = c(50)),
    iterations = 1,
    seed = 43,
    progress = FALSE
  )

  ir <- results$item_results
  # 2PL has both a and b per item; in build_true_params order: all a's, then all b's
  # actually, build_true_params returns rbind(a_df, b_df), so rows are:
  # item 1,2 param=a; then item 1,2 param=b
  a_rows <- ir[ir$param == "a", ]
  b_rows <- ir[ir$param == "b", ]

  expect_equal(a_rows$true_value, c(0.8, 1.2))
  expect_equal(b_rows$true_value, c(-1, 1))
})


test_that("GRM extract_params pre-indexed lookup returns correct true_values", {
  skip_if_not_installed("mirt")

  design <- irt_design(
    model = "GRM",
    n_items = 2,
    item_params = list(
      a = c(0.9, 1.1),
      b = matrix(c(-1, 0.5, 0, 1.5), nrow = 2, ncol = 2)
    )
  )

  results <- irt_simulate(
    irt_study(design, sample_sizes = c(50)),
    iterations = 1,
    seed = 44,
    progress = FALSE
  )

  ir <- results$item_results
  # GRM: a per item, then b1 per item, then b2 per item
  a_rows <- ir[ir$param == "a", ]
  b1_rows <- ir[ir$param == "b1", ]
  b2_rows <- ir[ir$param == "b2", ]

  expect_equal(a_rows$true_value, c(0.9, 1.1))
  expect_equal(b1_rows$true_value, c(-1, 0.5))
  expect_equal(b2_rows$true_value, c(0, 1.5))
})


test_that("summary() theta_summary has correct lex order by sample_size", {
  design_stub <- list(
    model = "1PL",
    n_items = 2,
    item_params = list(b = c(-1, 1))
  )
  class(design_stub) <- "irt_design"

  study_stub <- list(
    design = design_stub,
    sample_sizes = c(200, 100),
    missing_type = "none"
  )
  class(study_stub) <- "irt_study"

  ir <- data.frame(
    iteration   = c(1, 2),
    sample_size = c(100, 100),
    item        = c(1, 1),
    param       = c("b", "b"),
    true_value  = c(-1, -1),
    estimate    = c(-0.95, -1.05),
    se          = rep(0.15, 2),
    ci_lower    = rep(-0.3, 2),
    ci_upper    = rep(0.3, 2),
    converged   = rep(TRUE, 2),
    stringsAsFactors = FALSE
  )

  # theta_results intentionally unsorted
  theta_results <- data.frame(
    iteration   = c(1, 2),
    sample_size = c(200, 100),
    theta_cor   = c(0.95, 0.96),
    theta_rmse  = c(0.25, 0.24),
    converged   = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  results <- structure(
    list(
      item_results  = ir,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = 2,
      seed          = 42,
      elapsed       = 0.1
    ),
    class = "irt_results"
  )

  s <- summary(results)

  # theta_summary should be sorted by sample_size
  expect_equal(s$theta_summary$sample_size, c(100, 200))
})
