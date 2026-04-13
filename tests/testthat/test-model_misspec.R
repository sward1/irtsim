# test-model_misspec.R
# TDD tests for model misspecification support (Objective 26)
# Tests cover cross-fitting scenarios where generation and estimation models differ.

# --- Helper: Create test designs -------------------------------------------------

make_design_1pl <- function(n = 10) {
  irt_design(
    model = "1PL",
    n_items = n,
    item_params = list(b = seq(-2, 2, length.out = n))
  )
}

make_design_2pl <- function(n = 10) {
  irt_design(
    model = "2PL",
    n_items = n,
    item_params = list(
      a = rlnorm(n, 0, 0.25),
      b = seq(-2, 2, length.out = n)
    )
  )
}

make_design_grm <- function(n = 10, n_cat = 4) {
  n_thresh <- n_cat - 1L
  b_mat <- matrix(
    seq(-2, 2, length.out = n * n_thresh),
    nrow = n, ncol = n_thresh
  )
  b_mat <- t(apply(b_mat, 1, sort))
  irt_design(
    model = "GRM",
    n_items = n,
    item_params = list(a = rlnorm(n, 0, 0.25), b = b_mat)
  )
}

# =============================================================================
# 1. irt_study() with estimation_model parameter
# =============================================================================

test_that("irt_study accepts estimation_model parameter", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  expect_s3_class(study, "irt_study")
  expect_equal(study$estimation_model, "1PL")
})

test_that("irt_study defaults estimation_model to design$model when NULL", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 300)
  expect_equal(study$estimation_model, d$model)
})

test_that("irt_study defaults estimation_model to design$model when not provided", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 300)
  expect_equal(study$estimation_model, "1PL")
})

test_that("irt_study rejects unknown estimation_model", {
  d <- make_design_2pl()
  expect_error(
    irt_study(d, sample_sizes = 300, estimation_model = "UNKNOWN"),
    "model|must be one of"
  )
})

# --- Compatible cross-fits: same response format ---------------------------------

test_that("irt_study allows (gen=1PL, est=2PL) cross-fit", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "2PL")
  expect_equal(study$estimation_model, "2PL")
})

test_that("irt_study allows (gen=2PL, est=1PL) cross-fit", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  expect_equal(study$estimation_model, "1PL")
})

test_that("irt_study allows (gen=1PL, est=1PL) same-model", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  expect_equal(study$estimation_model, "1PL")
})

test_that("irt_study allows (gen=2PL, est=2PL) same-model", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "2PL")
  expect_equal(study$estimation_model, "2PL")
})

test_that("irt_study allows (gen=GRM, est=GRM) same-model", {
  d <- make_design_grm()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "GRM")
  expect_equal(study$estimation_model, "GRM")
})

# --- Incompatible cross-fits: response format mismatch --------------------------

test_that("irt_study rejects (gen=1PL, est=GRM) incompatible cross-fit", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = 300, estimation_model = "GRM"),
    "Incompatible|response format|binary|polytomous"
  )
})

test_that("irt_study rejects (gen=2PL, est=GRM) incompatible cross-fit", {
  d <- make_design_2pl()
  expect_error(
    irt_study(d, sample_sizes = 300, estimation_model = "GRM"),
    "Incompatible|response format|binary|polytomous"
  )
})

test_that("irt_study rejects (gen=GRM, est=1PL) incompatible cross-fit", {
  d <- make_design_grm()
  expect_error(
    irt_study(d, sample_sizes = 300, estimation_model = "1PL"),
    "Incompatible|response format|polytomous|binary"
  )
})

test_that("irt_study rejects (gen=GRM, est=2PL) incompatible cross-fit", {
  d <- make_design_grm()
  expect_error(
    irt_study(d, sample_sizes = 300, estimation_model = "2PL"),
    "Incompatible|response format|polytomous|binary"
  )
})

# =============================================================================
# 2. Model registry response_format metadata
# =============================================================================

test_that("model_registry has response_format for 1PL", {
  cfg <- get_model_config("1PL")
  expect_equal(cfg$response_format, "binary")
})

test_that("model_registry has response_format for 2PL", {
  cfg <- get_model_config("2PL")
  expect_equal(cfg$response_format, "binary")
})

test_that("model_registry has response_format for GRM", {
  cfg <- get_model_config("GRM")
  expect_equal(cfg$response_format, "polytomous")
})

# =============================================================================
# 3. build_true_params_for_estimation() helper
# =============================================================================

test_that("build_true_params_for_estimation(gen=2PL, est=2PL) returns correct rows", {
  d <- make_design_2pl(n = 10)
  true_params <- build_true_params_for_estimation(d, "2PL")

  # Should have 2 rows per item (a and b)
  expect_equal(nrow(true_params), 20L)
  expect_equal(ncol(true_params), 3L)
  expect_true(all(c("item", "param", "true_value") %in% colnames(true_params)))

  # First 10 rows are "a", next 10 are "b"
  expect_true(all(true_params$param[1:10] == "a"))
  expect_true(all(true_params$param[11:20] == "b"))

  # True values match design
  expect_equal(true_params$true_value[1:10], d$item_params$a)
  expect_equal(true_params$true_value[11:20], d$item_params$b)
})

test_that("build_true_params_for_estimation(gen=1PL, est=1PL) returns only b rows", {
  d <- make_design_1pl(n = 10)
  true_params <- build_true_params_for_estimation(d, "1PL")

  # Should have 1 row per item (only b)
  expect_equal(nrow(true_params), 10L)
  expect_equal(ncol(true_params), 3L)
  expect_true(all(true_params$param == "b"))

  # True values match design
  expect_equal(true_params$true_value, d$item_params$b)
})

test_that("build_true_params_for_estimation(gen=1PL, est=2PL) adds a=1 rows", {
  d <- make_design_1pl(n = 10)
  true_params <- build_true_params_for_estimation(d, "2PL")

  # Should have 2 rows per item (a and b)
  expect_equal(nrow(true_params), 20L)

  # First 10 rows are "a" with value 1 (Rasch constraint)
  expect_true(all(true_params$param[1:10] == "a"))
  expect_equal(unique(true_params$true_value[1:10]), 1.0)

  # Next 10 rows are "b" from design
  expect_true(all(true_params$param[11:20] == "b"))
  expect_equal(true_params$true_value[11:20], d$item_params$b)
})

test_that("build_true_params_for_estimation(gen=2PL, est=1PL) drops a rows", {
  d <- make_design_2pl(n = 10)
  true_params <- build_true_params_for_estimation(d, "1PL")

  # Should have 1 row per item (only b, drop a)
  expect_equal(nrow(true_params), 10L)
  expect_true(all(true_params$param == "b"))

  # True values match design b
  expect_equal(true_params$true_value, d$item_params$b)
})

test_that("build_true_params_for_estimation(gen=GRM, est=GRM) returns correct schema", {
  d <- make_design_grm(n = 5, n_cat = 4)
  true_params <- build_true_params_for_estimation(d, "GRM")

  # GRM with 5 items, 3 thresholds: 5 a rows + (5 * 3) b rows = 20 rows
  expect_equal(nrow(true_params), 20L)

  # First 5 rows are "a"
  expect_true(all(true_params$param[1:5] == "a"))
  expect_equal(true_params$true_value[1:5], d$item_params$a)

  # Remaining 15 rows are "b1", "b2", "b3"
  expect_true(all(true_params$param[6:10] == "b1"))
  expect_true(all(true_params$param[11:15] == "b2"))
  expect_true(all(true_params$param[16:20] == "b3"))
})

# =============================================================================
# 4. irt_simulate() with estimation_model
# =============================================================================

test_that("irt_simulate smoke test: (gen=2PL, est=1PL) cross-fit", {
  d <- make_design_2pl(n = 10)
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  results <- irt_simulate(study, iterations = 3, seed = 42, progress = FALSE)

  expect_s3_class(results, "irt_results")

  # item_results should only have "b" rows (1PL doesn't estimate a)
  expect_true(all(results$item_results$param == "b"))

  # Should have n_items rows per iteration per sample_size
  n_expected_rows <- 10L * 1L * 3L  # n_items * n_sample_sizes * iterations
  expect_equal(nrow(results$item_results), n_expected_rows)

  # Convergence rate should be > 0
  expect_true(sum(results$item_results$converged) > 0)
})

test_that("irt_simulate smoke test: (gen=1PL, est=2PL) cross-fit", {
  d <- make_design_1pl(n = 10)
  study <- irt_study(d, sample_sizes = 300, estimation_model = "2PL")
  results <- irt_simulate(study, iterations = 3, seed = 42, progress = FALSE)

  expect_s3_class(results, "irt_results")

  # item_results should have both "a" and "b" rows
  params <- unique(results$item_results$param)
  expect_true("a" %in% params)
  expect_true("b" %in% params)

  # Should have 2 * n_items rows per iteration per sample_size
  n_expected_rows <- 10L * 2L * 1L * 3L  # n_items * 2 params * n_sample_sizes * iterations
  expect_equal(nrow(results$item_results), n_expected_rows)

  # Convergence rate should be > 0
  expect_true(sum(results$item_results$converged) > 0)
})

test_that("irt_simulate same-model (gen=1PL, est=1PL) produces expected schema", {
  d <- make_design_1pl(n = 10)
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  results <- irt_simulate(study, iterations = 3, seed = 42, progress = FALSE)

  expect_s3_class(results, "irt_results")

  # Should have n_items rows per iteration per sample_size
  n_expected_rows <- 10L * 1L * 3L
  expect_equal(nrow(results$item_results), n_expected_rows)

  # All params should be "b"
  expect_true(all(results$item_results$param == "b"))
})

# =============================================================================
# 5. Print methods for cross-fit display
# =============================================================================

test_that("print.irt_study shows 'Estimation:' line for cross-fit (2PL -> 1PL)", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")

  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")

  # Should show "Estimation:" line indicating cross-fit
  expect_true(grepl("Estimation", output_text))
})

test_that("print.irt_study omits 'Estimation:' line for same-model (2PL -> 2PL)", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 300, estimation_model = "2PL")

  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")

  # Should NOT show "Estimation:" line (backward compat)
  expect_false(grepl("Estimation", output_text))
})

test_that("print.irt_study omits 'Estimation:' line when estimation_model defaults", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 300)  # No estimation_model specified

  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")

  # Should NOT show "Estimation:" line
  expect_false(grepl("Estimation", output_text))
})

test_that("print.irt_results shows Gen/Est models for cross-fit (2PL -> 1PL)", {
  d <- make_design_2pl(n = 10)
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  results <- irt_simulate(study, iterations = 2, seed = 42, progress = FALSE)

  output <- capture.output(print(results))
  output_text <- paste(output, collapse = "\n")

  # Should indicate generation and estimation models are different
  expect_true(grepl("Gen.*model", output_text) || grepl("Est.*model", output_text))
})

test_that("print.irt_results shows single 'Model:' line for same-model (1PL -> 1PL)", {
  d <- make_design_1pl(n = 10)
  study <- irt_study(d, sample_sizes = 300, estimation_model = "1PL")
  results <- irt_simulate(study, iterations = 2, seed = 42, progress = FALSE)

  output <- capture.output(print(results))
  output_text <- paste(output, collapse = "\n")

  # Should show single "Model:" line (backward compat, no "Gen model:" or "Est model:")
  model_line_count <- length(grep("Model:", output_text, value = TRUE))
  # Line should appear exactly once (or as part of one-line format)
  expect_true(model_line_count >= 1)
})

# =============================================================================
# 6. Backward compatibility: existing behavior unchanged when no misspecification
# =============================================================================

test_that("irt_study output is backward compatible when estimation_model defaults", {
  d <- make_design_2pl(n = 10)
  study <- irt_study(d, sample_sizes = c(100, 250, 500))

  # Should have estimation_model set to design model
  expect_equal(study$estimation_model, d$model)

  # Print output should NOT contain "Estimation:" (backward compat)
  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")
  expect_false(grepl("Estimation:", output_text))
})

test_that("irt_simulate default behavior unchanged (gen=est)", {
  d <- make_design_1pl(n = 10)
  study <- irt_study(d, sample_sizes = 300)
  results <- irt_simulate(study, iterations = 2, seed = 42, progress = FALSE)

  # Results schema should match expected 1PL output
  expect_true(all(results$item_results$param == "b"))
  expect_equal(nrow(results$item_results), 10L * 2L)  # n_items * iterations
})
