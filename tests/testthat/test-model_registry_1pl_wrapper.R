# test-model_registry_1pl_wrapper.R
# TDD tests for Obj 37 sub-step (f): user-facing irt_params_1pl() wrapper.
#
# Registry method already lives on .model_1pl() (sub-step a, Session 25).
# This wrapper exists purely to close the irt_params_* family so the helper
# set is symmetric across 1PL/2PL/3PL/GRM/PCM/GPCM (the registry order
# enforced by R/irt_params_helpers.R).
#
# Contract:
#   - Thin delegator over get_model_config("1PL")$generate_default_params().
#   - Signature mirrors irt_params_2pl() minus the a_* args:
#       n_items, b_dist, b_mean, b_sd, b_range, seed.
#   - Returns list(b = numeric(n_items)) — single-element list, no `a`
#     (1PL fixes a = 1 inside validate_params, not at generation time).
#   - Defaults match the registry method exactly (single source of truth).
#
# Registry-level coverage for the 1PL generate_default_params method lives
# in test-model_registry_generate_default_params.R; this file is wrapper-only.


# --- Wrapper: irt_params_1pl ------------------------------------------------

test_that("irt_params_1pl is exported and callable", {
  expect_true(exists("irt_params_1pl", mode = "function"))
})

test_that("irt_params_1pl returns list(b) with expected shape", {
  out <- irt_params_1pl(n_items = 10, seed = 5)
  expect_true(is.list(out))
  expect_named(out, "b")
  expect_length(out$b, 10)
  expect_type(out$b, "double")
  # 1PL wrapper must NOT return `a` — the Rasch a = 1 contract is enforced
  # downstream in validate_params, not at generation time. Matches the
  # registry method's list(b = ...) return shape.
  expect_false("a" %in% names(out))
})

test_that("irt_params_1pl delegates to the registry method (byte-for-byte)", {
  cfg <- get_model_config("1PL")
  expected <- cfg$generate_default_params(
    n_items = 25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    seed = 2025
  )
  observed <- irt_params_1pl(
    n_items = 25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    seed = 2025
  )
  expect_equal(observed, expected)
})

test_that("irt_params_1pl default args match registry defaults", {
  # Calling wrapper with only n_items / seed must equal the registry call
  # with the same — wrapper must not silently override defaults.
  cfg <- get_model_config("1PL")
  expect_equal(
    irt_params_1pl(n_items = 15, seed = 11),
    cfg$generate_default_params(n_items = 15, seed = 11)
  )
})

test_that("irt_params_1pl supports b_dist = 'even'", {
  out <- irt_params_1pl(
    n_items = 8, b_dist = "even", b_range = c(-3, 3)
  )
  expect_equal(min(out$b), -3)
  expect_equal(max(out$b), 3)
  diffs <- diff(out$b)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
})

test_that("irt_params_1pl reproducibility with seed", {
  a <- irt_params_1pl(n_items = 12, seed = 42)
  b <- irt_params_1pl(n_items = 12, seed = 42)
  expect_equal(a, b)
})

test_that("irt_params_1pl input validation surfaces from registry method", {
  expect_error(irt_params_1pl(n_items = 0), "n_items")
  expect_error(irt_params_1pl(n_items = -1), "n_items")
  expect_error(
    irt_params_1pl(n_items = 5, b_dist = "garbage"),
    "b_dist"
  )
})
