# test-model_registry_pcm.R
# TDD tests for Obj 37 sub-step (d): add PCM to the model registry.
#
# Templated off test-model_registry_3pl.R per the contract locked in
# Session 26. Treat this file as the template for GPCM (sub-step e).
#
# Locked contract (Session 25–26, carried forward):
#   - Each model config exposes the full set of methods:
#       param_schema, response_format, mirt_itemtype,
#       generate_default_params, validate_params,
#       build_true_params, convert_to_mirt, extract_params.
#   - generate_default_params() takes named, model-specific args.
#   - Defaults live in the registry method; the user-facing wrapper is a
#     thin delegator and forwards args unchanged.
#   - Seed handling lives in the registry method (set.seed(seed) if non-null).
#   - Wrapper is byte-for-byte equivalent to the registry method.
#
# PCM-specific design (this objective):
#   - PCM is Rasch-family polytomous: a = 1 fixed per item.
#     generate_default_params still returns a = rep(1, n_items) so the
#     item_params list shape is consistent with GRM / GPCM.
#   - b is an n_items x (n_categories - 1) matrix of step parameters.
#   - PCM steps are NOT sorted within row. This is the defining contrast
#     vs. GRM (which sorts within row). build_true_params and
#     extract_params therefore index steps by position, not by rank.
#   - User-facing arg `step_dispersion` controls within-item step spread.
#     Default step_dispersion = 1.0. Anchored to Embretson & Reise (2000)
#     PCM examples and mirt::simdata polytomous conventions. Schroeders &
#     Gnambs (2025) does NOT contain a PCM example, so this default is
#     literature-traced, not paper-traced.
#   - RNG call sequence inside generate_default_params(), locked here so
#     future refactors are bit-identical:
#       1. item centers: rnorm(n_items, b_mean, b_sd)   (or seq() if b_dist = "even")
#       2. within-item step offsets: rnorm(n_items * (n_categories - 1), 0, step_dispersion)
#       3. b = b_centers + offsets matrix (broadcast across columns), UNSORTED
#   - convert_to_mirt() returns a = rep(1, n_items), d = -b (matrix),
#     itemtype = rep("gpcm", n_items). mirt::simdata accepts itemtype
#     "gpcm" with a vector of 1s as the canonical PCM simulation
#     mechanism. The top-level mirt_itemtype = "Rasch" is used by
#     fit_model() for canonical PCM estimation (a constrained = 1, free
#     latent variance). The simulation/estimation split mirrors GRM's
#     "graded" vs "2PL" handling of the single-threshold edge case.

# --- Field existence ---------------------------------------------------------

test_that("PCM is registered and retrievable via get_model_config", {
  cfg <- get_model_config("PCM")
  expect_true(is.list(cfg))
})

test_that("PCM config exposes the full method contract", {
  cfg <- get_model_config("PCM")
  required <- c(
    "param_schema", "response_format", "mirt_itemtype",
    "generate_default_params", "validate_params",
    "build_true_params", "convert_to_mirt", "extract_params"
  )
  for (field in required) {
    expect_true(
      field %in% names(cfg),
      info = paste("PCM config missing field:", field)
    )
  }
  for (fn in c("generate_default_params", "validate_params",
               "build_true_params", "convert_to_mirt", "extract_params")) {
    expect_true(
      is.function(cfg[[fn]]),
      info = paste("PCM", fn, "is not a function")
    )
  }
})

test_that("PCM param_schema names a and b", {
  cfg <- get_model_config("PCM")
  expect_true(all(c("a", "b") %in% names(cfg$param_schema)))
})

test_that("PCM mirt_itemtype is 'Rasch'", {
  # top-level mirt_itemtype drives fit_model() — PCM is fit as Rasch
  # (a constrained to 1, free latent variance) in mirt.
  cfg <- get_model_config("PCM")
  expect_identical(cfg$mirt_itemtype, "Rasch")
})

test_that("PCM response_format is 'polytomous'", {
  cfg <- get_model_config("PCM")
  expect_identical(cfg$response_format, "polytomous")
})


# --- generate_default_params: return shape ----------------------------------

test_that("PCM generate_default_params returns list(a, b) with correct dims", {
  cfg <- get_model_config("PCM")
  out <- cfg$generate_default_params(
    n_items = 12, n_categories = 4, seed = 1
  )
  expect_true(is.list(out))
  expect_named(out, c("a", "b"), ignore.order = TRUE)
  expect_length(out$a, 12)
  expect_true(is.matrix(out$b))
  expect_equal(dim(out$b), c(12L, 3L))   # n_thresholds = n_categories - 1
  expect_type(out$a, "double")
  expect_type(out$b, "double")
})

test_that("PCM generate_default_params returns a = rep(1, n_items)", {
  cfg <- get_model_config("PCM")
  out <- cfg$generate_default_params(n_items = 20, n_categories = 5, seed = 1)
  expect_equal(out$a, rep(1, 20))
})


# --- generate_default_params: defaults --------------------------------------

test_that("PCM default RNG sequence: centers then offset matrix", {
  # Locks the RNG sequence so future refactors stay bit-identical.
  # Sequence: rnorm(n_items) for centers, then rnorm(n_items * n_thresholds)
  # for the offset matrix (column-major fill).
  cfg <- get_model_config("PCM")
  n_items <- 8
  n_categories <- 4
  n_thresholds <- n_categories - 1L

  out <- cfg$generate_default_params(
    n_items = n_items, n_categories = n_categories, seed = 99
  )

  set.seed(99)
  centers_expected <- stats::rnorm(n_items, mean = 0, sd = 1)
  offsets_expected <- matrix(
    stats::rnorm(n_items * n_thresholds, mean = 0, sd = 1),
    nrow = n_items, ncol = n_thresholds
  )
  b_expected <- centers_expected + offsets_expected   # vector recycles down rows

  expect_equal(out$b, b_expected)
})

test_that("PCM generate_default_params is reproducible with seed", {
  cfg <- get_model_config("PCM")
  a <- cfg$generate_default_params(n_items = 8, n_categories = 4, seed = 42)
  b <- cfg$generate_default_params(n_items = 8, n_categories = 4, seed = 42)
  expect_equal(a, b)
})


# --- generate_default_params: custom step_dispersion ------------------------

test_that("PCM honors custom step_dispersion (within-item spread scales)", {
  cfg <- get_model_config("PCM")
  out_narrow <- cfg$generate_default_params(
    n_items = 200, n_categories = 5, step_dispersion = 0.25, seed = 100
  )
  out_wide <- cfg$generate_default_params(
    n_items = 200, n_categories = 5, step_dispersion = 2.0, seed = 100
  )
  # Within-item SD of step offsets should track step_dispersion.
  # Center each row on its mean to isolate within-item spread from item-center spread.
  within_sd_narrow <- mean(apply(out_narrow$b - rowMeans(out_narrow$b), 1, sd))
  within_sd_wide   <- mean(apply(out_wide$b   - rowMeans(out_wide$b),   1, sd))
  expect_gt(within_sd_wide, within_sd_narrow * 3)   # ~8x in expectation; 3x is a robust floor
})

test_that("PCM custom step_dispersion reproduces a hand-rolled draw", {
  cfg <- get_model_config("PCM")
  n_items <- 10
  n_categories <- 4
  n_thresholds <- n_categories - 1L
  step_dispersion <- 0.5

  out <- cfg$generate_default_params(
    n_items = n_items, n_categories = n_categories,
    step_dispersion = step_dispersion, seed = 7
  )

  set.seed(7)
  centers_expected <- stats::rnorm(n_items, mean = 0, sd = 1)
  offsets_expected <- matrix(
    stats::rnorm(n_items * n_thresholds, mean = 0, sd = step_dispersion),
    nrow = n_items, ncol = n_thresholds
  )
  b_expected <- centers_expected + offsets_expected

  expect_equal(out$b, b_expected)
})


# --- generate_default_params: b_dist = "even" -------------------------------

test_that("PCM generate_default_params supports b_dist = 'even'", {
  cfg <- get_model_config("PCM")
  out <- cfg$generate_default_params(
    n_items = 10, n_categories = 4,
    b_dist = "even", b_range = c(-2, 2),
    step_dispersion = 0,    # zero spread -> rows equal their center
    seed = 1
  )
  # With step_dispersion = 0, every step in row i equals the item center.
  # Item centers should be evenly spaced across b_range.
  centers <- rowMeans(out$b)
  expect_equal(min(centers), -2)
  expect_equal(max(centers), 2)
  diffs <- diff(centers)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
})


# --- generate_default_params: PCM steps are NOT sorted ----------------------

test_that("PCM step matrix is NOT sorted within row (model property)", {
  # PCM allows disordered steps. With step_dispersion = 1, the probability
  # that ALL rows happen to be sorted by chance with n_items = 200,
  # n_thresholds = 4 is vanishingly small (< 1/24^200). This test would
  # only fail if implementation sorted the rows.
  cfg <- get_model_config("PCM")
  out <- cfg$generate_default_params(
    n_items = 200, n_categories = 5, step_dispersion = 1.0, seed = 13
  )
  row_is_sorted <- apply(out$b, 1, function(r) !is.unsorted(r))
  expect_false(all(row_is_sorted))
})


# --- generate_default_params: validation ------------------------------------

test_that("PCM generate_default_params rejects non-positive n_items", {
  cfg <- get_model_config("PCM")
  expect_error(cfg$generate_default_params(n_items = 0, n_categories = 4), "n_items")
  expect_error(cfg$generate_default_params(n_items = -3, n_categories = 4), "n_items")
})

test_that("PCM generate_default_params rejects n_categories < 2", {
  cfg <- get_model_config("PCM")
  expect_error(cfg$generate_default_params(n_items = 5, n_categories = 1), "n_categories")
  expect_error(cfg$generate_default_params(n_items = 5, n_categories = 0), "n_categories")
})

test_that("PCM generate_default_params rejects unknown b_dist", {
  cfg <- get_model_config("PCM")
  expect_error(
    cfg$generate_default_params(n_items = 5, n_categories = 4, b_dist = "exponential"),
    "b_dist"
  )
})

test_that("PCM generate_default_params rejects non-positive step_dispersion (but allows 0)", {
  # Zero dispersion is allowed (degenerate but valid: all steps equal the item center).
  # Negative dispersion is invalid (sd cannot be negative).
  cfg <- get_model_config("PCM")
  expect_silent(
    cfg$generate_default_params(
      n_items = 5, n_categories = 4, step_dispersion = 0, seed = 1
    )
  )
  expect_error(
    cfg$generate_default_params(
      n_items = 5, n_categories = 4, step_dispersion = -0.1, seed = 1
    ),
    "step_dispersion"
  )
})


# --- validate_params --------------------------------------------------------

test_that("PCM validate_params requires a and b", {
  cfg <- get_model_config("PCM")
  expect_error(
    cfg$validate_params(list(b = matrix(0, 5, 3)), n_items = 5),
    "`a`"
  )
  expect_error(
    cfg$validate_params(list(a = rep(1, 5)), n_items = 5),
    "`b`"
  )
})

test_that("PCM validate_params requires b to be a matrix", {
  cfg <- get_model_config("PCM")
  expect_error(
    cfg$validate_params(
      list(a = rep(1, 4), b = c(-1, 0, 0.5, 1)),
      n_items = 4
    ),
    "matrix"
  )
})

test_that("PCM validate_params enforces a == 1 (Rasch family)", {
  cfg <- get_model_config("PCM")
  good <- list(a = rep(1, 4), b = matrix(0, 4, 3))
  expect_silent(cfg$validate_params(good, n_items = 4))

  bad <- list(a = c(1, 1.5, 1, 1), b = matrix(0, 4, 3))
  expect_error(cfg$validate_params(bad, n_items = 4), "a|Rasch|1")
})

test_that("PCM validate_params enforces length / nrow match n_items", {
  cfg <- get_model_config("PCM")
  expect_error(
    cfg$validate_params(
      list(a = rep(1, 3), b = matrix(0, 5, 3)),
      n_items = 5
    ),
    "n_items|length"
  )
  expect_error(
    cfg$validate_params(
      list(a = rep(1, 5), b = matrix(0, 4, 3)),
      n_items = 5
    ),
    "n_items|nrow"
  )
})


# --- build_true_params ------------------------------------------------------

test_that("PCM build_true_params yields a row + step rows per item, in position order", {
  cfg <- get_model_config("PCM")
  # Deliberately disordered b to confirm steps are recorded in column position,
  # NOT in sorted rank.
  b <- rbind(
    c( 0.5, -0.5,  1.0),    # disordered
    c(-1.0,  0.2, -0.3),    # disordered
    c(-1.5,  0.0,  1.5)     # ordered (allowed but not required)
  )
  design <- list(
    n_items = 3,
    item_params = list(a = rep(1, 3), b = b)
  )
  tp <- cfg$build_true_params(design)
  expect_s3_class(tp, "data.frame")
  expect_true(all(c("item", "param", "true_value") %in% names(tp)))
  expect_equal(nrow(tp), (1 + 3) * 3)   # a + b1..b3 per item
  expect_setequal(unique(tp$param), c("a", "b1", "b2", "b3"))

  # Step values preserve column position — disordered values must NOT be sorted.
  expect_equal(tp$true_value[tp$param == "a"],  rep(1, 3))
  expect_equal(tp$true_value[tp$param == "b1"], b[, 1])
  expect_equal(tp$true_value[tp$param == "b2"], b[, 2])
  expect_equal(tp$true_value[tp$param == "b3"], b[, 3])
})


# --- convert_to_mirt --------------------------------------------------------

test_that("PCM convert_to_mirt returns a = 1, d = -b, itemtype = 'gpcm'", {
  cfg <- get_model_config("PCM")
  b <- rbind(
    c(-0.5, 0.2, 0.8),
    c( 0.0, 1.0, 0.5),       # disordered — must pass through unchanged
    c(-1.0, -0.3, 0.7)
  )
  design <- list(
    n_items = 3,
    n_factors = 1,
    item_params = list(a = rep(1, 3), b = b)
  )
  out <- cfg$convert_to_mirt(design)
  expect_true(all(c("a", "d", "itemtype") %in% names(out)))
  expect_equal(out$a, rep(1, 3))
  expect_equal(out$d, -b)        # PCM: d_k = -a * b_k, a = 1
  expect_equal(out$itemtype, rep("gpcm", 3))
})

test_that("PCM convert_to_mirt does not return guess or upper", {
  # PCM is not a 3PL/4PL family — guess/upper fields are absent.
  cfg <- get_model_config("PCM")
  design <- list(
    n_items = 2, n_factors = 1,
    item_params = list(a = rep(1, 2), b = matrix(c(-0.5, 0.5, 0, 1), nrow = 2))
  )
  out <- cfg$convert_to_mirt(design)
  expect_null(out$guess)
  expect_null(out$upper)
})


# --- Wrapper: irt_params_pcm ------------------------------------------------

test_that("irt_params_pcm is exported and callable", {
  expect_true(exists("irt_params_pcm", mode = "function"))
})

test_that("irt_params_pcm returns list(a, b) with expected shape", {
  out <- irt_params_pcm(n_items = 10, n_categories = 4, seed = 5)
  expect_named(out, c("a", "b"), ignore.order = TRUE)
  expect_length(out$a, 10)
  expect_equal(out$a, rep(1, 10))
  expect_true(is.matrix(out$b))
  expect_equal(dim(out$b), c(10L, 3L))
})

test_that("irt_params_pcm delegates to the registry method (byte-for-byte)", {
  cfg <- get_model_config("PCM")
  expected <- cfg$generate_default_params(
    n_items = 25, n_categories = 5,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    step_dispersion = 1.0,
    seed = 2025
  )
  observed <- irt_params_pcm(
    n_items = 25, n_categories = 5,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    step_dispersion = 1.0,
    seed = 2025
  )
  expect_equal(observed, expected)
})

test_that("irt_params_pcm default args match registry defaults", {
  # Calling wrapper with only n_items / n_categories / seed must equal the
  # registry call with the same — wrapper must not silently override defaults.
  cfg <- get_model_config("PCM")
  expect_equal(
    irt_params_pcm(n_items = 15, n_categories = 4, seed = 11),
    cfg$generate_default_params(n_items = 15, n_categories = 4, seed = 11)
  )
})

test_that("irt_params_pcm supports b_dist = 'even'", {
  out <- irt_params_pcm(
    n_items = 8, n_categories = 4,
    b_dist = "even", b_range = c(-3, 3),
    step_dispersion = 0,    # zero spread -> rows equal centers
    seed = 1
  )
  centers <- rowMeans(out$b)
  expect_equal(min(centers), -3)
  expect_equal(max(centers), 3)
})

test_that("irt_params_pcm input validation surfaces from registry method", {
  expect_error(irt_params_pcm(n_items = 0, n_categories = 4), "n_items")
  expect_error(irt_params_pcm(n_items = 5, n_categories = 1), "n_categories")
  expect_error(irt_params_pcm(n_items = 5, n_categories = 4, b_dist = "garbage"), "b_dist")
  expect_error(
    irt_params_pcm(n_items = 5, n_categories = 4, step_dispersion = -0.5),
    "step_dispersion"
  )
})
