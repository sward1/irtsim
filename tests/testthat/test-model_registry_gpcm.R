# test-model_registry_gpcm.R
# TDD tests for Obj 37 sub-step (e): add GPCM to the model registry.
#
# Templated off test-model_registry_pcm.R per the contract locked in
# Session 27. GPCM = PCM + per-item discrimination, so most of the
# polytomous machinery is reused. The deltas (locked Session 27 NEXT STEP):
#   (i)   Restore a_dist / a_mean / a_sd args; discrimination is generated
#         via generate_discrimination() (lnorm), mirroring 2PL / 3PL / GRM.
#   (ii)  param_schema names a and b (same as PCM; semantics on `a` differ).
#   (iii) validate_params drops the `a == 1` Rasch enforcement and
#         reinstates `a > 0` (same constraint as 2PL / 3PL / GRM).
#   (iv)  convert_to_mirt returns the user-supplied `a` vector
#         (NOT rep(1, n_items)) so mirt::simdata gets per-item
#         discrimination.
#   (v)   mirt_itemtype = "gpcm" — simulation and estimation itemtypes
#         match, so there is NO simulation/estimation split (unlike PCM
#         which used "gpcm" for sim and "Rasch" for fit).
#
# Carried over from PCM (unchanged for GPCM):
#   - Each model config exposes the full 8-method contract:
#       param_schema, response_format, mirt_itemtype,
#       generate_default_params, validate_params,
#       build_true_params, convert_to_mirt, extract_params.
#   - generate_default_params() takes named, model-specific args.
#   - Defaults live in the registry method; the wrapper is a thin delegator.
#   - Seed handling lives in the registry method (set.seed(seed) if non-null).
#   - GPCM is a partial-credit family — `b` steps are NOT sorted within row
#     (same property as PCM). build_true_params and extract_params index
#     steps by column position, not by rank.
#   - RNG call sequence inside generate_default_params(), locked here so
#     future refactors stay bit-identical:
#       1. discrimination:    generate_discrimination(n_items, a_dist, a_mean, a_sd)
#                             (calls rlnorm(n_items, meanlog=a_mean, sdlog=a_sd))
#       2. item centers:      rnorm(n_items, b_mean, b_sd)
#                             (or seq() if b_dist = "even")
#       3. within-item offsets: rnorm(n_items * (n_categories - 1), 0, step_dispersion)
#       4. b = b_centers + offsets matrix (broadcast across columns), UNSORTED
#   - convert_to_mirt() returns d = -a * b (broadcast across columns, just
#     like GRM), itemtype = rep("gpcm", n_items). guess / upper are NOT
#     returned (only relevant for 3PL/4PL).

# --- Field existence ---------------------------------------------------------

test_that("GPCM is registered and retrievable via get_model_config", {
  cfg <- get_model_config("GPCM")
  expect_true(is.list(cfg))
})

test_that("GPCM config exposes the full method contract", {
  cfg <- get_model_config("GPCM")
  required <- c(
    "param_schema", "response_format", "mirt_itemtype",
    "generate_default_params", "validate_params",
    "build_true_params", "convert_to_mirt", "extract_params"
  )
  for (field in required) {
    expect_true(
      field %in% names(cfg),
      info = paste("GPCM config missing field:", field)
    )
  }
  for (fn in c("generate_default_params", "validate_params",
               "build_true_params", "convert_to_mirt", "extract_params")) {
    expect_true(
      is.function(cfg[[fn]]),
      info = paste("GPCM", fn, "is not a function")
    )
  }
})

test_that("GPCM param_schema names a and b", {
  cfg <- get_model_config("GPCM")
  expect_true(all(c("a", "b") %in% names(cfg$param_schema)))
})

test_that("GPCM mirt_itemtype is 'gpcm'", {
  # GPCM's simulation and estimation itemtypes match — no split needed
  # (unlike PCM which used "gpcm" for sim and "Rasch" for fit).
  cfg <- get_model_config("GPCM")
  expect_identical(cfg$mirt_itemtype, "gpcm")
})

test_that("GPCM response_format is 'polytomous'", {
  cfg <- get_model_config("GPCM")
  expect_identical(cfg$response_format, "polytomous")
})


# --- generate_default_params: return shape ----------------------------------

test_that("GPCM generate_default_params returns list(a, b) with correct dims", {
  cfg <- get_model_config("GPCM")
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

test_that("GPCM generate_default_params returns a variable (NOT all 1s)", {
  # Sanity check vs PCM: GPCM's `a` comes from rlnorm with sdlog = 0.25,
  # so the probability of all values landing exactly at 1 is zero.
  cfg <- get_model_config("GPCM")
  out <- cfg$generate_default_params(n_items = 20, n_categories = 5, seed = 1)
  expect_true(all(out$a > 0))
  expect_false(all(out$a == 1))
})


# --- generate_default_params: defaults --------------------------------------

test_that("GPCM default RNG sequence: a (lnorm) -> centers -> offset matrix", {
  # Locks the RNG sequence so future refactors stay bit-identical.
  # Sequence: rlnorm(n_items) for a, rnorm(n_items) for centers,
  # then rnorm(n_items * n_thresholds) for the offset matrix (column-major fill).
  cfg <- get_model_config("GPCM")
  n_items <- 8
  n_categories <- 4
  n_thresholds <- n_categories - 1L

  out <- cfg$generate_default_params(
    n_items = n_items, n_categories = n_categories, seed = 99
  )

  set.seed(99)
  a_expected <- stats::rlnorm(n_items, meanlog = 0, sdlog = 0.25)
  centers_expected <- stats::rnorm(n_items, mean = 0, sd = 1)
  offsets_expected <- matrix(
    stats::rnorm(n_items * n_thresholds, mean = 0, sd = 1),
    nrow = n_items, ncol = n_thresholds
  )
  b_expected <- centers_expected + offsets_expected   # vector recycles down rows

  expect_equal(out$a, a_expected)
  expect_equal(out$b, b_expected)
})

test_that("GPCM generate_default_params is reproducible with seed", {
  cfg <- get_model_config("GPCM")
  a <- cfg$generate_default_params(n_items = 8, n_categories = 4, seed = 42)
  b <- cfg$generate_default_params(n_items = 8, n_categories = 4, seed = 42)
  expect_equal(a, b)
})


# --- generate_default_params: honors a_dist / a_mean / a_sd -----------------

test_that("GPCM honors custom a_mean (shifts lnorm location)", {
  # Larger meanlog -> distributionally larger a values.
  cfg <- get_model_config("GPCM")
  small <- cfg$generate_default_params(
    n_items = 500, n_categories = 4, a_mean = 0,   seed = 100
  )
  large <- cfg$generate_default_params(
    n_items = 500, n_categories = 4, a_mean = 0.5, seed = 100
  )
  expect_gt(mean(large$a), mean(small$a))
})

test_that("GPCM honors custom a_sd (scales lnorm spread)", {
  cfg <- get_model_config("GPCM")
  narrow <- cfg$generate_default_params(
    n_items = 500, n_categories = 4, a_sd = 0.10, seed = 100
  )
  wide <- cfg$generate_default_params(
    n_items = 500, n_categories = 4, a_sd = 0.50, seed = 100
  )
  expect_gt(sd(wide$a), sd(narrow$a))
})

test_that("GPCM custom a_dist/a_mean/a_sd reproduces a hand-rolled draw", {
  cfg <- get_model_config("GPCM")
  n_items <- 10
  n_categories <- 4
  n_thresholds <- n_categories - 1L

  out <- cfg$generate_default_params(
    n_items = n_items, n_categories = n_categories,
    a_dist = "lnorm", a_mean = 0.1, a_sd = 0.30,
    seed = 7
  )

  set.seed(7)
  a_expected <- stats::rlnorm(n_items, meanlog = 0.1, sdlog = 0.30)
  centers_expected <- stats::rnorm(n_items, mean = 0, sd = 1)
  offsets_expected <- matrix(
    stats::rnorm(n_items * n_thresholds, mean = 0, sd = 1),
    nrow = n_items, ncol = n_thresholds
  )
  b_expected <- centers_expected + offsets_expected

  expect_equal(out$a, a_expected)
  expect_equal(out$b, b_expected)
})


# --- generate_default_params: custom step_dispersion ------------------------

test_that("GPCM honors custom step_dispersion (within-item spread scales)", {
  cfg <- get_model_config("GPCM")
  out_narrow <- cfg$generate_default_params(
    n_items = 200, n_categories = 5, step_dispersion = 0.25, seed = 100
  )
  out_wide <- cfg$generate_default_params(
    n_items = 200, n_categories = 5, step_dispersion = 2.0, seed = 100
  )
  within_sd_narrow <- mean(apply(out_narrow$b - rowMeans(out_narrow$b), 1, sd))
  within_sd_wide   <- mean(apply(out_wide$b   - rowMeans(out_wide$b),   1, sd))
  expect_gt(within_sd_wide, within_sd_narrow * 3)   # ~8x in expectation
})

test_that("GPCM custom step_dispersion reproduces a hand-rolled draw", {
  cfg <- get_model_config("GPCM")
  n_items <- 10
  n_categories <- 4
  n_thresholds <- n_categories - 1L
  step_dispersion <- 0.5

  out <- cfg$generate_default_params(
    n_items = n_items, n_categories = n_categories,
    step_dispersion = step_dispersion, seed = 7
  )

  set.seed(7)
  a_expected <- stats::rlnorm(n_items, meanlog = 0, sdlog = 0.25)
  centers_expected <- stats::rnorm(n_items, mean = 0, sd = 1)
  offsets_expected <- matrix(
    stats::rnorm(n_items * n_thresholds, mean = 0, sd = step_dispersion),
    nrow = n_items, ncol = n_thresholds
  )
  b_expected <- centers_expected + offsets_expected

  expect_equal(out$a, a_expected)
  expect_equal(out$b, b_expected)
})


# --- generate_default_params: b_dist = "even" -------------------------------

test_that("GPCM generate_default_params supports b_dist = 'even'", {
  cfg <- get_model_config("GPCM")
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


# --- generate_default_params: GPCM steps are NOT sorted ---------------------

test_that("GPCM step matrix is NOT sorted within row (partial-credit family)", {
  # GPCM inherits PCM's partial-credit step structure: disordered steps are
  # allowed. With step_dispersion = 1, the probability that ALL rows happen
  # to be sorted by chance with n_items = 200, n_thresholds = 4 is
  # vanishingly small (< 1/24^200). This test would only fail if
  # implementation sorted the rows.
  cfg <- get_model_config("GPCM")
  out <- cfg$generate_default_params(
    n_items = 200, n_categories = 5, step_dispersion = 1.0, seed = 13
  )
  row_is_sorted <- apply(out$b, 1, function(r) !is.unsorted(r))
  expect_false(all(row_is_sorted))
})


# --- generate_default_params: validation ------------------------------------

test_that("GPCM generate_default_params rejects non-positive n_items", {
  cfg <- get_model_config("GPCM")
  expect_error(cfg$generate_default_params(n_items = 0,  n_categories = 4), "n_items")
  expect_error(cfg$generate_default_params(n_items = -3, n_categories = 4), "n_items")
})

test_that("GPCM generate_default_params rejects n_categories < 2", {
  cfg <- get_model_config("GPCM")
  expect_error(cfg$generate_default_params(n_items = 5, n_categories = 1), "n_categories")
  expect_error(cfg$generate_default_params(n_items = 5, n_categories = 0), "n_categories")
})

test_that("GPCM generate_default_params rejects unknown a_dist", {
  cfg <- get_model_config("GPCM")
  expect_error(
    cfg$generate_default_params(
      n_items = 5, n_categories = 4, a_dist = "normal"
    ),
    "a_dist"
  )
})

test_that("GPCM generate_default_params rejects unknown b_dist", {
  cfg <- get_model_config("GPCM")
  expect_error(
    cfg$generate_default_params(
      n_items = 5, n_categories = 4, b_dist = "exponential"
    ),
    "b_dist"
  )
})

test_that("GPCM generate_default_params rejects non-positive step_dispersion (but allows 0)", {
  cfg <- get_model_config("GPCM")
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

test_that("GPCM validate_params requires a and b", {
  cfg <- get_model_config("GPCM")
  expect_error(
    cfg$validate_params(list(b = matrix(0, 5, 3)), n_items = 5),
    "`a`"
  )
  expect_error(
    cfg$validate_params(list(a = rep(1, 5)), n_items = 5),
    "`b`"
  )
})

test_that("GPCM validate_params requires b to be a matrix", {
  cfg <- get_model_config("GPCM")
  expect_error(
    cfg$validate_params(
      list(a = c(1.0, 0.8, 1.2, 1.1), b = c(-1, 0, 0.5, 1)),
      n_items = 4
    ),
    "matrix"
  )
})

test_that("GPCM validate_params requires a > 0 (NOT a == 1)", {
  # GPCM is partial-credit family with per-item discrimination: a > 0,
  # not a == 1. A non-unit a vector that PCM would reject must pass GPCM.
  cfg <- get_model_config("GPCM")
  good <- list(a = c(1.0, 0.8, 1.2, 1.1), b = matrix(0, 4, 3))
  expect_silent(cfg$validate_params(good, n_items = 4))

  zero <- list(a = c(1.0, 0, 1.2, 1.1), b = matrix(0, 4, 3))
  expect_error(cfg$validate_params(zero, n_items = 4), "positive|>")

  neg <- list(a = c(1.0, -0.5, 1.2, 1.1), b = matrix(0, 4, 3))
  expect_error(cfg$validate_params(neg, n_items = 4), "positive|>")
})

test_that("GPCM validate_params enforces length / nrow match n_items", {
  cfg <- get_model_config("GPCM")
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

test_that("GPCM build_true_params yields a row + step rows per item, in position order", {
  cfg <- get_model_config("GPCM")
  # User-supplied non-unit a, plus a deliberately disordered b to confirm
  # steps are recorded in column position and a flows through unchanged.
  a <- c(0.9, 1.3, 0.7)
  b <- rbind(
    c( 0.5, -0.5,  1.0),    # disordered
    c(-1.0,  0.2, -0.3),    # disordered
    c(-1.5,  0.0,  1.5)     # ordered (allowed but not required)
  )
  design <- list(
    n_items = 3,
    item_params = list(a = a, b = b)
  )
  tp <- cfg$build_true_params(design)
  expect_s3_class(tp, "data.frame")
  expect_true(all(c("item", "param", "true_value") %in% names(tp)))
  expect_equal(nrow(tp), (1 + 3) * 3)   # a + b1..b3 per item
  expect_setequal(unique(tp$param), c("a", "b1", "b2", "b3"))

  # `a` row reflects the user-supplied vector (NOT rep(1, n_items)).
  # Step values preserve column position — disordered values must NOT be sorted.
  expect_equal(tp$true_value[tp$param == "a"],  a)
  expect_equal(tp$true_value[tp$param == "b1"], b[, 1])
  expect_equal(tp$true_value[tp$param == "b2"], b[, 2])
  expect_equal(tp$true_value[tp$param == "b3"], b[, 3])
})


# --- convert_to_mirt --------------------------------------------------------

test_that("GPCM convert_to_mirt returns user-supplied a, d = -a*b, itemtype = 'gpcm'", {
  cfg <- get_model_config("GPCM")
  a <- c(0.9, 1.3, 0.7)
  b <- rbind(
    c(-0.5, 0.2, 0.8),
    c( 0.0, 1.0, 0.5),       # disordered — must pass through unchanged
    c(-1.0, -0.3, 0.7)
  )
  design <- list(
    n_items = 3,
    n_factors = 1,
    item_params = list(a = a, b = b)
  )
  out <- cfg$convert_to_mirt(design)
  expect_true(all(c("a", "d", "itemtype") %in% names(out)))
  # GPCM passes the user-supplied a through verbatim (NOT rep(1, n_items)).
  expect_equal(out$a, a)
  # GPCM: d_k = -a * b_k. a recycles down rows of b in R's column-major
  # multiplication, giving d[i, k] = -a[i] * b[i, k].
  expect_equal(out$d, -a * b)
  expect_equal(out$itemtype, rep("gpcm", 3))
})

test_that("GPCM convert_to_mirt does not return guess or upper", {
  # GPCM is not a 3PL/4PL family — guess/upper fields are absent.
  cfg <- get_model_config("GPCM")
  design <- list(
    n_items = 2, n_factors = 1,
    item_params = list(
      a = c(0.9, 1.1),
      b = matrix(c(-0.5, 0.5, 0, 1), nrow = 2)
    )
  )
  out <- cfg$convert_to_mirt(design)
  expect_null(out$guess)
  expect_null(out$upper)
})


# --- Wrapper: irt_params_gpcm -----------------------------------------------

test_that("irt_params_gpcm is exported and callable", {
  expect_true(exists("irt_params_gpcm", mode = "function"))
})

test_that("irt_params_gpcm returns list(a, b) with expected shape", {
  out <- irt_params_gpcm(n_items = 10, n_categories = 4, seed = 5)
  expect_named(out, c("a", "b"), ignore.order = TRUE)
  expect_length(out$a, 10)
  expect_true(all(out$a > 0))
  expect_false(all(out$a == 1))    # GPCM's a is variable, not Rasch
  expect_true(is.matrix(out$b))
  expect_equal(dim(out$b), c(10L, 3L))
})

test_that("irt_params_gpcm delegates to the registry method (byte-for-byte)", {
  cfg <- get_model_config("GPCM")
  expected <- cfg$generate_default_params(
    n_items = 25, n_categories = 5,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    step_dispersion = 1.0,
    seed = 2025
  )
  observed <- irt_params_gpcm(
    n_items = 25, n_categories = 5,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    step_dispersion = 1.0,
    seed = 2025
  )
  expect_equal(observed, expected)
})

test_that("irt_params_gpcm default args match registry defaults", {
  # Calling wrapper with only n_items / n_categories / seed must equal the
  # registry call with the same — wrapper must not silently override defaults.
  cfg <- get_model_config("GPCM")
  expect_equal(
    irt_params_gpcm(n_items = 15, n_categories = 4, seed = 11),
    cfg$generate_default_params(n_items = 15, n_categories = 4, seed = 11)
  )
})

test_that("irt_params_gpcm exposes a_dist/a_mean/a_sd to user", {
  # Sanity check that the wrapper actually forwards the discrimination args
  # (vs accidentally swallowing them like PCM's wrapper would).
  out_small <- irt_params_gpcm(
    n_items = 500, n_categories = 4, a_mean = 0,   seed = 100
  )
  out_large <- irt_params_gpcm(
    n_items = 500, n_categories = 4, a_mean = 0.5, seed = 100
  )
  expect_gt(mean(out_large$a), mean(out_small$a))
})

test_that("irt_params_gpcm supports b_dist = 'even'", {
  out <- irt_params_gpcm(
    n_items = 8, n_categories = 4,
    b_dist = "even", b_range = c(-3, 3),
    step_dispersion = 0,
    seed = 1
  )
  centers <- rowMeans(out$b)
  expect_equal(min(centers), -3)
  expect_equal(max(centers), 3)
})

test_that("irt_params_gpcm input validation surfaces from registry method", {
  expect_error(irt_params_gpcm(n_items = 0, n_categories = 4), "n_items")
  expect_error(irt_params_gpcm(n_items = 5, n_categories = 1), "n_categories")
  expect_error(irt_params_gpcm(n_items = 5, n_categories = 4, a_dist = "normal"), "a_dist")
  expect_error(irt_params_gpcm(n_items = 5, n_categories = 4, b_dist = "garbage"), "b_dist")
  expect_error(
    irt_params_gpcm(n_items = 5, n_categories = 4, step_dispersion = -0.5),
    "step_dispersion"
  )
})
