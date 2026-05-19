# test-model_registry_3pl.R
# TDD tests for Obj 37 sub-step (c): add 3PL to the model registry.
#
# Scope: registry entry .model_3pl() + user-facing irt_params_3pl() wrapper.
# Treat this file as the template for PCM/GPCM (sub-steps d, e).
#
# Locked contract (Session 25, carried forward):
#   - Each model config exposes the full set of methods:
#       param_schema, response_format, mirt_itemtype,
#       generate_default_params, validate_params,
#       build_true_params, convert_to_mirt, extract_params.
#   - generate_default_params() takes named, model-specific args (not variadic).
#   - Defaults live in the registry method; the user-facing wrapper is a
#     thin delegator and forwards args unchanged.
#   - Seed handling lives in the registry method (set.seed(seed) if non-null).
#   - Wrappers are byte-for-byte equivalent to the registry method.
#
# 3PL-specific design (this objective):
#   - User-facing parameter name: `c` (lower asymptote / guessing). Internal
#     mirt name is `g`; convert_to_mirt() handles the rename.
#   - c is drawn from Beta(shape1, shape2). Defaults: c_shape1 = 5,
#     c_shape2 = 17 (E[c] ~= 0.227, SD ~= 0.087). Matches typical MC defaults.
#   - convert_to_mirt() contract extends to return `guess` (= c) and
#     `upper` (= 1 by default) so generate_data() can pass them to
#     mirt::simdata. Existing models (1PL/2PL/GRM) return NULL or omit
#     these fields; generate_data() treats absence as "no override".
#   - RNG call sequence inside generate_default_params(): rlnorm (a) ->
#     rnorm (b) -> rbeta (c). Locked here so future refactors are
#     bit-identical.

# --- Field existence ---------------------------------------------------------

test_that("3PL is registered and retrievable via get_model_config", {
  cfg <- get_model_config("3PL")
  expect_true(is.list(cfg))
})

test_that("3PL config exposes the full method contract", {
  cfg <- get_model_config("3PL")
  required <- c(
    "param_schema", "response_format", "mirt_itemtype",
    "generate_default_params", "validate_params",
    "build_true_params", "convert_to_mirt", "extract_params"
  )
  for (field in required) {
    expect_true(
      field %in% names(cfg),
      info = paste("3PL config missing field:", field)
    )
  }
  for (fn in c("generate_default_params", "validate_params",
               "build_true_params", "convert_to_mirt", "extract_params")) {
    expect_true(
      is.function(cfg[[fn]]),
      info = paste("3PL", fn, "is not a function")
    )
  }
})

test_that("3PL param_schema names a, b, c", {
  cfg <- get_model_config("3PL")
  expect_true(all(c("a", "b", "c") %in% names(cfg$param_schema)))
})

test_that("3PL mirt_itemtype is '3PL'", {
  cfg <- get_model_config("3PL")
  expect_identical(cfg$mirt_itemtype, "3PL")
})

test_that("3PL response_format is 'binary'", {
  cfg <- get_model_config("3PL")
  expect_identical(cfg$response_format, "binary")
})


# --- generate_default_params: return shape ----------------------------------

test_that("3PL generate_default_params returns list(a, b, c) of length n_items", {
  cfg <- get_model_config("3PL")
  out <- cfg$generate_default_params(n_items = 12, seed = 1)
  expect_true(is.list(out))
  expect_named(out, c("a", "b", "c"), ignore.order = TRUE)
  expect_length(out$a, 12)
  expect_length(out$b, 12)
  expect_length(out$c, 12)
  expect_type(out$a, "double")
  expect_type(out$b, "double")
  expect_type(out$c, "double")
})

test_that("3PL generate_default_params produces a > 0 and c in [0, 1)", {
  cfg <- get_model_config("3PL")
  out <- cfg$generate_default_params(n_items = 50, seed = 2)
  expect_true(all(out$a > 0))
  expect_true(all(out$c >= 0))
  expect_true(all(out$c < 1))
})


# --- generate_default_params: defaults --------------------------------------

test_that("3PL default c distribution is Beta(5, 17)", {
  # Direct sampling check: with seed fixed, the c vector from the registry
  # method must match a hand-rolled Beta(5, 17) draw with the same RNG
  # position (i.e., AFTER the a- and b-draws that precede it).
  cfg <- get_model_config("3PL")
  out <- cfg$generate_default_params(n_items = 30, seed = 99)

  # Reconstruct the same RNG sequence: a (rlnorm) -> b (rnorm) -> c (rbeta).
  set.seed(99)
  a_expected <- stats::rlnorm(30, meanlog = 0, sdlog = 0.25)
  b_expected <- stats::rnorm(30, mean = 0, sd = 1)
  c_expected <- stats::rbeta(30, shape1 = 5, shape2 = 17)

  expect_equal(out$a, a_expected)
  expect_equal(out$b, b_expected)
  expect_equal(out$c, c_expected)
})

test_that("3PL generate_default_params is reproducible with seed", {
  cfg <- get_model_config("3PL")
  a <- cfg$generate_default_params(n_items = 8, seed = 42)
  b <- cfg$generate_default_params(n_items = 8, seed = 42)
  expect_equal(a, b)
})


# --- generate_default_params: custom Beta parameters ------------------------

test_that("3PL generate_default_params honors custom c_shape1 / c_shape2", {
  cfg <- get_model_config("3PL")
  # High shape1 relative to shape2 -> mean closer to 1.
  out_hi <- cfg$generate_default_params(
    n_items = 500, c_shape1 = 17, c_shape2 = 5, seed = 100
  )
  # Inverted -> mean closer to 0 (the default direction).
  out_lo <- cfg$generate_default_params(
    n_items = 500, c_shape1 = 5, c_shape2 = 17, seed = 100
  )
  # Sanity: the two distributions should sit on opposite sides of 0.5.
  expect_gt(mean(out_hi$c), 0.5)
  expect_lt(mean(out_lo$c), 0.5)
})

test_that("3PL custom Beta args reproduce a hand-rolled draw", {
  cfg <- get_model_config("3PL")
  out <- cfg$generate_default_params(
    n_items = 20,
    c_shape1 = 3, c_shape2 = 9,
    seed = 7
  )
  set.seed(7)
  stats::rlnorm(20, meanlog = 0, sdlog = 0.25)
  stats::rnorm(20, mean = 0, sd = 1)
  c_expected <- stats::rbeta(20, shape1 = 3, shape2 = 9)
  expect_equal(out$c, c_expected)
})


# --- generate_default_params: b_dist = "even" -------------------------------

test_that("3PL generate_default_params supports b_dist = 'even'", {
  cfg <- get_model_config("3PL")
  out <- cfg$generate_default_params(
    n_items = 10, b_dist = "even", b_range = c(-2, 2), seed = 1
  )
  expect_length(out$b, 10)
  expect_equal(min(out$b), -2)
  expect_equal(max(out$b), 2)
  diffs <- diff(out$b)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
})


# --- generate_default_params: validation ------------------------------------

test_that("3PL generate_default_params rejects non-positive n_items", {
  cfg <- get_model_config("3PL")
  expect_error(cfg$generate_default_params(n_items = 0), "n_items")
  expect_error(cfg$generate_default_params(n_items = -3), "n_items")
})

test_that("3PL generate_default_params rejects unknown b_dist", {
  cfg <- get_model_config("3PL")
  expect_error(
    cfg$generate_default_params(n_items = 5, b_dist = "exponential"),
    "b_dist"
  )
})

test_that("3PL generate_default_params rejects non-positive Beta shapes", {
  cfg <- get_model_config("3PL")
  expect_error(
    cfg$generate_default_params(n_items = 5, c_shape1 = 0, seed = 1),
    "c_shape1"
  )
  expect_error(
    cfg$generate_default_params(n_items = 5, c_shape2 = -1, seed = 1),
    "c_shape2"
  )
})


# --- validate_params --------------------------------------------------------

test_that("3PL validate_params requires a, b, c", {
  cfg <- get_model_config("3PL")
  expect_error(
    cfg$validate_params(list(b = rep(0, 5), c = rep(0.2, 5)), n_items = 5),
    "`a`"
  )
  expect_error(
    cfg$validate_params(list(a = rep(1, 5), c = rep(0.2, 5)), n_items = 5),
    "`b`"
  )
  expect_error(
    cfg$validate_params(list(a = rep(1, 5), b = rep(0, 5)), n_items = 5),
    "`c`"
  )
})

test_that("3PL validate_params enforces c in [0, 1)", {
  cfg <- get_model_config("3PL")
  good <- list(a = rep(1, 4), b = rep(0, 4), c = c(0.0, 0.1, 0.25, 0.5))
  expect_silent(cfg$validate_params(good, n_items = 4))

  bad_low <- list(a = rep(1, 3), b = rep(0, 3), c = c(0.2, -0.01, 0.3))
  expect_error(cfg$validate_params(bad_low, n_items = 3), "c")

  bad_high <- list(a = rep(1, 3), b = rep(0, 3), c = c(0.2, 1.0, 0.3))
  expect_error(cfg$validate_params(bad_high, n_items = 3), "c")
})

test_that("3PL validate_params enforces a > 0 and length matches n_items", {
  cfg <- get_model_config("3PL")
  expect_error(
    cfg$validate_params(
      list(a = c(1, -0.5, 1), b = c(0, 0, 0), c = c(0.2, 0.2, 0.2)),
      n_items = 3
    ),
    "discrimination|positive|`a`"
  )
  expect_error(
    cfg$validate_params(
      list(a = rep(1, 4), b = rep(0, 5), c = rep(0.2, 5)),
      n_items = 5
    ),
    "n_items|length"
  )
  expect_error(
    cfg$validate_params(
      list(a = rep(1, 5), b = rep(0, 5), c = rep(0.2, 4)),
      n_items = 5
    ),
    "n_items|length"
  )
})


# --- build_true_params ------------------------------------------------------

test_that("3PL build_true_params yields a, b, c rows per item", {
  cfg <- get_model_config("3PL")
  design <- list(
    n_items = 4,
    item_params = list(
      a = c(1.0, 1.2, 0.9, 1.1),
      b = c(-1, 0, 0.5, 1.5),
      c = c(0.20, 0.22, 0.18, 0.25)
    )
  )
  tp <- cfg$build_true_params(design)
  expect_s3_class(tp, "data.frame")
  expect_true(all(c("item", "param", "true_value") %in% names(tp)))
  expect_equal(nrow(tp), 3 * 4) # a, b, c per item
  expect_setequal(unique(tp$param), c("a", "b", "c"))

  # Per-param values line up with the design
  expect_equal(tp$true_value[tp$param == "a"], design$item_params$a)
  expect_equal(tp$true_value[tp$param == "b"], design$item_params$b)
  expect_equal(tp$true_value[tp$param == "c"], design$item_params$c)
})


# --- convert_to_mirt --------------------------------------------------------

test_that("3PL convert_to_mirt returns a, d, guess, upper, and itemtype = '3PL'", {
  cfg <- get_model_config("3PL")
  design <- list(
    n_items = 3,
    n_factors = 1,
    item_params = list(
      a = c(1.0, 1.5, 0.8),
      b = c(-0.5, 0.0, 1.0),
      c = c(0.20, 0.25, 0.15)
    )
  )
  out <- cfg$convert_to_mirt(design)
  expect_true(all(c("a", "d", "guess", "upper", "itemtype") %in% names(out)))
  expect_equal(out$itemtype, rep("3PL", 3))
  # d = -a * b
  expect_equal(out$d, -design$item_params$a * design$item_params$b)
  # guess = c at the mirt boundary; upper fixed at 1
  expect_equal(out$guess, design$item_params$c)
  expect_equal(out$upper, rep(1, 3))
})


# --- Wrapper: irt_params_3pl -------------------------------------------------

test_that("irt_params_3pl is exported and callable", {
  expect_true(exists("irt_params_3pl", mode = "function"))
})

test_that("irt_params_3pl returns list(a, b, c) of length n_items", {
  out <- irt_params_3pl(n_items = 10, seed = 5)
  expect_named(out, c("a", "b", "c"), ignore.order = TRUE)
  expect_length(out$a, 10)
  expect_length(out$b, 10)
  expect_length(out$c, 10)
  expect_true(all(out$a > 0))
  expect_true(all(out$c >= 0 & out$c < 1))
})

test_that("irt_params_3pl delegates to the registry method (byte-for-byte)", {
  cfg <- get_model_config("3PL")
  expected <- cfg$generate_default_params(
    n_items = 25,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    c_shape1 = 5, c_shape2 = 17,
    seed = 2025
  )
  observed <- irt_params_3pl(
    n_items = 25,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    c_shape1 = 5, c_shape2 = 17,
    seed = 2025
  )
  expect_equal(observed, expected)
})

test_that("irt_params_3pl default args match registry defaults", {
  # Calling wrapper with only n_items + seed should equal the registry call
  # with only n_items + seed (i.e., wrapper does not silently override any
  # default).
  cfg <- get_model_config("3PL")
  expect_equal(
    irt_params_3pl(n_items = 15, seed = 11),
    cfg$generate_default_params(n_items = 15, seed = 11)
  )
})

test_that("irt_params_3pl supports b_dist = 'even'", {
  out <- irt_params_3pl(
    n_items = 8, b_dist = "even", b_range = c(-3, 3), seed = 1
  )
  expect_equal(min(out$b), -3)
  expect_equal(max(out$b), 3)
})

test_that("irt_params_3pl input validation surfaces from registry method", {
  expect_error(irt_params_3pl(n_items = 0), "n_items")
  expect_error(irt_params_3pl(n_items = 5, b_dist = "garbage"), "b_dist")
  expect_error(irt_params_3pl(n_items = 5, c_shape1 = 0), "c_shape1")
})
