# test-model_registry_generate_default_params.R
# TDD tests for Obj 37 sub-step (a): registry-level generate_default_params()
# method on each existing model config (1PL, 2PL, GRM).
#
# Contract (locked Session 25):
#   - Each model config carries a generate_default_params field that is a function.
#   - Named args per model; defaults co-located with the model config.
#   - Seed handling lives INSIDE the registry method (set.seed(seed) if non-null).
#   - Returns a named list shaped like the model's item_params slot:
#       1PL : list(b = numeric(n_items))
#       2PL : list(a = numeric(n_items), b = numeric(n_items))
#       GRM : list(a = numeric(n_items), b = matrix(n_items x n_thresholds))
#   - RNG call sequence matches the pre-refactor irt_params_2pl() /
#     irt_params_grm() exactly, so seeded output is bit-identical across the
#     refactor (this is the safety contract for sub-step (b)).

# --- Field existence ---------------------------------------------------------

test_that("all three existing model configs expose generate_default_params", {
  for (m in c("1PL", "2PL", "GRM")) {
    cfg <- get_model_config(m)
    expect_true(
      "generate_default_params" %in% names(cfg),
      info = paste(m, "missing generate_default_params field")
    )
    expect_true(
      is.function(cfg$generate_default_params),
      info = paste(m, "generate_default_params is not a function")
    )
  }
})

# --- 1PL ---------------------------------------------------------------------

test_that("1PL generate_default_params returns list(b = numeric(n_items))", {
  cfg <- get_model_config("1PL")
  out <- cfg$generate_default_params(n_items = 12, seed = 1)
  expect_true(is.list(out))
  expect_named(out, "b")
  expect_length(out$b, 12)
  expect_type(out$b, "double")
})

test_that("1PL generate_default_params is reproducible with seed", {
  cfg <- get_model_config("1PL")
  a <- cfg$generate_default_params(n_items = 8, seed = 42)
  b <- cfg$generate_default_params(n_items = 8, seed = 42)
  expect_equal(a, b)
})

test_that("1PL generate_default_params supports b_dist = 'even'", {
  cfg <- get_model_config("1PL")
  out <- cfg$generate_default_params(
    n_items = 10, b_dist = "even", b_range = c(-2, 2)
  )
  expect_length(out$b, 10)
  expect_equal(min(out$b), -2)
  expect_equal(max(out$b), 2)
  diffs <- diff(out$b)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
})

# --- 2PL ---------------------------------------------------------------------

test_that("2PL generate_default_params returns list(a, b) of length n_items", {
  cfg <- get_model_config("2PL")
  out <- cfg$generate_default_params(n_items = 15, seed = 7)
  expect_true(is.list(out))
  expect_named(out, c("a", "b"), ignore.order = TRUE)
  expect_length(out$a, 15)
  expect_length(out$b, 15)
  expect_true(all(out$a > 0))
})

test_that("2PL generate_default_params reproduces the legacy wrapper byte-for-byte", {
  # The refactor must preserve the exact RNG call sequence so that any seeded
  # downstream user of irt_params_2pl() gets identical output post-refactor.
  cfg <- get_model_config("2PL")

  expected <- irt_params_2pl(
    n_items = 25,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    seed = 2025
  )
  observed <- cfg$generate_default_params(
    n_items = 25,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_dist = "normal", b_mean = 0, b_sd = 1,
    seed = 2025
  )
  expect_equal(observed, expected)
})

test_that("2PL generate_default_params supports b_dist = 'even'", {
  cfg <- get_model_config("2PL")
  out <- cfg$generate_default_params(
    n_items = 8, b_dist = "even", b_range = c(-3, 3), seed = 1
  )
  expect_equal(min(out$b), -3)
  expect_equal(max(out$b), 3)
})

# --- GRM ---------------------------------------------------------------------

test_that("GRM generate_default_params returns list(a, b matrix) of correct shape", {
  cfg <- get_model_config("GRM")
  out <- cfg$generate_default_params(n_items = 10, n_categories = 5, seed = 3)
  expect_true(is.list(out))
  expect_named(out, c("a", "b"), ignore.order = TRUE)
  expect_length(out$a, 10)
  expect_true(is.matrix(out$b))
  expect_equal(nrow(out$b), 10)
  expect_equal(ncol(out$b), 4)
  expect_true(all(out$a > 0))
})

test_that("GRM generate_default_params produces ordered thresholds per item", {
  cfg <- get_model_config("GRM")
  out <- cfg$generate_default_params(n_items = 12, n_categories = 5, seed = 9)
  for (i in seq_len(nrow(out$b))) {
    expect_true(
      all(diff(out$b[i, ]) > 0),
      info = paste("Item", i, "thresholds not ordered")
    )
  }
})

test_that("GRM generate_default_params handles n_categories = 2", {
  cfg <- get_model_config("GRM")
  out <- cfg$generate_default_params(n_items = 6, n_categories = 2, seed = 4)
  expect_equal(ncol(out$b), 1)
})

test_that("GRM generate_default_params reproduces the legacy wrapper byte-for-byte", {
  cfg <- get_model_config("GRM")

  expected <- irt_params_grm(
    n_items = 8, n_categories = 4,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_mean = 0, b_sd = 1,
    seed = 2025
  )
  observed <- cfg$generate_default_params(
    n_items = 8, n_categories = 4,
    a_dist = "lnorm", a_mean = 0, a_sd = 0.25,
    b_mean = 0, b_sd = 1,
    seed = 2025
  )
  expect_equal(observed, expected)
})

# --- Input validation --------------------------------------------------------

test_that("generate_default_params rejects non-positive n_items in all models", {
  for (m in c("1PL", "2PL", "GRM")) {
    cfg <- get_model_config(m)
    # GRM additionally requires n_categories
    extra <- if (m == "GRM") list(n_categories = 4) else list()
    expect_error(
      do.call(cfg$generate_default_params, c(list(n_items = 0), extra)),
      "n_items",
      info = paste(m, "did not reject n_items = 0")
    )
    expect_error(
      do.call(cfg$generate_default_params, c(list(n_items = -1), extra)),
      "n_items",
      info = paste(m, "did not reject n_items = -1")
    )
  }
})

test_that("GRM generate_default_params rejects n_categories < 2", {
  cfg <- get_model_config("GRM")
  expect_error(
    cfg$generate_default_params(n_items = 5, n_categories = 1),
    "categor"
  )
})
