# test-irt_params_helpers.R
# TDD tests for irt_params_2pl() and irt_params_grm() — Objective 2
# These helper functions generate common item parameter configurations.

# --- irt_params_2pl() --------------------------------------------------------

test_that("irt_params_2pl returns a list with a and b vectors", {
  params <- irt_params_2pl(n_items = 20)
  expect_true(is.list(params))
  expect_named(params, c("a", "b"), ignore.order = TRUE)
  expect_length(params$a, 20)
  expect_length(params$b, 20)
})

test_that("irt_params_2pl generates positive discrimination values", {
  params <- irt_params_2pl(n_items = 50)
  expect_true(all(params$a > 0))
})

test_that("irt_params_2pl respects a_dist arguments", {
  set.seed(42)
  params <- irt_params_2pl(
    n_items = 100,
    a_dist = "lnorm",
    a_mean = 0,
    a_sd = 0.25
  )
  # Log of a should be approximately N(0, 0.25)
  log_a <- log(params$a)
  expect_true(abs(mean(log_a)) < 0.15)  # rough check with 100 items
  expect_true(all(params$a > 0))
})

test_that("irt_params_2pl respects b_dist arguments", {
  set.seed(42)
  params <- irt_params_2pl(
    n_items = 100,
    b_dist = "normal",
    b_mean = 0,
    b_sd = 1
  )
  # Difficulty should be approximately N(0, 1)
  expect_true(abs(mean(params$b)) < 0.3)
})

test_that("irt_params_2pl accepts evenly-spaced b option", {
  params <- irt_params_2pl(
    n_items = 10,
    b_dist = "even",
    b_range = c(-2, 2)
  )
  expect_length(params$b, 10)
  expect_equal(min(params$b), -2)
  expect_equal(max(params$b), 2)
  # Should be evenly spaced
  diffs <- diff(params$b)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
})

test_that("irt_params_2pl is reproducible with seed", {
  p1 <- irt_params_2pl(n_items = 10, seed = 123)
  p2 <- irt_params_2pl(n_items = 10, seed = 123)
  expect_equal(p1, p2)
})

test_that("irt_params_2pl rejects non-positive n_items", {
  expect_error(irt_params_2pl(n_items = 0), "n_items")
  expect_error(irt_params_2pl(n_items = -1), "n_items")
})

# --- irt_params_grm() --------------------------------------------------------

test_that("irt_params_grm returns a list with a vector and b matrix", {
  params <- irt_params_grm(n_items = 10, n_categories = 5)
  expect_true(is.list(params))
  expect_named(params, c("a", "b"), ignore.order = TRUE)
  expect_length(params$a, 10)
  expect_true(is.matrix(params$b))
  expect_equal(nrow(params$b), 10)
  expect_equal(ncol(params$b), 4)  # n_categories - 1 thresholds
})

test_that("irt_params_grm generates ordered thresholds within each item", {
  params <- irt_params_grm(n_items = 20, n_categories = 5)
  # For each item (row), thresholds should be monotonically increasing
  for (i in seq_len(nrow(params$b))) {
    expect_true(all(diff(params$b[i, ]) > 0),
                info = paste("Item", i, "thresholds not ordered"))
  }
})

test_that("irt_params_grm generates positive discrimination", {
  params <- irt_params_grm(n_items = 20, n_categories = 4)
  expect_true(all(params$a > 0))
})

test_that("irt_params_grm handles binary case (2 categories)", {
  params <- irt_params_grm(n_items = 10, n_categories = 2)
  expect_equal(ncol(params$b), 1)  # 2 categories → 1 threshold
})

test_that("irt_params_grm is reproducible with seed", {
  p1 <- irt_params_grm(n_items = 10, n_categories = 4, seed = 42)
  p2 <- irt_params_grm(n_items = 10, n_categories = 4, seed = 42)
  expect_equal(p1, p2)
})

test_that("irt_params_grm rejects n_categories < 2", {
  expect_error(
    irt_params_grm(n_items = 10, n_categories = 1),
    "categor"
  )
})

test_that("irt_params_grm rejects non-positive n_items", {
  expect_error(irt_params_grm(n_items = 0, n_categories = 5), "n_items")
})
