# test-generate_data.R
# TDD tests for generate_data() — Objective 6 (Part 1)
# These tests define the expected behavior BEFORE implementation exists.
#
# generate_data() wraps mirt::simdata to produce response matrices
# from an irt_design object. Key responsibilities:
#   1. Translate b (IRT difficulty) to d (mirt slope-intercept)
#   2. Generate theta from the specified distribution
#   3. Return a matrix of correct dimensions with valid response values
#   4. Respect seeds for reproducibility


# --- Helper: reusable designs ------------------------------------------------

make_1pl_design <- function(n_items = 10) {
  irt_design(
    model = "1PL",
    n_items = n_items,
    item_params = list(b = seq(-2, 2, length.out = n_items))
  )
}

make_2pl_design <- function(n_items = 15) {
  irt_design(
    model = "2PL",
    n_items = n_items,
    item_params = list(
      a = rep(1.2, n_items),
      b = seq(-2, 2, length.out = n_items)
    )
  )
}

make_grm_design <- function(n_items = 10, n_categories = 4) {
  n_thresh <- n_categories - 1L
  b_mat <- matrix(
    seq(-2, 2, length.out = n_items * n_thresh),
    nrow = n_items, ncol = n_thresh
  )
  # Ensure thresholds are ordered within each row
  # (skip t(apply()) for single-threshold case — same fix as irt_params_grm)
  if (n_thresh > 1L) {
    b_mat <- t(apply(b_mat, 1, sort))
  }
  irt_design(
    model = "GRM",
    n_items = n_items,
    item_params = list(a = rep(1.0, n_items), b = b_mat)
  )
}

# =============================================================================
# 1. Output Dimensions
# =============================================================================

test_that("generate_data returns matrix with N rows and n_items columns (1PL)", {
  design <- make_1pl_design(10)
  dat <- irtsim:::generate_data(design, n = 200, seed = 1)

  expect_true(is.matrix(dat))
  expect_equal(nrow(dat), 200)
  expect_equal(ncol(dat), 10)
})

test_that("generate_data returns matrix with N rows and n_items columns (2PL)", {
  design <- make_2pl_design(15)
  dat <- irtsim:::generate_data(design, n = 300, seed = 1)

  expect_true(is.matrix(dat))
  expect_equal(nrow(dat), 300)
  expect_equal(ncol(dat), 15)
})

test_that("generate_data returns matrix with N rows and n_items columns (GRM)", {
  design <- make_grm_design(10, n_categories = 4)
  dat <- irtsim:::generate_data(design, n = 250, seed = 1)

  expect_true(is.matrix(dat))
  expect_equal(nrow(dat), 250)
  expect_equal(ncol(dat), 10)
})

# =============================================================================
# 2. Valid Response Values
# =============================================================================

test_that("generate_data produces binary (0/1) responses for 1PL", {
  design <- make_1pl_design(10)
  dat <- irtsim:::generate_data(design, n = 500, seed = 42)

  unique_vals <- sort(unique(as.vector(dat)))
  expect_true(all(unique_vals %in% c(0L, 1L)))
})

test_that("generate_data produces binary (0/1) responses for 2PL", {
  design <- make_2pl_design(15)
  dat <- irtsim:::generate_data(design, n = 500, seed = 42)

  unique_vals <- sort(unique(as.vector(dat)))
  expect_true(all(unique_vals %in% c(0L, 1L)))
})

test_that("generate_data produces ordinal responses for GRM in valid range", {
  n_categories <- 5
  design <- make_grm_design(10, n_categories = n_categories)
  dat <- irtsim:::generate_data(design, n = 500, seed = 42)

  unique_vals <- sort(unique(as.vector(dat)))
  # mirt::simdata returns 0-based categories for GRM: 0, 1, ..., n_categories - 1
  expect_true(all(unique_vals >= 0))
  expect_true(all(unique_vals <= n_categories - 1))
  # With 500 respondents and 10 items, we should see most categories used
  expect_true(length(unique_vals) >= 3)
})

test_that("generate_data produces no NA values in complete data", {
  design <- make_2pl_design(15)
  dat <- irtsim:::generate_data(design, n = 200, seed = 1)

  expect_false(anyNA(dat))
})

# =============================================================================
# 3. b-to-d Parameterization Translation
# =============================================================================
#
# mirt uses slope-intercept parameterization: d = -a * b (for dichotomous)
# or d_k = -a * b_k (for GRM thresholds). generate_data() must handle
# this translation internally so the user specifies difficulty (b) but
# mirt::simdata receives intercepts (d).

test_that("generate_data correctly translates b to d for 1PL", {
  # With a = 1 and b = 0 (50% probability at theta = 0), responses

  # should be approximately 50% correct for examinees at theta = 0.
  # Use many easy items (b << 0) with theta = 0 to verify directionality.
  design <- irt_design(
    model = "1PL",
    n_items = 20,
    item_params = list(b = rep(-3, 20))
  )
  # Force theta near 0 via custom function
  dat <- irtsim:::generate_data(
    design,
    n = 1000,
    seed = 99,
    theta = rep(0, 1000)
  )
  # Items with b = -3 should be very easy for theta = 0 examinees

  # Mean proportion correct should be high (> 0.9)
  mean_p <- mean(dat)
  expect_gt(mean_p, 0.85)
})

test_that("generate_data correctly translates b to d for 2PL", {
  # Hard items (b = 3) should produce mostly 0s for theta = 0 examinees
  design <- irt_design(
    model = "2PL",
    n_items = 20,
    item_params = list(a = rep(1.5, 20), b = rep(3, 20))
  )
  dat <- irtsim:::generate_data(
    design,
    n = 1000,
    seed = 99,
    theta = rep(0, 1000)
  )
  # Mean proportion correct should be low (< 0.15)
  mean_p <- mean(dat)
  expect_lt(mean_p, 0.15)
})

# =============================================================================
# 4. Theta Distribution
# =============================================================================

test_that("generate_data uses standard normal theta when theta_dist = 'normal'", {
  design <- make_1pl_design(10)
  # Generate data with known seed and extract theta (if returned)
  # Primary test: function runs without error with "normal" theta_dist
  dat <- irtsim:::generate_data(design, n = 5000, seed = 1)

  # With standard normal theta and symmetric difficulty, mean proportion
  # correct should be near 0.5
  mean_p <- mean(dat)
  expect_gt(mean_p, 0.35)
  expect_lt(mean_p, 0.65)
})

test_that("generate_data uses uniform theta when theta_dist = 'uniform'", {
  design <- irt_design(
    model = "1PL",
    n_items = 10,
    item_params = list(b = seq(-2, 2, length.out = 10)),
    theta_dist = "uniform"
  )
  dat <- irtsim:::generate_data(design, n = 5000, seed = 1)

  # Uniform theta should also produce a reasonable range of responses
  expect_false(anyNA(dat))
  expect_equal(nrow(dat), 5000)
})

test_that("generate_data uses custom theta function", {
  # Custom function: all theta = 2 (high ability)
  custom_theta <- function(n) rep(2, n)
  design <- irt_design(
    model = "1PL",
    n_items = 10,
    item_params = list(b = seq(-2, 2, length.out = 10)),
    theta_dist = custom_theta
  )
  dat <- irtsim:::generate_data(design, n = 500, seed = 1)

  # High-ability examinees should get most items correct
  mean_p <- mean(dat)
  expect_gt(mean_p, 0.7)
})

# =============================================================================
# 5. Seed Reproducibility
# =============================================================================

test_that("generate_data is reproducible with the same seed", {
  design <- make_2pl_design(15)
  dat1 <- irtsim:::generate_data(design, n = 100, seed = 42)
  dat2 <- irtsim:::generate_data(design, n = 100, seed = 42)

  expect_identical(dat1, dat2)
})

test_that("generate_data produces different data with different seeds", {
  design <- make_2pl_design(15)
  dat1 <- irtsim:::generate_data(design, n = 100, seed = 1)
  dat2 <- irtsim:::generate_data(design, n = 100, seed = 2)

  # Very unlikely to be identical with different seeds
  expect_false(identical(dat1, dat2))
})

# =============================================================================
# 6. Theta Pass-Through
# =============================================================================

test_that("generate_data accepts a pre-generated theta vector", {
  design <- make_1pl_design(10)
  theta <- rnorm(200)
  dat <- irtsim:::generate_data(design, n = 200, seed = 1, theta = theta)

  expect_equal(nrow(dat), 200)
  expect_equal(ncol(dat), 10)
})

test_that("generate_data errors when theta length doesn't match n", {
  design <- make_1pl_design(10)
  theta <- rnorm(50) # Wrong length for n = 200

  expect_error(
    irtsim:::generate_data(design, n = 200, seed = 1, theta = theta),
    "theta"
  )
})

# =============================================================================
# 7. GRM-Specific Behavior
# =============================================================================

test_that("generate_data handles GRM with 2 categories (binary GRM)", {
  # Edge case: GRM with only 2 categories is functionally binary
  design <- make_grm_design(10, n_categories = 2)
  dat <- irtsim:::generate_data(design, n = 200, seed = 1)

  expect_true(is.matrix(dat))
  expect_equal(nrow(dat), 200)
  expect_equal(ncol(dat), 10)
  unique_vals <- sort(unique(as.vector(dat)))
  expect_true(all(unique_vals %in% c(0, 1)))
})

test_that("generate_data handles GRM with many categories", {
  design <- make_grm_design(8, n_categories = 7)
  dat <- irtsim:::generate_data(design, n = 1000, seed = 1)

  expect_true(is.matrix(dat))
  unique_vals <- sort(unique(as.vector(dat)))
  expect_true(all(unique_vals >= 0))
  expect_true(all(unique_vals <= 6))
})

# =============================================================================
# 8. Edge Cases
# =============================================================================

test_that("generate_data works with n = 1", {
  design <- make_1pl_design(10)
  dat <- irtsim:::generate_data(design, n = 1, seed = 1)

  expect_true(is.matrix(dat))
  expect_equal(nrow(dat), 1)
  expect_equal(ncol(dat), 10)
})

test_that("generate_data works with 1 item", {
  design <- irt_design(
    model = "1PL",
    n_items = 1,
    item_params = list(b = 0)
  )
  dat <- irtsim:::generate_data(design, n = 100, seed = 1)

  expect_true(is.matrix(dat))
  expect_equal(nrow(dat), 100)
  expect_equal(ncol(dat), 1)
})
