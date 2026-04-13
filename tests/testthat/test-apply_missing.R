# test-apply_missing.R
# TDD tests for apply_missing() — Objective 6 (Part 2)
# These tests define the expected behavior BEFORE implementation exists.
#
# apply_missing() takes a complete response matrix and an irt_study object,
# then introduces missingness according to the study's missing data mechanism.
# Supported mechanisms: none, mcar, mar, booklet, linking.


# --- Helper: create a complete response matrix --------------------------------

make_complete_data <- function(n = 200, n_items = 10, seed = 1) {
  set.seed(seed)
  matrix(sample(0:1, n * n_items, replace = TRUE), nrow = n, ncol = n_items)
}

make_study <- function(missing = "none", missing_rate = NULL,
                       test_design = NULL, n_items = 10) {
  design <- irt_design(
    model = "1PL",
    n_items = n_items,
    item_params = list(b = seq(-2, 2, length.out = n_items))
  )
  irt_study(
    design,
    sample_sizes = c(100, 200),
    missing = missing,
    missing_rate = missing_rate,
    test_design = test_design
  )
}

# =============================================================================
# 1. Missing = "none" — No Modification
# =============================================================================

test_that("apply_missing returns identical data when missing = 'none'", {
  dat <- make_complete_data(n = 100, n_items = 10)
  study <- make_study(missing = "none")
  result <- irtsim:::apply_missing(dat, study)

  expect_identical(result, dat)
})

test_that("apply_missing introduces no NAs when missing = 'none'", {
  dat <- make_complete_data(n = 100, n_items = 10)
  study <- make_study(missing = "none")
  result <- irtsim:::apply_missing(dat, study)

  expect_false(anyNA(result))
})

# =============================================================================
# 2. MCAR — Missing Completely At Random
# =============================================================================

test_that("apply_missing produces NAs under MCAR", {
  dat <- make_complete_data(n = 500, n_items = 20)
  study <- make_study(missing = "mcar", missing_rate = 0.3, n_items = 20)
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  expect_true(anyNA(result))
})

test_that("apply_missing MCAR rate is approximately correct", {
  dat <- make_complete_data(n = 2000, n_items = 20)
  study <- make_study(missing = "mcar", missing_rate = 0.2, n_items = 20)
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  observed_rate <- mean(is.na(result))
  # With 40,000 cells and 20% missing, stochastic tolerance is tight

  expect_gt(observed_rate, 0.18)
  expect_lt(observed_rate, 0.22)
})

test_that("apply_missing MCAR preserves matrix dimensions", {
  dat <- make_complete_data(n = 100, n_items = 15)
  study <- make_study(missing = "mcar", missing_rate = 0.25, n_items = 15)
  result <- irtsim:::apply_missing(dat, study, seed = 1)

  expect_equal(dim(result), dim(dat))
})

test_that("apply_missing MCAR preserves non-missing values", {
  dat <- make_complete_data(n = 200, n_items = 10)
  study <- make_study(missing = "mcar", missing_rate = 0.3)
  result <- irtsim:::apply_missing(dat, study, seed = 1)

  # Where result is not NA, values should match original
  non_na_mask <- !is.na(result)
  expect_equal(result[non_na_mask], dat[non_na_mask])
})

test_that("apply_missing MCAR is reproducible with same seed", {
  dat <- make_complete_data(n = 200, n_items = 10)
  study <- make_study(missing = "mcar", missing_rate = 0.3)

  result1 <- irtsim:::apply_missing(dat, study, seed = 42)
  result2 <- irtsim:::apply_missing(dat, study, seed = 42)

  # NA positions should be identical
  expect_identical(is.na(result1), is.na(result2))
})

test_that("apply_missing MCAR with rate = 0 introduces no NAs", {
  dat <- make_complete_data(n = 100, n_items = 10)
  study <- make_study(missing = "mcar", missing_rate = 0)
  result <- irtsim:::apply_missing(dat, study, seed = 1)

  expect_false(anyNA(result))
})

test_that("apply_missing MCAR spreads NAs across rows and columns", {
  dat <- make_complete_data(n = 500, n_items = 20)
  study <- make_study(missing = "mcar", missing_rate = 0.3, n_items = 20)
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  # With 30% missing across 500 rows and 20 columns, most rows/cols
  # should have at least one NA
  rows_with_na <- sum(apply(result, 1, anyNA))
  cols_with_na <- sum(apply(result, 2, anyNA))
  expect_gt(rows_with_na, 400)
  expect_equal(cols_with_na, 20)
})

# =============================================================================
# 3. MAR — Missing At Random (dependent on theta)
# =============================================================================

test_that("apply_missing produces NAs under MAR", {
  dat <- make_complete_data(n = 500, n_items = 10)
  study <- make_study(missing = "mar", missing_rate = 0.2)
  theta <- rnorm(500)
  result <- irtsim:::apply_missing(dat, study, seed = 42, theta = theta)

  expect_true(anyNA(result))
})

test_that("apply_missing MAR rate is approximately correct overall", {
  dat <- make_complete_data(n = 2000, n_items = 20)
  study <- make_study(missing = "mar", missing_rate = 0.2, n_items = 20)
  theta <- rnorm(2000)
  result <- irtsim:::apply_missing(dat, study, seed = 42, theta = theta)

  observed_rate <- mean(is.na(result))
  # MAR should target approximately the specified rate overall
  expect_gt(observed_rate, 0.15)
  expect_lt(observed_rate, 0.25)
})

test_that("apply_missing MAR missingness depends on theta", {
  dat <- make_complete_data(n = 2000, n_items = 20)
  study <- make_study(missing = "mar", missing_rate = 0.3, n_items = 20)

  # Create theta with extreme groups
  theta <- c(rep(-3, 1000), rep(3, 1000))
  result <- irtsim:::apply_missing(dat, study, seed = 42, theta = theta)

  # Missing rate should differ between low-theta and high-theta groups
  low_miss_rate <- mean(is.na(result[1:1000, ]))
  high_miss_rate <- mean(is.na(result[1001:2000, ]))

  expect_false(abs(low_miss_rate - high_miss_rate) < 0.01)
})

test_that("apply_missing MAR preserves matrix dimensions", {
  dat <- make_complete_data(n = 100, n_items = 10)
  study <- make_study(missing = "mar", missing_rate = 0.2)
  theta <- rnorm(100)
  result <- irtsim:::apply_missing(dat, study, seed = 1, theta = theta)

  expect_equal(dim(result), dim(dat))
})

test_that("apply_missing MAR errors without theta", {
  dat <- make_complete_data(n = 100, n_items = 10)
  study <- make_study(missing = "mar", missing_rate = 0.2)

  expect_error(
    irtsim:::apply_missing(dat, study, seed = 1),
    "theta"
  )
})

# =============================================================================
# 4. Booklet Design — Structured Missingness
# =============================================================================

test_that("apply_missing booklet produces structured missingness", {
  n_items <- 10
  # 2 booklets: booklet 1 administers items 1-6, booklet 2 administers items 5-10
  booklet_matrix <- matrix(0, nrow = 2, ncol = n_items)
  booklet_matrix[1, 1:6] <- 1
  booklet_matrix[2, 5:10] <- 1

  dat <- make_complete_data(n = 200, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  expect_true(anyNA(result))
})

test_that("apply_missing booklet assigns each respondent to exactly one booklet", {
  n_items <- 10
  booklet_matrix <- matrix(0, nrow = 3, ncol = n_items)
  booklet_matrix[1, 1:4] <- 1
  booklet_matrix[2, 3:7] <- 1
  booklet_matrix[3, 6:10] <- 1

  dat <- make_complete_data(n = 300, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  # Each row's NA pattern should match one of the booklet patterns
  for (i in seq_len(nrow(result))) {
    observed_pattern <- as.integer(!is.na(result[i, ]))
    matches_booklet <- apply(booklet_matrix, 1, function(bk) {
      identical(as.integer(bk), observed_pattern)
    })
    expect_true(any(matches_booklet),
                info = paste("Row", i, "does not match any booklet pattern"))
  }
})

test_that("apply_missing booklet distributes respondents roughly evenly", {
  n_items <- 10
  booklet_matrix <- matrix(0, nrow = 2, ncol = n_items)
  booklet_matrix[1, 1:6] <- 1
  booklet_matrix[2, 5:10] <- 1

  dat <- make_complete_data(n = 1000, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  # Count respondents per booklet
  pattern1 <- as.integer(booklet_matrix[1, ] == 1)
  count_bk1 <- sum(apply(result, 1, function(row) {
    identical(as.integer(!is.na(row)), pattern1)
  }))

  # With 2 booklets and 1000 respondents, each should get ~500
  expect_gt(count_bk1, 400)
  expect_lt(count_bk1, 600)
})

test_that("apply_missing booklet preserves observed values", {
  n_items <- 10
  booklet_matrix <- matrix(0, nrow = 2, ncol = n_items)
  booklet_matrix[1, 1:5] <- 1
  booklet_matrix[2, 6:10] <- 1

  dat <- make_complete_data(n = 200, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  # Non-NA values should match original data
  non_na_mask <- !is.na(result)
  expect_equal(result[non_na_mask], dat[non_na_mask])
})

test_that("apply_missing booklet preserves matrix dimensions", {
  n_items <- 10
  booklet_matrix <- matrix(0, nrow = 2, ncol = n_items)
  booklet_matrix[1, 1:6] <- 1
  booklet_matrix[2, 5:10] <- 1

  dat <- make_complete_data(n = 100, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  expect_equal(dim(result), dim(dat))
})

# =============================================================================
# 5. Linking Design — Structured Missingness for Test Equating
# =============================================================================

test_that("apply_missing linking produces structured missingness", {
  n_items <- 12
  # 2 forms: form 1 has items 1-8, form 2 has items 5-12 (items 5-8 are linking)
  linking_matrix <- matrix(0, nrow = 2, ncol = n_items)
  linking_matrix[1, 1:8] <- 1
  linking_matrix[2, 5:12] <- 1

  dat <- make_complete_data(n = 200, n_items = n_items)
  study <- make_study(
    missing = "linking",
    test_design = list(linking_matrix = linking_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  expect_true(anyNA(result))
})

test_that("apply_missing linking assigns each respondent to exactly one form", {
  n_items <- 12
  linking_matrix <- matrix(0, nrow = 3, ncol = n_items)
  linking_matrix[1, 1:6] <- 1
  linking_matrix[2, 4:9] <- 1
  linking_matrix[3, 7:12] <- 1

  dat <- make_complete_data(n = 300, n_items = n_items)
  study <- make_study(
    missing = "linking",
    test_design = list(linking_matrix = linking_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  for (i in seq_len(nrow(result))) {
    observed_pattern <- as.integer(!is.na(result[i, ]))
    matches_form <- apply(linking_matrix, 1, function(fm) {
      identical(as.integer(fm), observed_pattern)
    })
    expect_true(any(matches_form),
                info = paste("Row", i, "does not match any form pattern"))
  }
})

test_that("apply_missing linking preserves observed values", {
  n_items <- 12
  linking_matrix <- matrix(0, nrow = 2, ncol = n_items)
  linking_matrix[1, 1:8] <- 1
  linking_matrix[2, 5:12] <- 1

  dat <- make_complete_data(n = 200, n_items = n_items)
  study <- make_study(
    missing = "linking",
    test_design = list(linking_matrix = linking_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  non_na_mask <- !is.na(result)
  expect_equal(result[non_na_mask], dat[non_na_mask])
})

test_that("apply_missing linking distributes respondents roughly evenly", {
  n_items <- 12
  linking_matrix <- matrix(0, nrow = 2, ncol = n_items)
  linking_matrix[1, 1:8] <- 1
  linking_matrix[2, 5:12] <- 1

  dat <- make_complete_data(n = 1000, n_items = n_items)
  study <- make_study(
    missing = "linking",
    test_design = list(linking_matrix = linking_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  pattern1 <- as.integer(linking_matrix[1, ] == 1)
  count_f1 <- sum(apply(result, 1, function(row) {
    identical(as.integer(!is.na(row)), pattern1)
  }))

  expect_gt(count_f1, 400)
  expect_lt(count_f1, 600)
})

# =============================================================================
# 6. Seed Reproducibility
# =============================================================================

test_that("apply_missing MCAR is reproducible with same seed", {
  dat <- make_complete_data(n = 200, n_items = 10)
  study <- make_study(missing = "mcar", missing_rate = 0.3)

  r1 <- irtsim:::apply_missing(dat, study, seed = 99)
  r2 <- irtsim:::apply_missing(dat, study, seed = 99)

  expect_identical(is.na(r1), is.na(r2))
})

test_that("apply_missing booklet is reproducible with same seed", {
  n_items <- 10
  booklet_matrix <- matrix(0, nrow = 2, ncol = n_items)
  booklet_matrix[1, 1:5] <- 1
  booklet_matrix[2, 6:10] <- 1

  dat <- make_complete_data(n = 200, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )

  r1 <- irtsim:::apply_missing(dat, study, seed = 99)
  r2 <- irtsim:::apply_missing(dat, study, seed = 99)

  expect_identical(is.na(r1), is.na(r2))
})

# =============================================================================
# 7. Edge Cases
# =============================================================================

test_that("apply_missing handles single-row data", {
  dat <- make_complete_data(n = 1, n_items = 10)
  study <- make_study(missing = "mcar", missing_rate = 0.5)
  result <- irtsim:::apply_missing(dat, study, seed = 1)

  expect_equal(dim(result), c(1, 10))
})

test_that("apply_missing handles single-column data", {
  dat <- make_complete_data(n = 100, n_items = 1)
  study <- make_study(missing = "mcar", missing_rate = 0.3, n_items = 1)
  result <- irtsim:::apply_missing(dat, study, seed = 1)

  expect_equal(dim(result), c(100, 1))
})

test_that("apply_missing booklet with one booklet administers all items", {
  n_items <- 10
  # Single booklet that includes all items — should produce no NAs
  booklet_matrix <- matrix(1, nrow = 1, ncol = n_items)

  dat <- make_complete_data(n = 100, n_items = n_items)
  study <- make_study(
    missing = "booklet",
    test_design = list(booklet_matrix = booklet_matrix),
    n_items = n_items
  )
  result <- irtsim:::apply_missing(dat, study, seed = 42)

  expect_false(anyNA(result))
})
