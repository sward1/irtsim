# test-irt_design.R
# TDD tests for irt_design() — Objective 2
# These tests define the expected behavior BEFORE implementation exists.

# --- 1PL (Rasch) Construction ------------------------------------------------

test_that("irt_design creates valid 1PL design with explicit difficulty", {
  b <- seq(-2, 2, length.out = 10)
  design <- irt_design(model = "1PL", n_items = 10, item_params = list(b = b))

  expect_s3_class(design, "irt_design")
  expect_equal(design$model, "1PL")
  expect_equal(design$n_items, 10L)
  expect_equal(design$item_params$b, b)
  # 1PL has no discrimination vector — all items share a = 1
  expect_equal(design$item_params$a, rep(1, 10))
  expect_equal(design$n_factors, 1L)
})

test_that("irt_design 1PL defaults to standard normal theta", {

  design <- irt_design(model = "1PL", n_items = 5, item_params = list(b = rnorm(5)))
  expect_equal(design$theta_dist, "normal")
})

# --- 2PL Construction --------------------------------------------------------

test_that("irt_design creates valid 2PL design", {
  a <- rlnorm(20, meanlog = 0, sdlog = 0.25)
  b <- seq(-2, 2, length.out = 20)
  design <- irt_design(
    model = "2PL",
    n_items = 20,
    item_params = list(a = a, b = b)
  )

  expect_s3_class(design, "irt_design")
  expect_equal(design$model, "2PL")
  expect_equal(design$n_items, 20L)
  expect_equal(design$item_params$a, a)
  expect_equal(design$item_params$b, b)
})

test_that("irt_design 2PL requires both a and b parameters", {
  expect_error(
    irt_design(model = "2PL", n_items = 10, item_params = list(b = rnorm(10))),
    "a"
  )
  expect_error(
    irt_design(model = "2PL", n_items = 10, item_params = list(a = rlnorm(10))),
    "b"
  )
})

# --- GRM Construction --------------------------------------------------------

test_that("irt_design creates valid GRM design", {
  n_items <- 10
  n_categories <- 5  # 5 categories → 4 thresholds per item
  a <- rlnorm(n_items, meanlog = 0, sdlog = 0.25)
  # b is a matrix: n_items x (n_categories - 1)
  b <- matrix(
    sort(rnorm(n_items * (n_categories - 1))),
    nrow = n_items, ncol = n_categories - 1
  )

  design <- irt_design(
    model = "GRM",
    n_items = n_items,
    item_params = list(a = a, b = b)
  )

  expect_s3_class(design, "irt_design")
  expect_equal(design$model, "GRM")
  expect_equal(design$n_items, n_items)
  expect_equal(design$item_params$a, a)
  expect_true(is.matrix(design$item_params$b))
  expect_equal(nrow(design$item_params$b), n_items)
  expect_equal(ncol(design$item_params$b), n_categories - 1)
})

test_that("irt_design GRM requires matrix b with correct dimensions", {
  a <- rlnorm(5, 0, 0.25)
  # Wrong: b as vector instead of matrix

  expect_error(
    irt_design(model = "GRM", n_items = 5, item_params = list(a = a, b = rnorm(5))),
    "matrix"
  )
})

# --- Theta Distribution Options ---------------------------------------------

test_that("irt_design accepts character theta_dist options", {
  b <- rnorm(10)
  design_normal <- irt_design(
    model = "1PL", n_items = 10,
    item_params = list(b = b), theta_dist = "normal"
  )
  expect_equal(design_normal$theta_dist, "normal")

  design_uniform <- irt_design(
    model = "1PL", n_items = 10,
    item_params = list(b = b), theta_dist = "uniform"
  )
  expect_equal(design_uniform$theta_dist, "uniform")
})

test_that("irt_design accepts custom theta_dist function", {
  custom_fn <- function(n) rnorm(n, mean = 1, sd = 0.5)
  design <- irt_design(
    model = "1PL", n_items = 10,
    item_params = list(b = rnorm(10)),
    theta_dist = custom_fn
  )
  expect_true(is.function(design$theta_dist))
})

test_that("irt_design rejects invalid theta_dist strings", {
  expect_error(
    irt_design(
      model = "1PL", n_items = 5,
      item_params = list(b = rnorm(5)),
      theta_dist = "banana"
    ),
    "theta_dist"
  )
})

# --- n_factors ---------------------------------------------------------------

test_that("irt_design defaults to 1 factor", {
  design <- irt_design(model = "1PL", n_items = 5, item_params = list(b = rnorm(5)))
  expect_equal(design$n_factors, 1L)
})

test_that("irt_design accepts n_factors argument", {
  # For multidimensional, a would be a matrix (n_items x n_factors)
  a <- matrix(rlnorm(20), nrow = 10, ncol = 2)
  b <- rnorm(10)
  design <- irt_design(
    model = "2PL", n_items = 10,
    item_params = list(a = a, b = b),
    n_factors = 2
  )
  expect_equal(design$n_factors, 2L)
})

# --- Parameter Validation (Errors) ------------------------------------------

test_that("irt_design rejects unsupported model types", {
  expect_error(
    irt_design(model = "4PL", n_items = 10, item_params = list(b = rnorm(10))),
    "model"
  )
  expect_error(
    irt_design(model = "GPCM", n_items = 10, item_params = list(b = rnorm(10))),
    "model"
  )
})

test_that("irt_design rejects non-positive n_items", {
  expect_error(
    irt_design(model = "1PL", n_items = 0, item_params = list(b = numeric(0))),
    "n_items"
  )
  expect_error(
    irt_design(model = "1PL", n_items = -5, item_params = list(b = rnorm(5))),
    "n_items"
  )
})

test_that("irt_design rejects mismatched n_items and parameter lengths", {
  expect_error(
    irt_design(model = "1PL", n_items = 10, item_params = list(b = rnorm(5))),
    "n_items|length|mismatch"
  )
  expect_error(
    irt_design(model = "2PL", n_items = 10,
               item_params = list(a = rlnorm(10), b = rnorm(5))),
    "n_items|length|mismatch"
  )
})

test_that("irt_design rejects non-positive discrimination values", {
  expect_error(
    irt_design(model = "2PL", n_items = 5,
               item_params = list(a = c(-1, 1, 1, 1, 1), b = rnorm(5))),
    "discrimination|positive|a "
  )
})

test_that("irt_design rejects non-numeric item parameters", {
  expect_error(
    irt_design(model = "1PL", n_items = 3,
               item_params = list(b = c("a", "b", "c"))),
    "numeric"
  )
})

test_that("irt_design rejects item_params that is not a list", {
  expect_error(
    irt_design(model = "1PL", n_items = 5, item_params = rnorm(5)),
    "list"
  )
})

test_that("irt_design rejects NA values in item parameters", {
  expect_error(
    irt_design(model = "1PL", n_items = 5,
               item_params = list(b = c(1, 2, NA, 4, 5))),
    "NA|missing"
  )
})

# --- Edge Cases --------------------------------------------------------------

test_that("irt_design handles single-item design", {
  design <- irt_design(model = "1PL", n_items = 1, item_params = list(b = 0))
  expect_s3_class(design, "irt_design")
  expect_equal(design$n_items, 1L)
})

test_that("irt_design handles large item count", {
  design <- irt_design(model = "1PL", n_items = 100,
                       item_params = list(b = rnorm(100)))
  expect_s3_class(design, "irt_design")
  expect_equal(design$n_items, 100L)
})

# --- S3 Class Structure ------------------------------------------------------

test_that("irt_design returns an object with correct class", {
  design <- irt_design(model = "1PL", n_items = 5, item_params = list(b = rnorm(5)))
  expect_s3_class(design, "irt_design")
  expect_true(is.list(design))
})

test_that("irt_design object contains all expected elements", {
  design <- irt_design(model = "2PL", n_items = 10,
                       item_params = list(a = rlnorm(10), b = rnorm(10)))
  expected_names <- c("model", "n_items", "item_params", "theta_dist", "n_factors")
  expect_true(all(expected_names %in% names(design)))
})

# --- Print Method ------------------------------------------------------------

test_that("print.irt_design produces informative output", {
  design <- irt_design(model = "2PL", n_items = 20,
                       item_params = list(a = rlnorm(20), b = rnorm(20)))
  output <- capture.output(print(design))
  output_text <- paste(output, collapse = "\n")

  # Should mention the model type
  expect_match(output_text, "2PL", fixed = TRUE)
  # Should mention the number of items
  expect_match(output_text, "20")
  # Should mention theta distribution
  expect_match(output_text, "normal|theta", ignore.case = TRUE)
  # Should return the object invisibly
  expect_invisible(print(design))
})

test_that("print.irt_design works for GRM", {
  a <- rlnorm(5, 0, 0.25)
  b <- matrix(sort(rnorm(20)), nrow = 5, ncol = 4)
  design <- irt_design(model = "GRM", n_items = 5, item_params = list(a = a, b = b))
  output <- capture.output(print(design))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "GRM", fixed = TRUE)
  # Should indicate polytomous / categories info
  expect_match(output_text, "5 items|categor", ignore.case = TRUE)
})

# --- Immutability Design Intent ----------------------------------------------

test_that("irt_design object is a plain list (not reference class)", {
  design <- irt_design(model = "1PL", n_items = 5, item_params = list(b = rnorm(5)))
  # S3 objects are plain lists; confirm no R6/R5 reference semantics
  expect_false(is.environment(design))
  expect_true(is.list(design))
})
