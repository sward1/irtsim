# test-irt_study.R
# TDD tests for irt_study() — Objective 4
# These tests define the expected behavior BEFORE implementation exists.

# --- Shared Fixture -----------------------------------------------------------

make_design_2pl <- function(n = 10) {
  irt_design(
    model = "2PL",
    n_items = n,
    item_params = list(a = rlnorm(n, 0, 0.25), b = rnorm(n))
  )
}

make_design_1pl <- function(n = 10) {
  irt_design(model = "1PL", n_items = n, item_params = list(b = rnorm(n)))
}

make_design_grm <- function(n = 10, n_cat = 5) {
  a <- rlnorm(n, 0, 0.25)
  b <- t(apply(matrix(rnorm(n * (n_cat - 1)), nrow = n), 1, sort))
  irt_design(model = "GRM", n_items = n, item_params = list(a = a, b = b))
}

# --- Basic Construction (missing = "none") ------------------------------------

test_that("irt_study creates valid study with no missing data", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = c(100, 250, 500))

  expect_s3_class(study, "irt_study")
  expect_identical(study$design, d)
  expect_equal(study$missing, "none")
  expect_equal(study$missing_rate, 0)
  expect_equal(study$sample_sizes, c(100L, 250L, 500L))
})

test_that("irt_study defaults missing to 'none'", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 200)
  expect_equal(study$missing, "none")
})

test_that("irt_study coerces sample_sizes to integer", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = c(100.0, 500.0))
  expect_true(is.integer(study$sample_sizes))
  expect_equal(study$sample_sizes, c(100L, 500L))
})

test_that("irt_study works with single sample size", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 300)
  expect_equal(study$sample_sizes, 300L)
})

# --- Missing Data Mechanisms --------------------------------------------------

test_that("irt_study accepts missing = 'mcar' with missing_rate", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = c(200, 400),
                     missing = "mcar", missing_rate = 0.2)

  expect_equal(study$missing, "mcar")
  expect_equal(study$missing_rate, 0.2)
})

test_that("irt_study accepts missing = 'mar' with missing_rate", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = 500,
                     missing = "mar", missing_rate = 0.15)

  expect_equal(study$missing, "mar")
  expect_equal(study$missing_rate, 0.15)
})

test_that("irt_study accepts missing = 'booklet' with test_design", {
  d <- make_design_2pl(n = 20)
  # Booklet design: 2 booklets, each with 15 of 20 items (10 common + 5 unique)
  booklet_matrix <- matrix(
    c(rep(1, 15), rep(0, 5),  # booklet 1: items 1-15
      rep(0, 5), rep(1, 15)), # booklet 2: items 6-20
    nrow = 2, ncol = 20, byrow = TRUE
  )
  study <- irt_study(d, sample_sizes = c(200, 400),
                     missing = "booklet",
                     test_design = list(booklet_matrix = booklet_matrix))

  expect_equal(study$missing, "booklet")
  expect_true(!is.null(study$test_design))
  expect_true(!is.null(study$test_design$booklet_matrix))
  expect_equal(ncol(study$test_design$booklet_matrix), 20L)
})

test_that("irt_study accepts missing = 'linking' with test_design", {
  d <- make_design_2pl(n = 30)
  # Linking design: 3 forms with common anchor items
  linking_matrix <- matrix(0, nrow = 3, ncol = 30)
  linking_matrix[1, 1:15]  <- 1   # form 1: items 1-15
  linking_matrix[2, 8:22]  <- 1   # form 2: items 8-22 (overlap 8-15)
  linking_matrix[3, 16:30] <- 1   # form 3: items 16-30 (overlap 16-22)

  study <- irt_study(d, sample_sizes = 500,
                     missing = "linking",
                     test_design = list(linking_matrix = linking_matrix))

  expect_equal(study$missing, "linking")
  expect_true(!is.null(study$test_design$linking_matrix))
})

test_that("irt_study sets missing_rate to 0 when missing = 'none'", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 100, missing = "none")
  expect_equal(study$missing_rate, 0)
})

# --- Input Validation: design argument ----------------------------------------

test_that("irt_study requires irt_design as first argument", {
  expect_error(
    irt_study("not_a_design", sample_sizes = 100),
    "irt_design"
  )
  expect_error(
    irt_study(list(model = "1PL"), sample_sizes = 100),
    "irt_design"
  )
})

test_that("irt_study rejects NULL design", {
  expect_error(
    irt_study(NULL, sample_sizes = 100),
    "irt_design|design|NULL"
  )
})

# --- Input Validation: sample_sizes -------------------------------------------

test_that("irt_study rejects non-positive sample sizes", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = c(100, 0, 500)),
    "sample_sizes|positive"
  )
  expect_error(
    irt_study(d, sample_sizes = -10),
    "sample_sizes|positive"
  )
})

test_that("irt_study rejects non-numeric sample sizes", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = "one hundred"),
    "sample_sizes|numeric|integer"
  )
})

test_that("irt_study rejects NA in sample sizes", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = c(100, NA, 500)),
    "sample_sizes|NA"
  )
})

test_that("irt_study rejects empty sample_sizes", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = integer(0)),
    "sample_sizes|empty|length"
  )
})

test_that("irt_study rejects fractional sample sizes", {
  d <- make_design_1pl()
  # 100.5 is not a whole number — should error or truncate
  expect_error(
    irt_study(d, sample_sizes = c(100.5, 200.7)),
    "sample_sizes|integer|whole"
  )
})

# --- Input Validation: missing & missing_rate ---------------------------------

test_that("irt_study rejects unsupported missing mechanism", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "listwise"),
    "missing"
  )
})

test_that("irt_study rejects missing_rate outside [0, 1)", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "mcar", missing_rate = 1.0),
    "missing_rate"
  )
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "mcar", missing_rate = -0.1),
    "missing_rate"
  )
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "mcar", missing_rate = 1.5),
    "missing_rate"
  )
})

test_that("irt_study requires missing_rate when missing is 'mcar'", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "mcar"),
    "missing_rate"
  )
})

test_that("irt_study requires missing_rate when missing is 'mar'", {
  d <- make_design_1pl()
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "mar"),
    "missing_rate"
  )
})

test_that("irt_study ignores missing_rate when missing = 'none'", {
  d <- make_design_1pl()
  # Providing a missing_rate when missing = "none" should be quietly ignored
  study <- irt_study(d, sample_sizes = 100, missing = "none", missing_rate = 0.5)
  expect_equal(study$missing_rate, 0)
})

# --- Input Validation: booklet/linking test_design ----------------------------

test_that("irt_study requires test_design for booklet missing", {
  d <- make_design_2pl(n = 20)
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "booklet"),
    "test_design|booklet"
  )
})

test_that("irt_study requires test_design for linking missing", {
  d <- make_design_2pl(n = 20)
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "linking"),
    "test_design|linking"
  )
})

test_that("irt_study rejects booklet matrix with wrong number of columns", {
  d <- make_design_2pl(n = 20)
  # Matrix has 10 columns but design has 20 items
  bad_matrix <- matrix(1, nrow = 2, ncol = 10)
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "booklet",
              test_design = list(booklet_matrix = bad_matrix)),
    "n_items|columns|items"
  )
})

test_that("irt_study rejects linking matrix with wrong number of columns", {
  d <- make_design_2pl(n = 20)
  bad_matrix <- matrix(1, nrow = 3, ncol = 15)
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "linking",
              test_design = list(linking_matrix = bad_matrix)),
    "n_items|columns|items"
  )
})

test_that("irt_study rejects booklet matrix with non-binary values", {
  d <- make_design_2pl(n = 10)
  bad_matrix <- matrix(c(1, 0, 2, 0, 1, 1, 0, 1, 1, 0,
                         0, 1, 0, 1, 0, 0, 1, 0, 0, 1),
                       nrow = 2, ncol = 10, byrow = TRUE)
  expect_error(
    irt_study(d, sample_sizes = 100, missing = "booklet",
              test_design = list(booklet_matrix = bad_matrix)),
    "binary|0.*1"
  )
})

# --- S3 Class Structure -------------------------------------------------------

test_that("irt_study returns an S3 object with correct class", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = c(100, 500))
  expect_s3_class(study, "irt_study")
  expect_true(is.list(study))
})

test_that("irt_study object contains all expected elements", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = c(100, 500),
                     missing = "mcar", missing_rate = 0.1)
  expected_names <- c("design", "missing", "missing_rate",
                      "sample_sizes", "test_design")
  expect_true(all(expected_names %in% names(study)))
})

test_that("irt_study preserves irt_design object unchanged", {
  d <- make_design_2pl()
  study <- irt_study(d, sample_sizes = c(100, 500))
  expect_identical(study$design, d)
})

test_that("irt_study object is a plain list (not reference class)", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = 100)
  expect_false(is.environment(study))
  expect_true(is.list(study))
})

# --- Print Method -------------------------------------------------------------

test_that("print.irt_study produces informative output", {
  d <- make_design_2pl(n = 20)
  study <- irt_study(d, sample_sizes = c(100, 250, 500),
                     missing = "mcar", missing_rate = 0.2)
  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")

  # Should mention IRT study or study conditions
  expect_match(output_text, "study|Study", ignore.case = FALSE)
  # Should mention the underlying model
  expect_match(output_text, "2PL", fixed = TRUE)
  # Should mention sample sizes
  expect_match(output_text, "100")
  expect_match(output_text, "500")
  # Should mention missing data mechanism
  expect_match(output_text, "mcar|MCAR", ignore.case = FALSE)
  # Should mention missing rate
  expect_match(output_text, "0.2|20%")
  # Should return the object invisibly
  expect_invisible(print(study))
})

test_that("print.irt_study works for booklet design", {
  d <- make_design_2pl(n = 20)
  booklet_matrix <- matrix(
    c(rep(1, 15), rep(0, 5),
      rep(0, 5), rep(1, 15)),
    nrow = 2, ncol = 20, byrow = TRUE
  )
  study <- irt_study(d, sample_sizes = 300,
                     missing = "booklet",
                     test_design = list(booklet_matrix = booklet_matrix))
  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")

  # Should mention booklet
  expect_match(output_text, "booklet|Booklet", ignore.case = FALSE)
  # Should mention number of booklets/forms
  expect_match(output_text, "2")
})

test_that("print.irt_study works for no-missing design", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = c(50, 100))
  output <- capture.output(print(study))
  output_text <- paste(output, collapse = "\n")

  # Should indicate no missing data or complete data
  expect_match(output_text, "none|complete|no missing", ignore.case = TRUE)
})

# --- GRM Integration ----------------------------------------------------------

test_that("irt_study works with GRM design", {
  d <- make_design_grm()
  study <- irt_study(d, sample_sizes = c(200, 500))

  expect_s3_class(study, "irt_study")
  expect_equal(study$design$model, "GRM")
})

# --- Edge Cases ---------------------------------------------------------------

test_that("irt_study sorts sample_sizes in ascending order", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = c(500, 100, 250))
  expect_equal(study$sample_sizes, c(100L, 250L, 500L))
})

test_that("irt_study removes duplicate sample sizes", {
  d <- make_design_1pl()
  study <- irt_study(d, sample_sizes = c(100, 250, 100, 500, 250))
  expect_equal(study$sample_sizes, c(100L, 250L, 500L))
})

test_that("irt_study accepts large sample size vector", {
  d <- make_design_1pl()
  sizes <- seq(50, 2000, by = 50)
  study <- irt_study(d, sample_sizes = sizes)
  expect_equal(length(study$sample_sizes), length(unique(sizes)))
})
