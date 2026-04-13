# test-missing_registry.R
# TDD tests for missing mechanism registry — Objective 22
# These tests define the expected behavior of the missing registry BEFORE implementation.

test_that("get_missing_config returns valid configuration for known mechanisms", {
  cfg <- get_missing_config("mcar")
  expect_type(cfg, "list")
  expect_true(all(c("requires_rate", "requires_test_design", "test_design_key",
                    "print_label", "print_style", "design_unit") %in% names(cfg)))
  expect_type(cfg$requires_rate, "logical")
  expect_type(cfg$requires_test_design, "logical")
  expect_type(cfg$print_label, "character")
  expect_type(cfg$print_style, "character")
})

test_that("get_missing_config errors on unknown mechanism with helpful message", {
  expect_error(
    get_missing_config("bogus"),
    "must be one of"
  )
  err <- tryCatch(get_missing_config("bogus"), error = function(e) e$message)
  expect_true(grepl("none", err, ignore.case = TRUE))
  expect_true(grepl("mcar", err, ignore.case = TRUE))
  expect_true(grepl("booklet", err, ignore.case = TRUE))
})

test_that("valid_missing_mechanisms returns character vector of all 5 mechanisms", {
  vm <- valid_missing_mechanisms()
  expect_type(vm, "character")
  expect_length(vm, 5L)
  expect_setequal(
    vm,
    c("none", "mcar", "mar", "booklet", "linking")
  )
})

test_that("get_missing_config('none') has correct metadata", {
  cfg <- get_missing_config("none")
  expect_false(cfg$requires_rate)
  expect_false(cfg$requires_test_design)
  expect_true(is.na(cfg$test_design_key))
  expect_equal(cfg$print_label, "none (complete data)")
  expect_equal(cfg$print_style, "plain")
  expect_true(is.na(cfg$design_unit))
})

test_that("get_missing_config('mcar') has correct metadata", {
  cfg <- get_missing_config("mcar")
  expect_true(cfg$requires_rate)
  expect_false(cfg$requires_test_design)
  expect_true(is.na(cfg$test_design_key))
  expect_equal(cfg$print_label, "MCAR")
  expect_equal(cfg$print_style, "rate")
  expect_true(is.na(cfg$design_unit))
})

test_that("get_missing_config('mar') has correct metadata", {
  cfg <- get_missing_config("mar")
  expect_true(cfg$requires_rate)
  expect_false(cfg$requires_test_design)
  expect_true(is.na(cfg$test_design_key))
  expect_equal(cfg$print_label, "MAR")
  expect_equal(cfg$print_style, "rate")
  expect_true(is.na(cfg$design_unit))
})

test_that("get_missing_config('booklet') has correct metadata", {
  cfg <- get_missing_config("booklet")
  expect_false(cfg$requires_rate)
  expect_true(cfg$requires_test_design)
  expect_equal(cfg$test_design_key, "booklet_matrix")
  expect_equal(cfg$print_label, "booklet design")
  expect_equal(cfg$print_style, "design")
  expect_equal(cfg$design_unit, "booklets")
})

test_that("get_missing_config('linking') has correct metadata", {
  cfg <- get_missing_config("linking")
  expect_false(cfg$requires_rate)
  expect_true(cfg$requires_test_design)
  expect_equal(cfg$test_design_key, "linking_matrix")
  expect_equal(cfg$print_label, "linking design")
  expect_equal(cfg$print_style, "design")
  expect_equal(cfg$design_unit, "forms")
})

test_that("registry keys equal valid_missing_mechanisms() output", {
  registry <- .get_missing_registry()
  expect_setequal(names(registry), valid_missing_mechanisms())
})

test_that("all 5 mechanisms are retrievable from registry", {
  for (mech in valid_missing_mechanisms()) {
    expect_no_error(get_missing_config(mech))
  }
})
