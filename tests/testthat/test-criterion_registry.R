test_that("get_criterion_config returns valid configuration for known criteria", {
  cfg <- get_criterion_config("rmse")
  expect_type(cfg, "list")
  expect_true(all(c("direction", "use_abs", "display_label") %in% names(cfg)))
  expect_type(cfg$direction, "character")
  expect_type(cfg$use_abs, "logical")
  expect_type(cfg$display_label, "character")
})

test_that("get_criterion_config errors on unknown criterion with helpful message", {
  expect_error(
    get_criterion_config("bogus"),
    "must be one of"
  )
  err <- tryCatch(get_criterion_config("bogus"), error = function(e) e$message)
  expect_true(grepl("bias", err, ignore.case = TRUE))
  expect_true(grepl("coverage", err, ignore.case = TRUE))
})

test_that("valid_criteria returns character vector of all 7 criteria", {
  vc <- valid_criteria()
  expect_type(vc, "character")
  expect_length(vc, 7L)
  expect_setequal(
    vc,
    c("bias", "empirical_se", "mse", "rmse", "coverage", "mcse_bias", "mcse_mse")
  )
})

test_that("get_criterion_config('bias')$use_abs is TRUE", {
  expect_true(get_criterion_config("bias")$use_abs)
})

test_that("all criteria except bias have use_abs = FALSE", {
  for (crit in setdiff(valid_criteria(), "bias")) {
    expect_false(
      get_criterion_config(crit)$use_abs,
      label = paste("criterion:", crit)
    )
  }
})

test_that("get_criterion_config('coverage')$direction is 'higher_is_better'", {
  expect_equal(get_criterion_config("coverage")$direction, "higher_is_better")
})

test_that("all criteria except coverage have direction = 'lower_is_better'", {
  for (crit in setdiff(valid_criteria(), "coverage")) {
    expect_equal(
      get_criterion_config(crit)$direction,
      "lower_is_better",
      label = paste("criterion:", crit)
    )
  }
})

test_that("all 7 criteria are retrievable from registry", {
  for (crit in valid_criteria()) {
    expect_no_error(get_criterion_config(crit))
  }
})

test_that("display labels match expected values", {
  expected_labels <- list(
    bias         = "Absolute Bias",
    empirical_se = "Empirical SE",
    mse          = "MSE",
    rmse         = "RMSE",
    coverage     = "Coverage",
    mcse_bias    = "MCSE (Bias)",
    mcse_mse     = "MCSE (MSE)"
  )

  for (crit in valid_criteria()) {
    label <- get_criterion_config(crit)$display_label
    expect_equal(label, expected_labels[[crit]])
  }
})

test_that("registry keys equal valid_criteria() output", {
  registry <- .get_criterion_registry()
  expect_setequal(names(registry), valid_criteria())
})
