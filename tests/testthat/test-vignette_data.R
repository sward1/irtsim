# test-vignette_data.R
# Verify that precomputed vignette .rds files exist and load properly.
# This is a regression test for vignette builds.

test_that("vignette example 1 (model misspecification) results exist and load", {
  skip_if_not(
    file.exists(system.file("extdata", "vignette_ex1_results.rds", package = "irtsim")),
    "Precomputed vignette data missing. Run data-raw/precompute_vignettes.R"
  )

  results_path <- system.file("extdata", "vignette_ex1_results.rds", package = "irtsim")
  results_list <- readRDS(results_path)

  expect_true(is.list(results_list))
  expect_true(inherits(results_list$correct, "irt_results"))
  expect_true(inherits(results_list$misspec, "irt_results"))
})

test_that("vignette example 2 (MCAR with custom criterion) results exist and load", {
  skip_if_not(
    file.exists(system.file("extdata", "vignette_ex2_results.rds", package = "irtsim")),
    "Precomputed vignette data missing. Run data-raw/precompute_vignettes.R"
  )

  results_path <- system.file("extdata", "vignette_ex2_results.rds", package = "irtsim")
  results_list <- readRDS(results_path)

  expect_true(is.list(results_list))
  expect_true(inherits(results_list$complete, "irt_results"))
  expect_true(inherits(results_list$mcar30, "irt_results"))
})

test_that("vignette example 3 (GRM with custom criterion) results exist and load", {
  skip_if_not(
    file.exists(system.file("extdata", "vignette_ex3_results.rds", package = "irtsim")),
    "Precomputed vignette data missing. Run data-raw/precompute_vignettes.R"
  )

  results_path <- system.file("extdata", "vignette_ex3_results.rds", package = "irtsim")
  results <- readRDS(results_path)

  expect_true(inherits(results, "irt_results"))
})
