# ==============================================================================
# Precompute Vignette Results
# ==============================================================================
#
# This script precomputes simulation results for the three vignettes at full
# scale (not the 3-iteration stubs used during build time). The .rds files
# are saved to inst/extdata/ and loaded by the vignette chunks at build time.
#
# **Important:** Re-run this script whenever vignette code changes.
#
# **How to run:**
#   source("data-raw/precompute_vignettes.R")
#
# This script uses parallel = TRUE (via future.apply) for speed. Results are
# bit-identical across re-runs with the same seed within parallel mode, but
# NOT bit-identical to a serial run with the same seed. See ?irt_simulate for
# the reproducibility contract.
#
# Wall-clock time: expect ~2--5 minutes depending on machine. Adjust
# iterations or sample sizes below if needed.
#
# File sizes: expect .rds files of ~0.5--1.5 MB each (total ~2--4 MB).

# --- Package + parallel setup -------------------------------------------------
# NOTE: Do NOT use devtools::load_all() here. Workers spawned by
# plan(multisession) run in fresh R processes that cannot see a load_all()
# namespace; they try library(irtsim) during unserialization and fail with
# "there is no package called 'irtsim'". We therefore install the in-source
# package into .libPaths() once at the top of the script, then use
# library(irtsim) so every worker can find it. This is idempotent and fast
# (quick = TRUE skips docs/vignettes/multiarch).
cat("=== Precompute Vignette Results ===\n")
cat("Started:", format(Sys.time()), "\n\n")

# Install from source so parallel workers can find the package.
# Skip if irtsim is already loadable (saves 10-30s).
cat("[setup] Checking irtsim installation... ")
if (!requireNamespace("irtsim", quietly = TRUE)) {
  cat("not found — installing from source\n")
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::install(
      pkg     = ".",
      quick   = TRUE,
      upgrade = FALSE,
      build   = FALSE,
      args    = "--no-multiarch",
      quiet   = FALSE
    )
  } else {
    stop("irtsim is not installed and devtools is not available to install it.")
  }
} else {
  cat("OK (already installed)\n")
}
library(irtsim)

# Set up parallel backend for speed
cat("[setup] Setting up parallel backend... ")
library(future)
n_workers <- parallel::detectCores() - 1
plan(multisession, workers = n_workers)
cat(sprintf("OK (%d workers)\n", n_workers))

# Create extdata directory if it doesn't exist
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
cat("\n")

# ==============================================================================
# EXAMPLE 1 (PAPER REPRODUCTION): Linked-test design, 1PL estimation
# ==============================================================================
# Faithful reproduction of Schroeders & Gnambs (2025), Example 1
# (https://ulrich-schroeders.github.io/IRT-sample-size/example_1.html).
#
# Design decisions taken from the paper:
#   - 30 items, 1PL estimation model
#   - Item discriminations generated as rnorm(30, 1, 0.1) (near-Rasch; fit 1PL)
#   - Item difficulties b = seq(-2, 2, length.out = 30)
#   - Two forms: form 1 = odd items + common block (items 13-18),
#     form 2 = even items + common block (items 13-18)
#   - 438 Monte Carlo iterations
#   - Sample sizes N = seq(100, 600, 50)
#   - Performance criterion: MSE (threshold 0.05)
#
# Minor fidelity note: irtsim's `apply_missing_structured()` assigns
# examinees to forms in round-robin order. The paper uses random
# assignment via `sample(c(1, 2), n, replace = TRUE)`. With N >= 100
# and balanced target proportions the difference is negligible for
# MSE trajectories, but it is disclosed in the vignette.

cat("[1/4] Example 1 — Paper Reproduction (438 iter × 11 N × 30 items, 1PL)\n")

set.seed(2024)
n_items_ex1p <- 30L

a_vals_ex1p <- rnorm(n_items_ex1p, mean = 1, sd = 0.1)
b_vals_ex1p <- seq(-2, 2, length.out = n_items_ex1p)

# Linking matrix: two forms with items 13-18 common
linking_matrix_ex1p <- matrix(0L, nrow = 2L, ncol = n_items_ex1p)
form1_items_ex1p <- sort(unique(c(seq(1L, n_items_ex1p, by = 2L), 13:18)))
form2_items_ex1p <- sort(unique(c(seq(2L, n_items_ex1p, by = 2L), 13:18)))
linking_matrix_ex1p[1L, form1_items_ex1p] <- 1L
linking_matrix_ex1p[2L, form2_items_ex1p] <- 1L

design_ex1p <- irt_design(
  model       = "2PL",
  n_items     = n_items_ex1p,
  item_params = list(a = a_vals_ex1p, b = b_vals_ex1p),
  theta_dist  = "normal"
)

study_ex1p <- irt_study(
  design_ex1p,
  sample_sizes     = seq(100L, 600L, by = 50L),
  missing          = "linking",
  test_design      = list(linking_matrix = linking_matrix_ex1p),
  estimation_model = "1PL"
)

cat("  Running... "); flush.console()
t0 <- proc.time()["elapsed"]
res_ex1p <- irt_simulate(
  study_ex1p,
  iterations = 438L,
  seed       = 2024L,
  parallel   = TRUE,
  progress   = FALSE,
  se         = FALSE,
  compute_theta = FALSE
)

cat(sprintf("done (%.0fs)\n", proc.time()["elapsed"] - t0))
saveRDS(
  list(
    results          = res_ex1p,
    a_vals           = a_vals_ex1p,
    b_vals           = b_vals_ex1p,
    linking_matrix   = linking_matrix_ex1p,
    form1_items      = form1_items_ex1p,
    form2_items      = form2_items_ex1p
  ),
  "inst/extdata/vignette_ex1_paper.rds",
  version = 2
)
cat(sprintf("  Saved: vignette_ex1_paper.rds (%.1f MB)\n\n",
    file.size("inst/extdata/vignette_ex1_paper.rds") / 1024^2))

# ==============================================================================
# EXAMPLE 1b: Model Misspecification (gen=2PL, est=1PL)
# ==============================================================================
# Substantial discrimination variability (a_sd=0.5) so Rasch misspecification
# produces a measurable sample-size penalty.

cat("[2/4] Example 1b — Model Misspecification (500 iter × 5 N × 20 items, 2PL vs 1PL)\n")

set.seed(2024)
n_items_ex1 <- 20

params_ex1 <- irt_params_2pl(
  n_items = n_items_ex1,
  a_dist  = "lnorm",
  a_mean  = 0.2,
  a_sd    = 0.5,
  b_mean  = 0,
  b_sd    = 1,
  seed    = 2024
)

design_ex1 <- irt_design(
  model       = "2PL",
  n_items     = n_items_ex1,
  item_params = params_ex1,
  theta_dist  = "normal"
)

sample_sizes_ex1 <- seq(200, 1000, by = 200)

# Study 1: Correct model (2PL generates, 2PL fits)
study_ex1_correct <- irt_study(
  design_ex1,
  sample_sizes = sample_sizes_ex1
)

# Study 2: Misspecified model (2PL generates, 1PL fits)
study_ex1_misspec <- irt_study(
  design_ex1,
  sample_sizes = sample_sizes_ex1,
  estimation_model = "1PL"
)

# Run simulations (500 iterations × 5 sample sizes)
cat("  Correct model... "); flush.console()
t0 <- proc.time()["elapsed"]
res_ex1_correct <- irt_simulate(
  study_ex1_correct,
  iterations = 500,
  seed = 2024,
  parallel = TRUE,
  progress = FALSE,
  se = FALSE,
  compute_theta = FALSE
)

cat(sprintf("done (%.0fs)\n", proc.time()["elapsed"] - t0))
cat("  Misspecified model... "); flush.console()
t0 <- proc.time()["elapsed"]
res_ex1_misspec <- irt_simulate(
  study_ex1_misspec,
  iterations = 500,
  seed = 2024,
  parallel = TRUE,
  progress = FALSE,
  se = FALSE,
  compute_theta = FALSE
)

# Save results
cat(sprintf("done (%.0fs)\n", proc.time()["elapsed"] - t0))
saveRDS(
  list(correct = res_ex1_correct, misspec = res_ex1_misspec),
  "inst/extdata/vignette_ex1_results.rds",
  version = 2
)
cat(sprintf("  Saved: vignette_ex1_results.rds (%.1f MB)\n\n",
    file.size("inst/extdata/vignette_ex1_results.rds") / 1024^2))

# ==============================================================================
# EXAMPLE 2: MCAR Missingness with Custom Criterion (Conditional Reliability)
# ==============================================================================
# This example demonstrates the impact of MCAR missingness on parameter recovery
# and uses a custom criterion function to compute conditional reliability.

cat("[3/4] Example 2 — MCAR Missingness (500 iter × 5 N × 30 items, 2PL)\n")

set.seed(2024)
n_items_ex2 <- 30

params_ex2 <- irt_params_2pl(
  n_items = n_items_ex2,
  a_dist  = "lnorm",
  a_mean  = 0.2,
  a_sd    = 0.3,
  b_mean  = 0,
  b_sd    = 1,
  seed    = 2024
)

design_ex2 <- irt_design(
  model       = "2PL",
  n_items     = n_items_ex2,
  item_params = params_ex2,
  theta_dist  = "normal"
)

sample_sizes_ex2 <- seq(200, 1000, by = 200)

study_ex2_complete <- irt_study(
  design_ex2,
  sample_sizes = sample_sizes_ex2
)

study_ex2_mcar30 <- irt_study(
  design_ex2,
  sample_sizes = sample_sizes_ex2,
  missing = "mcar",
  missing_rate = 0.30
)

# Run simulations (500 iterations × 3 sample sizes × 2 conditions)
cat("  Complete data... "); flush.console()
t0 <- proc.time()["elapsed"]
res_ex2_complete <- irt_simulate(
  study_ex2_complete,
  iterations = 500,
  seed = 2024,
  parallel = TRUE,
  progress = FALSE,
  se = TRUE,
  compute_theta = TRUE
)

cat(sprintf("done (%.0fs)\n", proc.time()["elapsed"] - t0))
cat("  30% MCAR... "); flush.console()
t0 <- proc.time()["elapsed"]
res_ex2_mcar30 <- irt_simulate(
  study_ex2_mcar30,
  iterations = 500,
  seed = 2024,
  parallel = TRUE,
  progress = FALSE,
  se = TRUE,
  compute_theta = TRUE
)

# Save results
cat(sprintf("done (%.0fs)\n", proc.time()["elapsed"] - t0))
saveRDS(
  list(complete = res_ex2_complete, mcar30 = res_ex2_mcar30),
  "inst/extdata/vignette_ex2_results.rds",
  version = 2
)
cat(sprintf("  Saved: vignette_ex2_results.rds (%.1f MB)\n\n",
    file.size("inst/extdata/vignette_ex2_results.rds") / 1024^2))

# ==============================================================================
# EXAMPLE 3: GRM Parameter Recovery for Clinical Scale
# ==============================================================================
# This example uses a GRM and demonstrates custom criterion callbacks
# for computing condition-specific metrics (e.g., posterior SEM at a target theta).

cat("[4/4] Example 3 — GRM (500 iter × 6 N × 9 items)\n")

set.seed(2024)
n_items_ex3 <- 9
n_categories_ex3 <- 4

a_vals_ex3 <- c(2.21, 1.72, 1.68, 1.35, 1.19, 1.63, 1.41, 1.26, 1.08)

b_mat_ex3 <- matrix(c(
  -0.47,  1.36,  2.86,
  -0.30,  1.45,  2.96,
   0.04,  1.76,  3.15,
  -0.21,  1.53,  3.22,
   0.26,  2.05,  3.60,
  -0.02,  1.82,  3.35,
   0.51,  2.18,  3.70,
   0.78,  2.45,  3.90,
   1.05,  2.72,  4.20
), nrow = n_items_ex3, ncol = n_categories_ex3 - 1, byrow = TRUE)

design_ex3 <- irt_design(
  model       = "GRM",
  n_items     = n_items_ex3,
  item_params = list(a = a_vals_ex3, b = b_mat_ex3),
  theta_dist  = "normal"
)

sample_sizes_ex3 <- seq(200, 1200, by = 200)

study_ex3 <- irt_study(
  design_ex3,
  sample_sizes = sample_sizes_ex3
)

# Run simulation (500 iterations × 3 sample sizes)
cat("  Running... "); flush.console()
t0 <- proc.time()["elapsed"]
res_ex3 <- irt_simulate(
  study_ex3,
  iterations = 500,
  seed = 2024,
  parallel = TRUE,
  progress = FALSE,
  se = TRUE,
  compute_theta = TRUE
)

# Save results
cat(sprintf("done (%.0fs)\n", proc.time()["elapsed"] - t0))
saveRDS(
  res_ex3,
  "inst/extdata/vignette_ex3_results.rds",
  version = 2
)
cat(sprintf("  Saved: vignette_ex3_results.rds (%.1f MB)\n\n",
    file.size("inst/extdata/vignette_ex3_results.rds") / 1024^2))

# ==============================================================================
# Summary
# ==============================================================================

cat("=== Precomputation Complete ===\n")
cat("Files in inst/extdata/:\n")
rds_files <- list.files("inst/extdata", pattern = "\\.rds$", full.names = TRUE)
for (f in rds_files) {
  cat(sprintf("  %s (%.1f MB)\n", basename(f), file.size(f) / 1024^2))
}
cat(sprintf("\nFinished: %s\n", format(Sys.time())))

# Reset plan
future::plan(future::sequential)
