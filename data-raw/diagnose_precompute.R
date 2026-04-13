# Diagnostic script: trace precompute_vignettes.R step by step
# Run from the irtsim/ directory: Rscript data-raw/diagnose_precompute.R

cat("Step 1: Working directory\n")
cat("  getwd() =", getwd(), "\n")
cat("  DESCRIPTION exists?", file.exists("DESCRIPTION"), "\n")
flush.console()

cat("\nStep 2: library(irtsim)\n")
tryCatch({
  library(irtsim)
  cat("  OK — irtsim loaded\n")
}, error = function(e) {
  cat("  FAILED:", conditionMessage(e), "\n")
})
flush.console()

cat("\nStep 3: library(future)\n")
tryCatch({
  library(future)
  cat("  OK — future loaded\n")
}, error = function(e) {
  cat("  FAILED:", conditionMessage(e), "\n")
})
flush.console()

cat("\nStep 4: plan(multisession)\n")
cat("  detectCores() =", parallel::detectCores(), "\n")
cat("  workers =", parallel::detectCores() - 1, "\n")
flush.console()
tryCatch({
  plan(multisession, workers = parallel::detectCores() - 1)
  cat("  OK — plan set\n")
}, error = function(e) {
  cat("  FAILED:", conditionMessage(e), "\n")
})
flush.console()

cat("\nStep 5: inst/extdata directory\n")
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
cat("  dir exists?", dir.exists("inst/extdata"), "\n")
flush.console()

cat("\nStep 6: Minimal 1PL smoke test (3 items, N=100, 3 iters, serial)\n")
tryCatch({
  d <- irt_design(model = "1PL", n_items = 3L,
                  item_params = list(b = c(-1, 0, 1)))
  s <- irt_study(d, sample_sizes = 100L)
  t0 <- proc.time()["elapsed"]
  r <- irt_simulate(s, iterations = 3L, seed = 1L, progress = FALSE,
                    se = FALSE, compute_theta = FALSE)
  elapsed <- proc.time()["elapsed"] - t0
  cat("  OK — 3 iterations in", round(elapsed, 1), "sec\n")
  cat("  Converged:", sum(r$theta_results$converged), "/",
      nrow(r$theta_results), "\n")
}, error = function(e) {
  cat("  FAILED:", conditionMessage(e), "\n")
})
flush.console()

cat("\nStep 7: Parallel smoke test (3 items, N=100, 3 iters, parallel)\n")
tryCatch({
  d <- irt_design(model = "1PL", n_items = 3L,
                  item_params = list(b = c(-1, 0, 1)))
  s <- irt_study(d, sample_sizes = 100L)
  t0 <- proc.time()["elapsed"]
  r <- irt_simulate(s, iterations = 3L, seed = 1L, progress = FALSE,
                    parallel = TRUE, se = FALSE, compute_theta = FALSE)
  elapsed <- proc.time()["elapsed"] - t0
  cat("  OK — 3 parallel iterations in", round(elapsed, 1), "sec\n")
  cat("  Converged:", sum(r$theta_results$converged), "/",
      nrow(r$theta_results), "\n")
}, error = function(e) {
  cat("  FAILED:", conditionMessage(e), "\n")
})
flush.console()

cat("\nDiagnostics complete.\n")
plan(sequential)
