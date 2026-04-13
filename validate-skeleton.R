# Run this script from the irtsim/ directory to validate the package skeleton.
# Usage: Rscript validate-skeleton.R
#   or in RStudio: source("validate-skeleton.R")

cat("=== irtsim skeleton validation ===\n\n")

# 1. Check available (CRAN name collision)
if (requireNamespace("available", quietly = TRUE)) {
  cat("1. Checking package name availability...\n")
  available::available("irtsim")
} else {
  cat("1. SKIP: install.packages('available') to check name\n")
}

# 2. devtools::check()
if (requireNamespace("devtools", quietly = TRUE)) {
  cat("\n2. Running devtools::check()...\n")
  results <- devtools::check(pkg = ".", quiet = FALSE)
  cat("\n--- CHECK RESULTS ---\n")
  cat("Errors:   ", length(results$errors), "\n")
  cat("Warnings: ", length(results$warnings), "\n")
  cat("Notes:    ", length(results$notes), "\n")

  if (length(results$errors) == 0 && length(results$warnings) == 0) {
    cat("\nSkeleton PASSES. Ready for Objective 2.\n")
  } else {
    cat("\nSkeleton has issues. Fix before proceeding.\n")
  }
} else {
  cat("\n2. SKIP: install.packages('devtools') to run check\n")
}
