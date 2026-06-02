# scalability_benchmark.R — Obj 36 (test-length scalability benchmark)
# -----------------------------------------------------------------------------
# Profiles irt_simulate() across realistic test lengths and sample sizes so
# v0.2.0 can ship a documented per-fit expectation. Now that 3PL/PCM/GPCM
# ship, real DoD / I-O assessments run 60-200 items — well past the 30-item
# paper Ex1 ceiling. This harness measures wall-clock + allocated memory per
# cell and isolates the mirt time fraction (the suspected dominator).
#
# NOT part of the package build: benchmarks/ is in .Rbuildignore. Run it by
# hand; it does not run on CRAN or in R CMD check.
#
# Two passes, deliberately separated (bench's allocation tracking thrashes the
# allocator on >10 GB cells — that caused the malloc-stack spam and the bogus
# 78s/fit reading in the first run):
#
#   PASS 1 — timing (memory off, full grid):
#     devtools::load_all(".")        # or library(irtsim)
#     source("benchmarks/scalability_benchmark.R")
#     timing <- run_scalability_sweep()           # -> results/scalability_results.csv
#
#   PASS 2 — peak heap (gc-based, representative subset; FRESH session is best):
#     mem <- run_memory_probe()                   # -> results/memory_probe.csv
#
#   OPTIONAL — attribute time to mirt on one cell:
#     prof <- profile_mirt_fraction(model = "2PL", n_items = 100, sample_size = 500)
#
# Requires: bench (wall-clock). profvis is optional, only for
# profile_mirt_fraction(use_profvis = TRUE).
# -----------------------------------------------------------------------------

# --- Sweep configuration (edit here to shrink the grid if wall-clock blows up) -
BENCH_N_ITEMS      <- c(30, 60, 100, 150)
BENCH_SAMPLE_SIZES <- c(200, 500, 1000)
BENCH_ITERATIONS   <- 10L          # MC iterations per cell (drop to 5 if needed)
BENCH_MODELS       <- c("2PL", "GRM")
BENCH_N_CATEGORIES <- 5L           # GRM only: 5-category Likert (typical I-O)
BENCH_SEED         <- 1L

# Where to write outputs (created if absent).
BENCH_OUT_DIR <- "benchmarks/results"


# --- One design for a given (model, n_items) ---------------------------------
# Param generation is seeded and lives OUTSIDE the timed expression — we are
# benchmarking the MC engine (irt_simulate), not parameter generation.
build_design <- function(model, n_items) {
  if (model == "2PL") {
    ip <- irt_params_2pl(
      n_items = n_items,
      a_mean = 0, a_sd = 0.25,   # log-normal: median a = 1
      b_mean = 0, b_sd = 1,
      seed = BENCH_SEED
    )
    irt_design(model = "2PL", n_items = n_items, item_params = ip)
  } else if (model == "GRM") {
    ip <- irt_params_grm(
      n_items = n_items,
      n_categories = BENCH_N_CATEGORIES,
      a_mean = 0, a_sd = 0.25,
      b_mean = 0, b_sd = 1,
      seed = BENCH_SEED
    )
    irt_design(model = "GRM", n_items = n_items, item_params = ip)
  } else {
    stop("Unsupported benchmark model: ", model)
  }
}


# --- One cell: time a single serial irt_simulate() run -----------------------
# Returns a one-row data.frame. Uses bench::mark with iterations = 1 (each cell
# already averages over BENCH_ITERATIONS MC reps internally; a second outer
# repeat would multiply an already-expensive cell). check = FALSE because the
# return value is a fitted-results object, not comparable across cells.
#
# memory = FALSE deliberately: bench's allocation tracking thrashes the macOS
# allocator once a cell allocates >10 GB (the malloc-stack-logging spam and the
# 78s/fit artifact in the first run came from this), so timing and memory are
# measured in SEPARATE passes. Peak heap is captured by run_memory_probe().
run_cell <- function(model, n_items, sample_size, iterations = BENCH_ITERATIONS) {
  design <- build_design(model, n_items)
  study  <- irt_study(design = design, sample_sizes = sample_size)

  cat(sprintf("  [%-3s] n_items = %3d, N = %4d, iters = %d ... ",
              model, n_items, sample_size, iterations))
  utils::flush.console()

  # A single failing cell must not kill the whole sweep: record it as NA + the
  # error message and move on. (Surfaced a real bug at GRM >=60 items in the
  # first run: mirt's "converged" flag can come back NA, tripping an unguarded
  # if() in irt_simulate() — see benchmarks notes / Obj 45.)
  mk <- tryCatch(
    bench::mark(
      irt_simulate(
        study      = study,
        iterations = iterations,
        seed       = BENCH_SEED,
        progress   = FALSE,
        parallel   = FALSE
      ),
      iterations = 1L,
      check      = FALSE,
      filter_gc  = FALSE,
      memory     = FALSE
    ),
    error = function(e) e
  )

  if (inherits(mk, "error")) {
    cat(sprintf("ERROR: %s\n", conditionMessage(mk)))
    utils::flush.console()
    return(data.frame(
      model = model, n_items = n_items, sample_size = sample_size,
      iterations = iterations, total_s = NA_real_, per_fit_s = NA_real_,
      error = conditionMessage(mk), stringsAsFactors = FALSE
    ))
  }

  total_s   <- as.numeric(mk$median, units = "secs")
  per_fit_s <- total_s / iterations

  cat(sprintf("%.1fs total, %.2fs/fit\n", total_s, per_fit_s))
  utils::flush.console()

  data.frame(
    model        = model,
    n_items      = n_items,
    sample_size  = sample_size,
    iterations   = iterations,
    total_s      = round(total_s, 3),
    per_fit_s    = round(per_fit_s, 4),
    error        = NA_character_,
    stringsAsFactors = FALSE
  )
}


# --- Peak-heap measurement for one cell --------------------------------------
# Uses gc()'s "max used" columns rather than bench's cumulative mem_alloc:
#   - peak heap is what actually drives OOM (the user's real concern), whereas
#     mem_alloc counts churned-and-freed bytes;
#   - no Rprofmem/malloc-stack tracking, so no allocator thrash and no inflated
#     timing.
# gc(reset = TRUE) zeroes the max-used watermark; we run one cell, then read it.
measure_cell_peak_mem <- function(model, n_items, sample_size,
                                  iterations = BENCH_ITERATIONS) {
  design <- build_design(model, n_items)
  study  <- irt_study(design = design, sample_sizes = sample_size)

  cat(sprintf("  [%-3s] n_items = %3d, N = %4d ... ", model, n_items, sample_size))
  utils::flush.console()

  invisible(gc(reset = TRUE, full = TRUE))
  invisible(irt_simulate(study = study, iterations = iterations,
                         seed = BENCH_SEED, progress = FALSE, parallel = FALSE))
  g <- gc(full = TRUE)
  # gc() returns a matrix (rows Ncells/Vcells) whose LAST column is always the
  # "max used (Mb)" figure. The column count varies — a "limit (Mb)" column
  # appears when a memory limit is set — so index by ncol(), NOT a hardcoded
  # position (hardcoding 6 read the max-used *cell count* under a 7-col layout).
  peak_mb <- sum(g[, ncol(g)])

  cat(sprintf("peak heap %.0f MB\n", peak_mb))
  utils::flush.console()

  data.frame(
    model       = model,
    n_items     = n_items,
    sample_size = sample_size,
    iterations  = iterations,
    peak_mem_mb = round(peak_mb, 1),
    stringsAsFactors = FALSE
  )
}


# --- Full grid sweep ----------------------------------------------------------
# Cells run smallest-first within each model so an early blow-up is visible
# before the expensive corner. Each cell prints as it completes; Ctrl-C is safe
# (rows completed so far are NOT lost only if you assign incrementally — here we
# rbind at the end, so to be crash-safe we append to a list as we go).
run_scalability_sweep <- function(n_items_grid     = BENCH_N_ITEMS,
                                  sample_size_grid = BENCH_SAMPLE_SIZES,
                                  models           = BENCH_MODELS,
                                  iterations       = BENCH_ITERATIONS,
                                  write_csv         = TRUE) {
  if (!requireNamespace("bench", quietly = TRUE)) {
    stop("Package 'bench' is required. install.packages('bench')")
  }

  rows <- list()
  sweep_start <- Sys.time()

  for (model in models) {
    cat(sprintf("\n=== Model: %s ===\n", model))
    for (n_items in n_items_grid) {
      for (ss in sample_size_grid) {
        row <- run_cell(model, n_items, ss, iterations)
        rows[[length(rows) + 1L]] <- row
        # Crash-safe incremental write: refresh the CSV after every cell.
        if (isTRUE(write_csv)) {
          if (!dir.exists(BENCH_OUT_DIR)) {
            dir.create(BENCH_OUT_DIR, recursive = TRUE)
          }
          utils::write.csv(
            do.call(rbind, rows),
            file.path(BENCH_OUT_DIR, "scalability_results.csv"),
            row.names = FALSE
          )
        }
      }
    }
  }

  results <- do.call(rbind, rows)
  elapsed <- round(as.numeric(difftime(Sys.time(), sweep_start, units = "mins")), 1)

  cat(sprintf("\nSweep complete: %d cells in %.1f min.\n", nrow(results), elapsed))
  cat(sprintf("Results written to %s\n",
              file.path(BENCH_OUT_DIR, "scalability_results.csv")))

  attr(results, "sweep_minutes")  <- elapsed
  attr(results, "session_info")   <- utils::sessionInfo()
  attr(results, "run_timestamp")  <- format(sweep_start, "%Y-%m-%d %H:%M:%S")
  results
}


# --- Peak-memory probe (separate pass, gc-based) -----------------------------
# Runs the gc peak-heap measurement over a representative subset rather than the
# full grid: both models x all item lengths at the middle sample size, where the
# item-scaling signal is clearest. Best run in a FRESH R session (right after
# load_all) so the max-used watermark isn't pre-inflated by the timing sweep.
run_memory_probe <- function(n_items_grid     = BENCH_N_ITEMS,
                             sample_size       = 500L,
                             models            = BENCH_MODELS,
                             iterations        = BENCH_ITERATIONS,
                             write_csv          = TRUE) {
  rows <- list()
  for (model in models) {
    cat(sprintf("\n=== Peak memory: %s (N = %d) ===\n", model, sample_size))
    for (n_items in n_items_grid) {
      row <- measure_cell_peak_mem(model, n_items, sample_size, iterations)
      rows[[length(rows) + 1L]] <- row
      if (isTRUE(write_csv)) {
        if (!dir.exists(BENCH_OUT_DIR)) dir.create(BENCH_OUT_DIR, recursive = TRUE)
        utils::write.csv(
          do.call(rbind, rows),
          file.path(BENCH_OUT_DIR, "memory_probe.csv"),
          row.names = FALSE
        )
      }
    }
  }
  results <- do.call(rbind, rows)
  cat(sprintf("\nMemory probe complete: %d cells. Written to %s\n",
              nrow(results), file.path(BENCH_OUT_DIR, "memory_probe.csv")))
  results
}


# --- mirt time-fraction attribution ------------------------------------------
# Runs Rprof around one cell and reports the share of total sampled time spent
# inside mirt (mirt::mirt for fitting + mirt::simdata for generation). This
# confirms whether mirt dominates and where (fit vs. data generation).
# Set use_profvis = TRUE for an interactive flame graph instead (needs profvis).
profile_mirt_fraction <- function(model = "GRM", n_items = 100, sample_size = 500,
                                  iterations = BENCH_ITERATIONS,
                                  use_profvis = FALSE) {
  design <- build_design(model, n_items)
  study  <- irt_study(design = design, sample_sizes = sample_size)

  run_one <- function() {
    irt_simulate(study = study, iterations = iterations,
                 seed = BENCH_SEED, progress = FALSE, parallel = FALSE)
  }

  if (isTRUE(use_profvis)) {
    if (!requireNamespace("profvis", quietly = TRUE)) {
      stop("Package 'profvis' is required for use_profvis = TRUE.")
    }
    return(profvis::profvis(run_one()))
  }

  if (!dir.exists(BENCH_OUT_DIR)) dir.create(BENCH_OUT_DIR, recursive = TRUE)
  prof_file <- file.path(BENCH_OUT_DIR,
                         sprintf("Rprof_%s_%ditems_%dN.out", model, n_items, sample_size))

  Rprof(prof_file, interval = 0.01, memory.profiling = FALSE)
  invisible(run_one())
  Rprof(NULL)

  sm <- summaryRprof(prof_file)
  by_total <- sm$by.total
  total_time <- sm$sampling.time

  # mirt-attributable time: any frame in the mirt namespace. by.total rownames
  # are quoted function names like "\"mirt\"" / "\"simdata\"" / "mirt::mirt".
  fn_names  <- rownames(by_total)
  # Match actual mirt-namespace frames; exclude this wrapper, whose own name
  # contains "mirt" and would otherwise self-match at 100%.
  is_mirt   <- grepl("mirt|simdata|fscores", fn_names, ignore.case = TRUE) &
               !grepl("profile_mirt_fraction", fn_names)
  # Use the single highest-level mirt frame's total.time to avoid double-counting
  # nested mirt calls; fall back to 0 if none sampled.
  mirt_time <- if (any(is_mirt)) max(by_total$total.time[is_mirt]) else 0
  mirt_frac <- if (total_time > 0) mirt_time / total_time else NA_real_

  cat(sprintf("\nmirt time fraction for %s, %d items, N = %d (%d iters):\n",
              model, n_items, sample_size, iterations))
  cat(sprintf("  total sampled time : %.2fs\n", total_time))
  cat(sprintf("  mirt-attributable  : %.2fs (%.0f%%)\n", mirt_time, 100 * mirt_frac))
  cat("\nTop 15 frames by total time:\n")
  print(utils::head(by_total[order(-by_total$total.time), c("total.time", "total.pct",
                                                            "self.time", "self.pct")], 15))

  invisible(list(mirt_fraction = mirt_frac, total_time = total_time,
                 summary = sm, prof_file = prof_file))
}
