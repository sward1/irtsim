# irt_simulate.R
# Core simulation engine: runs the Monte Carlo loop over an irt_study
# specification and returns raw per-iteration parameter estimates.
#
# Architecture (Approach A): irt_simulate() stores raw per-iteration
# estimates. Criterion computation happens at summary() time, not here.
# This follows simhelpers / SimDesign / Morris et al. (2019) conventions.


#' Run an IRT Monte Carlo Simulation
#'
#' Execute a Monte Carlo simulation study based on an [irt_study]
#' specification. For each iteration and sample size, data are generated,
#' missing values applied, the IRT model is fitted, and parameter estimates
#' are extracted and stored.
#'
#' The returned `irt_results` object stores raw per-iteration estimates.
#' Use [summary.irt_results()] to compute performance criteria (bias, MSE,
#' RMSE, coverage, etc.) and [plot.irt_results()] to visualize results.
#'
#' @param study An [irt_study] object specifying the design and study
#'   conditions.
#' @param iterations Positive integer. Number of Monte Carlo replications.
#' @param seed Integer. Base random seed for reproducibility. Each iteration
#'   uses `seed + iteration - 1`.
#' @param progress Logical. Print progress messages? Default `TRUE`.
#' @param parallel Logical. Run iterations in parallel using
#'   [future.apply::future_lapply()]? Default `FALSE`. Requires users to set
#'   up a future plan (e.g., `future::plan(multisession)`) before calling.
#'   See Details.
#' @param se Logical. Compute standard errors and confidence intervals for item
#'   parameter estimates? Default `TRUE`. Set to `FALSE` for significant speed
#'   improvement when only point estimates are needed (e.g., MSE, bias, RMSE
#'   criteria). When `FALSE`, `se`/`ci_lower`/`ci_upper` columns in
#'   `item_results` are `NA`.
#' @param compute_theta Logical. Compute EAP theta estimates and recovery metrics
#'   (correlation, RMSE)? Default `TRUE`. Set to `FALSE` to skip the
#'   [mirt::fscores()] call when theta recovery is not needed. When `FALSE`,
#'   `theta_cor` and `theta_rmse` in `theta_results` are `NA` (but `converged`
#'   is still tracked).
#'
#' @details
#'
#' ## Parallelization
#'
#' When `parallel = TRUE`, the Monte Carlo loop over iterations is
#' parallelized via [future.apply::future_lapply()]. Each parallel task
#' processes one iteration across all sample sizes sequentially.
#'
#' **Important:** This function does NOT configure a future plan. Users must
#' set their own plan before calling with `parallel = TRUE`:
#'
#' ```r
#' library(future)
#' plan(multisession, workers = 4)  # or your preferred backend
#' results <- irt_simulate(study, iterations = 100, seed = 42, parallel = TRUE)
#' ```
#'
#' Without an explicit plan, future defaults to sequential execution
#' (no parallelism).
#'
#' ## Reproducibility contract
#'
#' Reproducibility is guaranteed **within a given dispatch mode**, not across
#' modes:
#'
#' - **Serial mode** (`parallel = FALSE`) uses deterministic per-cell seeds
#'   under the session's default RNG kind (Mersenne-Twister). Re-running with
#'   the same base `seed` reproduces identical results bit-for-bit.
#' - **Parallel mode** (`parallel = TRUE`) delegates RNG management to
#'   `future.apply::future_lapply(..., future.seed = TRUE)`, which assigns
#'   each iteration a formally independent L'Ecuyer-CMRG substream. Re-running
#'   with the same base `seed` reproduces identical results bit-for-bit across
#'   parallel runs, including across different worker counts.
#' - **Across modes**, numerical results will differ because the two paths
#'   use different RNG algorithms and different seeding strategies. Both are
#'   statistically valid; the parallel path has the stronger formal guarantee
#'   of independent substreams, which is the standard for Monte Carlo work.
#'
#' Progress messages are suppressed in parallel mode (workers cannot stream to
#' stdout safely). Set `progress = FALSE` in serial mode to suppress messages
#' (they appear every 10% of iterations).
#'
#' @return An S3 object of class `irt_results` containing:
#'   \describe{
#'     \item{item_results}{Data frame with per-iteration item parameter
#'       estimates (columns: iteration, sample_size, item, param, true_value,
#'       estimate, se, ci_lower, ci_upper, converged).}
#'     \item{theta_results}{Data frame with per-iteration theta recovery
#'       summaries (columns: iteration, sample_size, theta_cor, theta_rmse,
#'       converged).}
#'     \item{study}{The original [irt_study] object.}
#'     \item{iterations}{Number of replications run.}
#'     \item{seed}{Base seed used.}
#'     \item{elapsed}{Elapsed wall-clock time in seconds.}
#'     \item{se}{Logical flag indicating whether SEs and CIs were computed.}
#'     \item{compute_theta}{Logical flag indicating whether theta recovery
#'       metrics were computed.}
#'   }
#'
#' @examples
#' \donttest{
#' # Minimal example (iterations and sample sizes reduced for speed;
#' # use iterations >= 100 and 3+ sample sizes in practice)
#' design <- irt_design(
#'   model = "1PL", n_items = 5,
#'   item_params = list(b = seq(-2, 2, length.out = 5))
#' )
#' study <- irt_study(design, sample_sizes = c(200, 500))
#' results <- irt_simulate(study, iterations = 10, seed = 42)
#' summary(results)
#' plot(results)
#' }
#'
#' @seealso [irt_study()] for specifying study conditions,
#'   [summary.irt_results()] and [plot.irt_results()] for analyzing output,
#'   [irt_iterations()] for determining the number of replications.
#' @importFrom future.apply future_lapply
#' @importFrom cli cli_progress_bar cli_progress_update cli_abort
#' @export
irt_simulate <- function(study, iterations, seed, progress = TRUE,
                         parallel = FALSE, se = TRUE, compute_theta = TRUE) {

  # --- Validate inputs ---------------------------------------------------------
  if (!inherits(study, "irt_study")) {
    cli::cli_abort("{.arg study} must be an {.cls irt_study} object.")
  }

  if (missing(seed)) {
    cli::cli_abort("{.arg seed} is required for reproducibility.")
  }

  if (!is.numeric(iterations) || length(iterations) != 1L) {
    cli::cli_abort("{.arg iterations} must be a single positive integer.")
  }
  if (iterations != floor(iterations)) {
    cli::cli_abort("{.arg iterations} must be a whole integer value.")
  }
  iterations <- as.integer(iterations)
  if (iterations < 1L) {
    cli::cli_abort("{.arg iterations} must be a positive integer.")
  }

  seed <- as.integer(seed)

  # Validate parallel parameter
  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    cli::cli_abort("{.arg parallel} must be a single logical value (TRUE or FALSE).")
  }

  # Validate se parameter
  if (!is.logical(se) || length(se) != 1L || is.na(se)) {
    cli::cli_abort("{.arg se} must be a single logical value (TRUE or FALSE).")
  }

  # Validate compute_theta parameter
  if (!is.logical(compute_theta) || length(compute_theta) != 1L || is.na(compute_theta)) {
    cli::cli_abort("{.arg compute_theta} must be a single logical value (TRUE or FALSE).")
  }

  # --- Extract design info -----------------------------------------------------
  design <- study$design
  estimation_model <- study$estimation_model
  n_items <- design$n_items
  sample_sizes <- study$sample_sizes
  n_ss <- length(sample_sizes)

  # Pre-compute true parameter vectors (adjusted for estimation model)
  # and build a pre-indexed lookup. The lookup maps from "<item>_<param>"
  # -> true_value for O(1) access in extract_params() instead of repeated
  # vector scans. Using estimation_model schema ensures rows match what
  # fit_model will extract.
  true_params <- build_true_params_for_estimation(design, estimation_model)
  true_params_lookup <- stats::setNames(
    true_params$true_value,
    paste0(true_params$item, "_", true_params$param)
  )

  # --- Simulation loop ---------------------------------------------------------
  start_time <- proc.time()["elapsed"]

  # Refactor iteration body into closure over shared state (design, study, etc.).
  # The `use_cell_seed` flag controls the reproducibility strategy:
  #   - TRUE  (serial path): derive a deterministic per-cell seed and call
  #                          set.seed() before each step. Reproducible across
  #                          runs under the session's default RNG kind.
  #   - FALSE (parallel path): no explicit per-cell seeding. Random draws consume
  #                            the current RNG state, which future.apply manages
  #                            as an L'Ecuyer-CMRG substream (future.seed = TRUE).
  #                            Reproducible across parallel runs with the same
  #                            base seed, but not bit-identical to serial output.
  run_one_iteration <- function(iter, use_cell_seed = TRUE) {
    iter_seed <- seed + iter - 1L

    iter_items <- vector("list", n_ss)
    iter_thetas <- vector("list", n_ss)

    for (ss_idx in seq_along(sample_sizes)) {
      n <- sample_sizes[ss_idx]

      if (use_cell_seed) {
        # Derive per-cell seed: unique for every (base_seed, iter, ss_idx) triple.
        # Uses a hash-style combination to avoid overlapping sequences across
        # different base seeds.
        cell_seed <- as.integer(((seed * 31L + iter) * 131L + ss_idx) %% .Machine$integer.max)
        set.seed(cell_seed)
        data_seed <- cell_seed
        missing_seed <- cell_seed + .Machine$integer.max %/% 2L
      } else {
        # Parallel path: let the current (L'Ecuyer-CMRG) substream drive draws.
        data_seed <- NULL
        missing_seed <- NULL
      }

      # 1. Generate data
      theta_true <- generate_theta(design$theta_dist, n)
      dat <- generate_data(design, n = n, seed = data_seed,
                           theta = theta_true)

      # 2. Apply missing data
      dat <- apply_missing(dat, study,
                           seed = missing_seed,
                           theta = theta_true)

      # 3. Fit model (use estimation_model, not generation model)
      fit_result <- fit_model(dat, estimation_model, se = se)

      # 4. Extract parameters and theta
      if (fit_result$converged) {
        item_df <- extract_params(fit_result$model, design, estimation_model,
                                   iter, n, true_params, true_params_lookup, se = se)
        if (compute_theta) {
          theta_summary <- extract_theta_summary(
            fit_result$model, theta_true, iter, n
          )
        } else {
          theta_summary <- data.frame(
            iteration   = iter,
            sample_size = n,
            theta_cor   = NA_real_,
            theta_rmse  = NA_real_,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )
        }
      } else {
        item_df <- build_na_item_results(true_params, iter, n)
        theta_summary <- data.frame(
          iteration   = iter,
          sample_size = n,
          theta_cor   = NA_real_,
          theta_rmse  = NA_real_,
          converged   = FALSE,
          stringsAsFactors = FALSE
        )
      }

      iter_items[[ss_idx]] <- item_df
      iter_thetas[[ss_idx]] <- theta_summary
    }

    # Return results for this iteration
    list(items = iter_items, thetas = iter_thetas)
  }

  # Dispatch: serial vs. parallel
  if (parallel) {
    # Parallel mode: future.apply manages per-iteration RNG substreams via
    # L'Ecuyer-CMRG (future.seed = TRUE). We disable explicit per-cell seeding
    # so the workers consume the substreams future.apply assigns them. This
    # gives formally independent substreams across iterations and
    # reproducibility within parallel mode for a given seed, but numeric
    # results will differ from the serial path (different RNG, different
    # seeding strategy).
    #
    # IMPORTANT: future.seed = TRUE derives the initial L'Ecuyer-CMRG seed
    # from the current .Random.seed. Without an explicit set.seed() call
    # beforehand, repeated invocations of irt_simulate() would start from
    # different .Random.seed states (session-dependent) and produce
    # different substreams even with the same `seed` argument. We therefore
    # anchor .Random.seed here so that same base seed → same substreams →
    # bit-identical results across parallel re-runs. This is consistent
    # with the serial path, which also writes to the caller's RNG state
    # (via per-cell set.seed). Both modes leak RNG state to the session;
    # neither mode claims to preserve caller state.
    set.seed(seed)
    iteration_results <- future.apply::future_lapply(
      seq_len(iterations),
      run_one_iteration,
      use_cell_seed = FALSE,
      future.seed = TRUE
    )
  } else {
    # Serial mode: use for loop with optional progress bar. Explicit
    # per-cell seeding preserves bit-identical reproducibility under the
    # session's default RNG kind (Mersenne-Twister) and matches the
    # pre-parallel behavior of the package.
    iteration_results <- vector("list", iterations)

    if (progress) {
      pb <- cli::cli_progress_bar(
        format = "Iteration {cli::pb_current}/{iterations}",
        total = iterations,
        clear = FALSE
      )
    }

    for (iter in seq_len(iterations)) {
      iteration_results[[iter]] <- run_one_iteration(iter, use_cell_seed = TRUE)
      if (progress) {
        cli::cli_progress_update(id = pb)
      }
    }
  }

  elapsed <- as.numeric(proc.time()["elapsed"] - start_time)

  # --- Flatten and assemble results --------------------------------------------
  # Each element of iteration_results is a list with items and thetas.
  # Flatten: extract all items lists, then all thetas lists
  item_results_list <- unlist(
    lapply(iteration_results, `[[`, "items"),
    recursive = FALSE
  )
  theta_results_list <- unlist(
    lapply(iteration_results, `[[`, "thetas"),
    recursive = FALSE
  )

  item_results <- do.call(rbind, item_results_list)
  theta_results <- do.call(rbind, theta_results_list)
  rownames(item_results) <- NULL
  rownames(theta_results) <- NULL

  structure(
    list(
      item_results  = item_results,
      theta_results = theta_results,
      study         = study,
      iterations    = iterations,
      seed          = seed,
      elapsed       = elapsed,
      se            = se,
      compute_theta = compute_theta
    ),
    class = "irt_results"
  )
}


# =============================================================================
# Internal Helpers
# =============================================================================


#' Build True Parameter Data Frame (Internal)
#'
#' Creates a data frame of true parameter values from the design,
#' used for populating the true_value column and for building NA rows
#' on non-convergence.
#'
#' @param design An `irt_design` object.
#' @return Data frame with columns: item, param, true_value.
#' @keywords internal
build_true_params <- function(design) {
  model_config <- get_model_config(design$model)
  model_config$build_true_params(design)
}


#' Build True Parameter Data Frame for Estimation Model (Internal)
#'
#' Creates a data frame of true parameter values adjusted for the estimation
#' model, accounting for misspecification. When generation and estimation
#' models differ, some parameters may need to be filled with defaults
#' (e.g., discrimination = 1 for Rasch when estimating 1PL from 2PL data)
#' or dropped entirely (e.g., discrimination when estimating 1PL from 2PL).
#'
#' @param design An `irt_design` object specifying the generation model.
#' @param estimation_model Character string: "1PL", "2PL", or "GRM".
#' @return Data frame with columns: item, param, true_value, matching the
#'   schema of the estimation model.
#' @keywords internal
build_true_params_for_estimation <- function(design, estimation_model) {
  gen_model <- design$model
  est_config <- get_model_config(estimation_model)
  n_items <- design$n_items

  # If estimation model matches generation model, return standard true params
  if (gen_model == estimation_model) {
    return(build_true_params(design))
  }

  # --- Cross-fit cases (already validated for compatibility in irt_study) ----

  # Case 1: Generation 1PL, Estimation 2PL
  # Add discrimination = 1 for all items (Rasch constraint)
  if (gen_model == "1PL" && estimation_model == "2PL") {
    b <- design$item_params$b
    rows <- list()
    rows[[1]] <- data.frame(
      item       = seq_len(n_items),
      param      = "a",
      true_value = rep(1.0, n_items),  # Rasch: a fixed at 1
      stringsAsFactors = FALSE
    )
    rows[[2]] <- data.frame(
      item       = seq_len(n_items),
      param      = "b",
      true_value = b,
      stringsAsFactors = FALSE
    )
    return(do.call(rbind, rows))
  }

  # Case 2: Generation 2PL, Estimation 1PL
  # Drop discrimination, keep only difficulty
  if (gen_model == "2PL" && estimation_model == "1PL") {
    b <- design$item_params$b
    return(data.frame(
      item       = seq_len(n_items),
      param      = "b",
      true_value = b,
      stringsAsFactors = FALSE
    ))
  }

  # If no explicit cross-fit case matched but models are compatible,
  # fall back to estimation model's standard build_true_params.
  # This handles same-model cases and any future cross-fits.
  est_config$build_true_params(design)
}



#' Build NA Item Results for Non-Converged Iterations (Internal)
#'
#' @param true_params Data frame from `build_true_params()`.
#' @param iteration Integer iteration number.
#' @param sample_size Integer sample size.
#' @return Data frame matching item_results schema with NA estimates.
#' @keywords internal
build_na_item_results <- function(true_params, iteration, sample_size) {
  n <- nrow(true_params)
  data.frame(
    iteration   = rep(iteration, n),
    sample_size = rep(sample_size, n),
    item        = true_params$item,
    param       = true_params$param,
    true_value  = true_params$true_value,
    estimate    = rep(NA_real_, n),
    se          = rep(NA_real_, n),
    ci_lower    = rep(NA_real_, n),
    ci_upper    = rep(NA_real_, n),
    converged   = rep(FALSE, n),
    stringsAsFactors = FALSE
  )
}


#' Fit an IRT Model (Internal)
#'
#' Wraps [mirt::mirt()] with error and convergence handling.
#'
#' @param data Numeric matrix of response data (may contain NAs).
#' @param model Character string: "1PL", "2PL", or "GRM".
#' @param se Logical. Compute standard errors? Default `TRUE`.
#' @return A list with elements `model` (fitted mirt object or NULL)
#'   and `converged` (logical).
#' @keywords internal
fit_model <- function(data, model, se = TRUE) {
  # Map our model names to mirt itemtype via the registry
  model_config <- get_model_config(model)
  itemtype <- model_config$mirt_itemtype

  # Name columns so mirt::coef() returns predictable item names
  df <- as.data.frame(data)
  colnames(df) <- paste0("Item_", seq_len(ncol(df)))

  result <- tryCatch(
    {
      # Suppress mirt's convergence warnings and EM-termination messages.
      # mirt emits "EM cycles terminated after 500 iterations" via message(),
      # which escapes suppressWarnings(). capture.output() catches any stray
      # cat() output that verbose=FALSE doesn't fully suppress.
      if (se) {
        utils::capture.output(
          mod <- suppressMessages(suppressWarnings(mirt::mirt(
            data = df,
            model = 1,
            itemtype = itemtype,
            verbose = FALSE,
            SE = TRUE,
            SE.type = "Oakes"
          ))),
          type = "output"
        )
      } else {
        utils::capture.output(
          mod <- suppressMessages(suppressWarnings(mirt::mirt(
            data = df,
            model = 1,
            itemtype = itemtype,
            verbose = FALSE,
            SE = FALSE
          ))),
          type = "output"
        )
      }

      # Check convergence via mirt's internal flag
      converged <- mirt::extract.mirt(mod, "converged")

      list(model = mod, converged = converged)
    },
    error = function(e) {
      list(model = NULL, converged = FALSE)
    }
  )

  result
}


#' Extract Item Parameter Estimates from a Fitted mirt Model (Internal)
#'
#' Pulls point estimates, SEs, and CIs from a fitted mirt object and
#' returns them in long format matching the item_results schema.
#'
#' Uses the list-based `coef(mod)` output (one matrix per item, keyed by
#' item name). Default `coef()` returns rows `par, CI_2.5, CI_97.5`;
#' SE is derived from CI width: `SE = (upper - lower) / (2 * z_0.975)`.
#'
#' @param mod A fitted mirt object.
#' @param design An `irt_design` object (for true values and model type).
#' @param estimation_model Character string: "1PL", "2PL", or "GRM" (the
#'   model that was fitted, which may differ from design$model).
#' @param iteration Integer iteration number.
#' @param sample_size Integer sample size.
#' @param true_params Data frame (used for schema).
#' @param true_params_lookup Named character vector mapping keys of the form
#'   `"item_param"` (e.g., `"Item_1_a"`) to `true_value` (for O(1) lookup
#'   instead of repeated vector scans).
#' @param se Logical. Extract standard errors and CIs? Default `TRUE`.
#' @return Data frame with item_results columns.
#' @keywords internal
extract_params <- function(mod, design, estimation_model, iteration,
                           sample_size, true_params, true_params_lookup, se = TRUE) {
  est_config <- get_model_config(estimation_model)
  est_config$extract_params(mod, design, iteration, sample_size,
                            true_params, true_params_lookup, se = se)
}


#' Extract Theta Recovery Summary from a Fitted mirt Model (Internal)
#'
#' Computes EAP theta estimates and summarizes recovery via correlation
#' and RMSE against true theta.
#'
#' @param mod A fitted mirt object.
#' @param theta_true Numeric vector of true theta values.
#' @param iteration Integer iteration number.
#' @param sample_size Integer sample size.
#' @return Single-row data frame with theta_results columns.
#' @keywords internal
extract_theta_summary <- function(mod, theta_true, iteration, sample_size) {
  theta_hat <- as.numeric(suppressWarnings(
    mirt::fscores(mod, method = "EAP", full.scores = TRUE)
  ))

  # Handle any NAs in theta estimates
  valid <- !is.na(theta_hat) & !is.na(theta_true)

  if (sum(valid) > 2L) {
    theta_cor <- stats::cor(theta_true[valid], theta_hat[valid])
    theta_rmse <- sqrt(mean((theta_true[valid] - theta_hat[valid])^2))
  } else {
    theta_cor <- NA_real_
    theta_rmse <- NA_real_
  }

  data.frame(
    iteration   = iteration,
    sample_size = sample_size,
    theta_cor   = theta_cor,
    theta_rmse  = theta_rmse,
    converged   = TRUE,
    stringsAsFactors = FALSE
  )
}


#' Print an IRT Simulation Result
#'
#' Display a compact summary of an [irt_simulate()] result, including model,
#' items, sample sizes, iterations, convergence rate, and elapsed time.
#'
#' @param x An `irt_results` object.
#' @param ... Additional arguments (ignored).
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' design <- irt_design(
#'   model = "1PL", n_items = 5,
#'   item_params = list(b = seq(-2, 2, length.out = 5))
#' )
#' study <- irt_study(design, sample_sizes = c(200, 500))
#' results <- irt_simulate(study, iterations = 10, seed = 42)
#' print(results)
#' }
#'
#' @seealso [irt_simulate()]
#' @export
print.irt_results <- function(x, ...) {
  cat("IRT Simulation Results\n")

  # Show generation and estimation models if they differ, else show single Model line
  if (x$study$estimation_model != x$study$design$model) {
    cat("  Gen model:    ", x$study$design$model, "\n")
    cat("  Est model:    ", x$study$estimation_model, "\n")
  } else {
    cat("  Model:        ", x$study$design$model, "\n")
  }

  cat("  Items:        ", x$study$design$n_items, "\n")
  cat("  Sample sizes: ", paste(x$study$sample_sizes, collapse = ", "), "\n")
  cat("  Iterations:   ", x$iterations, "\n")

  # Convergence rate
  n_total <- nrow(x$theta_results)
  n_converged <- sum(x$theta_results$converged)
  conv_rate <- round(100 * n_converged / n_total, 1)
  cat("  Convergence:  ", n_converged, "/", n_total,
      " (", conv_rate, "%)\n", sep = "")

  cat("  Elapsed:      ", round(x$elapsed, 1), "s\n")
  cat("  Seed:         ", x$seed, "\n")

  invisible(x)
}
