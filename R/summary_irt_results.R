# summary_irt_results.R
# S3 summary method for irt_results objects.
# Computes performance criteria (bias, MSE, RMSE, coverage, etc.)
# from raw per-iteration estimates stored by irt_simulate().


#' Summarize IRT Simulation Results
#'
#' Compute performance criteria for each sample size, item, and parameter
#' combination from an [irt_simulate()] result. Criteria follow
#' Morris et al. (2019) definitions. Optionally, users can provide a custom
#' callback function to compute additional item-level performance criteria
#' (e.g., conditional reliability, external criterion SE).
#'
#' @param object An `irt_results` object from [irt_simulate()].
#' @param criterion Optional character vector. Which criteria to include
#'   in the output. Valid values: `"bias"`, `"empirical_se"`, `"mse"`,
#'   `"rmse"`, `"coverage"`, `"mcse_bias"`, `"mcse_mse"`.
#'   If `NULL` (default), all criteria are returned.
#' @param param Optional character vector. Which parameter types to
#'   include (e.g., `"a"`, `"b"`, `"b1"`). If `NULL` (default), all
#'   parameters are summarized.
#' @param criterion_fn Optional function. A user-defined callback to compute
#'   custom performance criteria. Must accept named arguments
#'   `estimates` (numeric vector), `true_value` (scalar), `ci_lower` (numeric),
#'   `ci_upper` (numeric), `converged` (logical), and `...` (for future use).
#'   Must return a named numeric vector of length >= 1. The names become new
#'   columns in `item_summary`, appended after `n_converged`. If `NULL` (default),
#'   no custom criteria are computed.
#' @param ... Additional arguments (ignored).
#'
#' @return An S3 object of class `summary_irt_results` containing:
#'   \describe{
#'     \item{item_summary}{Data frame with one row per sample_size ×
#'       item × param combination, containing the requested criteria
#'       plus `n_converged` and any custom columns from `criterion_fn`.}
#'     \item{theta_summary}{Data frame with one row per sample_size,
#'       containing `mean_cor`, `sd_cor`, `mean_rmse`, `sd_rmse`,
#'       and `n_converged`.}
#'     \item{iterations}{Number of replications.}
#'     \item{seed}{Base seed used.}
#'     \item{model}{IRT model type.}
#'   }
#'
#' @references
#' Morris, T. P., White, I. R., & Crowther, M. J. (2019). Using simulation
#' studies to evaluate statistical methods. *Statistics in Medicine*, 38(11),
#' 2074--2102. \doi{10.1002/sim.8086}
#'
#' @examples
#' \dontrun{
#' results <- irt_simulate(study, iterations = 100, seed = 42)
#' s <- summary(results)
#' s$item_summary
#' s$theta_summary
#'
#' # Only bias and RMSE for difficulty parameters
#' summary(results, criterion = c("bias", "rmse"), param = "b")
#'
#' # Compute custom criterion: relative bias
#' custom_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
#'   valid_est <- estimates[!is.na(estimates)]
#'   rel_bias <- (mean(valid_est) - true_value) / true_value
#'   c(relative_bias = rel_bias)
#' }
#' summary(results, criterion_fn = custom_fn)
#'
#' # Multiple custom criteria
#' multi_fn <- function(estimates, true_value, ci_lower, ci_upper, converged, ...) {
#'   valid_est <- estimates[!is.na(estimates)]
#'   c(mean_est = mean(valid_est), sd_est = sd(valid_est))
#' }
#' summary(results, criterion_fn = multi_fn)
#' }
#'
#' @seealso [irt_simulate()] for running simulations,
#'   [plot.irt_results()] for visualization,
#'   [recommended_n()] for sample-size recommendations.
#' @importFrom cli cli_abort
#' @export
summary.irt_results <- function(object, criterion = NULL, param = NULL,
                                criterion_fn = NULL, ...) {

  # --- Validate input ---------------------------------------------------------
  if (!inherits(object, "irt_results")) {
    cli::cli_abort("{.arg object} must be an {.cls irt_results} object.")
  }

  # --- Validate criterion argument --------------------------------------------
  all_criteria <- valid_criteria()

  if (!is.null(criterion)) {
    bad <- setdiff(criterion, all_criteria)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "Invalid {.arg criterion}.",
        "i" = "Valid options: {.val {all_criteria}}",
        "x" = "You passed: {.val {bad}}"
      ))
    }
  }

  # --- Validate criterion_fn argument -----------------------------------------
  if (!is.null(criterion_fn) && !is.function(criterion_fn)) {
    cli::cli_abort("{.arg criterion_fn} must be a function or NULL.")
  }

  # --- Validate param argument ------------------------------------------------
  available_params <- unique(object$item_results$param)

  if (!is.null(param)) {
    bad <- setdiff(param, available_params)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "Invalid {.arg param}.",
        "i" = "Available: {.val {available_params}}",
        "x" = "You passed: {.val {bad}}"
      ))
    }
  }

  # --- Filter item_results by param if requested ------------------------------
  ir <- object$item_results
  if (!is.null(param)) {
    ir <- ir[ir$param %in% param, , drop = FALSE]
  }

  # --- Compute item-level criteria using split() + lapply() ------------------
  # Create a splitting key from sample_size, item, param. Use interaction()
  # to create a factor with lexicographic ordering.
  # Split will group rows by this key while preserving lex order.
  ir$.key <- interaction(ir$sample_size, ir$item, ir$param,
                         drop = TRUE, lex.order = TRUE)

  # Split returns a list where each element corresponds to a unique key value.
  # Keys are already sorted (lex.order = TRUE).
  chunks <- split(ir, ir$.key, drop = TRUE)

  # Track whether custom criterion function is provided
  has_custom <- !is.null(criterion_fn)

  # Compute criteria for each chunk
  result_rows <- lapply(chunks, function(subset) {
    true_val <- subset$true_value[1L]
    n_converged <- sum(subset$converged)
    ss <- subset$sample_size[1L]
    item <- subset$item[1L]
    prm <- subset$param[1L]

    # Pass CIs if available (not all-NA)
    has_ci <- any(!is.na(subset$ci_lower)) && any(!is.na(subset$ci_upper))
    if (has_ci) {
      crit <- compute_criterion(
        estimates  = subset$estimate,
        true_value = true_val,
        ci_lower   = subset$ci_lower,
        ci_upper   = subset$ci_upper
      )
    } else {
      crit <- compute_criterion(
        estimates  = subset$estimate,
        true_value = true_val
      )
    }

    # Build base row with built-in criteria
    row <- data.frame(
      sample_size  = ss,
      item         = item,
      param        = prm,
      true_value   = true_val,
      bias         = crit$bias,
      empirical_se = crit$empirical_se,
      mse          = crit$mse,
      rmse         = crit$rmse,
      coverage     = if (is.null(crit$coverage)) NA_real_ else crit$coverage,
      mcse_bias    = crit$mcse_bias,
      mcse_mse     = crit$mcse_mse,
      n_converged  = n_converged,
      stringsAsFactors = FALSE
    )

    # Invoke criterion_fn if provided
    if (has_custom) {
      user_values <- tryCatch(
        criterion_fn(
          estimates  = subset$estimate,
          true_value = true_val,
          ci_lower   = subset$ci_lower,
          ci_upper   = subset$ci_upper,
          converged  = subset$converged
        ),
        error = function(e) {
          cli::cli_abort(c(
            "criterion_fn failed.",
            "i" = "At sample_size={.val {ss}}, item={.val {item}}, param={.val {prm}}",
            "x" = conditionMessage(e)
          ))
        }
      )

      # Validate return shape
      if (!is.numeric(user_values)) {
        cli::cli_abort(c(
          "criterion_fn must return a numeric vector.",
          "x" = "Got {.cls {typeof(user_values)}} at sample_size={.val {ss}}, item={.val {item}}, param={.val {prm}}"
        ))
      }

      if (is.null(names(user_values)) || any(names(user_values) == "")) {
        cli::cli_abort(c(
          "criterion_fn must return a named vector (all names non-empty).",
          "x" = "At sample_size={.val {ss}}, item={.val {item}}, param={.val {prm}}"
        ))
      }

      # Store custom values as an attribute on the row (for later cross-cell validation)
      attr(row, "custom_values") <- user_values
    }

    row
  })

  # --- Validate cross-cell name consistency and bind custom columns -----------
  custom_names_ref <- NULL
  if (has_custom) {
    # Extract reference name set from first row's custom_values attribute
    custom_names_ref <- names(attr(result_rows[[1]], "custom_values"))

    # Walk through result_rows to validate consistency and bind custom columns
    for (i in seq_along(result_rows)) {
      custom_vals <- attr(result_rows[[i]], "custom_values")
      current_names <- names(custom_vals)

      # Check consistency with reference
      if (!identical(current_names, custom_names_ref)) {
        cli::cli_abort(c(
          "criterion_fn returned inconsistent names across cells.",
          "i" = "Expected: {.val {custom_names_ref}}",
          "x" = "Got: {.val {current_names}} at sample_size={.val {result_rows[[i]]$sample_size[1L]}}, item={.val {result_rows[[i]]$item[1L]}}, param={.val {result_rows[[i]]$param[1L]}}"
        ))
      }

      # Bind custom columns to the row
      for (nm in custom_names_ref) {
        result_rows[[i]][[nm]] <- custom_vals[[nm]]
      }

      # Strip the attribute before rbind to avoid data leakage
      attr(result_rows[[i]], "custom_values") <- NULL
    }
  }

  item_summary <- do.call(rbind, result_rows)
  rownames(item_summary) <- NULL

  # Remove the .key column (it was only for splitting)
  # Since we used lapply, .key is not in item_summary, but we drop ir$.key to keep ir clean
  ir$.key <- NULL

  # --- Apply criterion filter (column subsetting) -----------------------------
  if (!is.null(criterion)) {
    # Keep built-in columns and the requested criteria
    keep_cols <- c("sample_size", "item", "param", "true_value",
                   criterion, "n_converged")
    # If criterion_fn was used, also keep custom columns
    if (has_custom && !is.null(custom_names_ref)) {
      keep_cols <- c(keep_cols, custom_names_ref)
    }
    item_summary <- item_summary[, keep_cols, drop = FALSE]
  }

  # --- Compute theta-level summary using split() + lapply() ------------------
  tr <- object$theta_results

  # Create splitting key: just sample_size (theta summary is per sample_size)
  tr$.key <- as.factor(tr$sample_size)
  chunks_theta <- split(tr, tr$.key, drop = TRUE)

  # Compute theta summary for each sample_size
  theta_rows <- lapply(chunks_theta, function(subset) {
    ss <- subset$sample_size[1L]
    # Filter to converged iterations
    subset_conv <- subset[subset$converged, , drop = FALSE]

    data.frame(
      sample_size = ss,
      mean_cor    = mean(subset_conv$theta_cor, na.rm = TRUE),
      sd_cor      = sd(subset_conv$theta_cor, na.rm = TRUE),
      mean_rmse   = mean(subset_conv$theta_rmse, na.rm = TRUE),
      sd_rmse     = sd(subset_conv$theta_rmse, na.rm = TRUE),
      n_converged = nrow(subset_conv),
      stringsAsFactors = FALSE
    )
  })

  theta_summary <- do.call(rbind, theta_rows)
  rownames(theta_summary) <- NULL

  # Sort theta_summary by sample_size (ascending)
  theta_summary <- theta_summary[order(theta_summary$sample_size), ,
                                 drop = FALSE]
  rownames(theta_summary) <- NULL

  # Clean up temporary key column
  tr$.key <- NULL

  # --- Assemble result --------------------------------------------------------
  structure(
    list(
      item_summary  = item_summary,
      theta_summary = theta_summary,
      iterations    = object$iterations,
      seed          = object$seed,
      model         = object$study$design$model
    ),
    class = "summary_irt_results"
  )
}


#' Print Summary of IRT Simulation Results
#'
#' Display item parameter criteria and theta recovery statistics from a
#' [summary.irt_results()] object.
#'
#' @param x A `summary_irt_results` object.
#' @param ... Additional arguments (ignored).
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \dontrun{
#' results <- irt_simulate(study, iterations = 100, seed = 42)
#' s <- summary(results)
#' print(s)
#' }
#'
#' @seealso [summary.irt_results()]
#' @export
print.summary_irt_results <- function(x, ...) {

  cat("IRT Simulation Summary\n")
  cat("  Model:      ", x$model, "\n")
  cat("  Iterations: ", x$iterations, "\n")
  cat("  Seed:       ", x$seed, "\n\n")

  # Item parameter summary
  cat("Item Parameter Criteria:\n")
  print(x$item_summary, row.names = FALSE)

  # Theta recovery summary
  cat("\nTheta Recovery:\n")
  print(x$theta_summary, row.names = FALSE)

  invisible(x)
}
