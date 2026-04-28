# recommended_n.R
# S3 generic and method for extracting the minimum sample size
# that meets a user-specified criterion threshold.
#
# Dispatches on summary_irt_results objects. Standalone extractor
# (not a field on the summary) so users can query multiple thresholds
# against the same summary without recomputing.


#' Find the Minimum Sample Size Meeting a Criterion Threshold
#'
#' Given a [summary.irt_results()] object, find the smallest sample size
#' at which a performance criterion meets the specified threshold for
#' each item and parameter combination.
#'
#' For criteria where smaller is better (bias, empirical_se, mse, rmse,
#' mcse_bias, mcse_mse), the threshold is met when the criterion value
#' is at or below the threshold. For bias, the absolute value is used.
#' For coverage (where higher is better), the threshold is met when
#' coverage is at or above the threshold.
#'
#' @param object A `summary_irt_results` object from [summary.irt_results()].
#' @param criterion Character string. Which criterion to evaluate.
#'   One of: `"bias"`, `"empirical_se"`, `"mse"`, `"rmse"`, `"coverage"`,
#'   `"mcse_bias"`, `"mcse_mse"`.
#' @param threshold Positive numeric. The threshold value the criterion
#'   must meet.
#' @param param Optional character vector. Filter to specific parameter
#'   types (e.g., `"a"`, `"b"`, `"b1"`).
#' @param item Optional integer vector. Filter to specific item numbers.
#' @param aggregate Character. How to roll the per-item recommended sample
#'   sizes up into a single recommendation. One of `"max"` (default — the
#'   smallest N that powers every item/param), `"mean"`, `"median"`, or
#'   `"none"` (return the per-item data frame unchanged). `"mean"` and
#'   `"median"` round up via `ceiling()` so the recommendation is never
#'   under the computed central tendency.
#' @param ... Additional arguments (ignored).
#'
#' @return When `aggregate = "none"`, a data frame with columns:
#'   \describe{
#'     \item{item}{Item number.}
#'     \item{param}{Parameter name.}
#'     \item{recommended_n}{Minimum sample size meeting the threshold,
#'       or `NA` if no tested sample size meets it.}
#'     \item{criterion}{The criterion used (echoed back for reference).}
#'     \item{threshold}{The threshold used (echoed back for reference).}
#'   }
#'   When `aggregate` is `"max"`, `"mean"`, or `"median"` (the typical
#'   case), an integer scalar carrying the recommended sample size with
#'   attributes `details` (the per-item data frame above), `aggregate`,
#'   `criterion`, and `threshold`. If any item/param combination fails to
#'   meet the threshold at every tested sample size, the aggregate is
#'   `NA_integer_` and a warning lists the affected combinations.
#'
#' @examples
#' \donttest{
#' design <- irt_design(
#'   model = "1PL", n_items = 5,
#'   item_params = list(b = seq(-2, 2, length.out = 5))
#' )
#' study <- irt_study(design, sample_sizes = c(200, 500))
#' results <- irt_simulate(study, iterations = 10, seed = 42)
#' s <- summary(results)
#'
#' # Default — single recommended N (max across items) for RMSE <= 0.20
#' n_rec <- recommended_n(s, criterion = "rmse", threshold = 0.20)
#' n_rec
#' attr(n_rec, "details")  # per-item breakdown
#'
#' # Mean / median aggregates (rounded up via ceiling)
#' recommended_n(s, criterion = "rmse", threshold = 0.20, aggregate = "mean")
#'
#' # Legacy behavior — full per-item data frame
#' recommended_n(s, criterion = "rmse", threshold = 0.20, aggregate = "none")
#'
#' # Minimum N for 95% coverage on difficulty parameters only
#' recommended_n(s, criterion = "coverage", threshold = 0.95, param = "b")
#' }
#'
#' @seealso [summary.irt_results()] for computing criteria,
#'   [plot.irt_results()] for visualization.
#' @export
recommended_n <- function(object, ...) {
  UseMethod("recommended_n")
}


#' @rdname recommended_n
#' @export
recommended_n.summary_irt_results <- function(object, criterion, threshold,
                                               param = NULL, item = NULL,
                                               aggregate = c("max", "mean",
                                                             "median", "none"),
                                               ...) {


  # --- Validate input ---
  if (!inherits(object, "summary_irt_results")) {
    stop("`object` must be a `summary_irt_results` object.", call. = FALSE)
  }

  if (missing(criterion)) {
    stop("`criterion` is required.", call. = FALSE)
  }

  if (missing(threshold)) {
    stop("`threshold` is required.", call. = FALSE)
  }

  all_criteria <- valid_criteria()

  if (length(criterion) != 1L || !criterion %in% all_criteria) {
    stop("Invalid criterion: '", criterion, "'. Valid options: ",
         paste(all_criteria, collapse = ", "), call. = FALSE)
  }

  if (!criterion %in% names(object$item_summary)) {
    stop("Criterion '", criterion, "' not found in summary. ",
         "Available columns: ",
         paste(names(object$item_summary), collapse = ", "), call. = FALSE)
  }

  if (!is.numeric(threshold) || length(threshold) != 1L || threshold <= 0) {
    stop("`threshold` must be a single positive number.", call. = FALSE)
  }

  # --- Validate aggregate ---
  valid_agg <- c("max", "mean", "median", "none")
  aggregate <- tryCatch(
    match.arg(aggregate, choices = valid_agg),
    error = function(e) {
      cli::cli_abort(
        "{.arg aggregate} must be one of {.val {valid_agg}}, not {.val {aggregate}}.",
        call = NULL
      )
    }
  )

  # --- Filter by param ---
  is_df <- object$item_summary

  if (!is.null(param)) {
    available_params <- unique(is_df$param)
    bad <- setdiff(param, available_params)
    if (length(bad) > 0L) {
      stop("Invalid param: ", paste(bad, collapse = ", "),
           ". Available: ", paste(available_params, collapse = ", "),
           call. = FALSE)
    }
    is_df <- is_df[is_df$param %in% param, , drop = FALSE]
  }

  # --- Filter by item ---
  if (!is.null(item)) {
    is_df <- is_df[is_df$item %in% item, , drop = FALSE]
  }

  # --- Determine direction and absolute-value handling ---
  cfg <- get_criterion_config(criterion)
  higher_is_better <- cfg$direction == "higher_is_better"
  use_abs <- cfg$use_abs

  # --- Find minimum qualifying N per item × param ---
  combos <- unique(is_df[, c("item", "param"), drop = FALSE])
  combos <- combos[order(combos$item, combos$param), , drop = FALSE]
  rownames(combos) <- NULL

  rec_n <- rep(NA_integer_, nrow(combos))

  for (r in seq_len(nrow(combos))) {
    mask <- is_df$item == combos$item[r] & is_df$param == combos$param[r]
    subset <- is_df[mask, , drop = FALSE]
    subset <- subset[order(subset$sample_size), , drop = FALSE]

    vals <- subset[[criterion]]

    if (use_abs) {
      vals <- abs(vals)
    }

    if (higher_is_better) {
      meets <- !is.na(vals) & vals >= threshold
    } else {
      meets <- !is.na(vals) & vals <= threshold
    }

    if (any(meets)) {
      rec_n[r] <- subset$sample_size[which(meets)[1]]
    }
  }

  # --- Assemble per-item output ---
  details <- data.frame(
    item          = combos$item,
    param         = combos$param,
    recommended_n = rec_n,
    criterion     = rep(criterion, nrow(combos)),
    threshold     = rep(threshold, nrow(combos)),
    stringsAsFactors = FALSE
  )

  if (aggregate == "none") {
    return(details)
  }

  # --- Aggregate to a scalar ---
  na_rows <- which(is.na(details$recommended_n))
  if (length(na_rows) > 0L) {
    cmp <- if (higher_is_better) ">=" else "<="
    failed <- paste0("(item ", details$item[na_rows],
                     ", param ", details$param[na_rows], ")")
    cli::cli_warn(
      c(
        "No tested sample size meets {.field {criterion}} {cmp} {threshold} for some item/param combinations.",
        "i" = "Affected: {failed}",
        "i" = "Aggregate returned as NA. Inspect {.code attr(result, \"details\")} for per-item values."
      )
    )
    agg_value <- NA_integer_
  } else {
    agg_value <- switch(
      aggregate,
      max    = max(details$recommended_n),
      mean   = ceiling(mean(details$recommended_n)),
      median = ceiling(stats::median(details$recommended_n))
    )
    agg_value <- as.integer(agg_value)
  }

  out <- agg_value
  attr(out, "details")   <- details
  attr(out, "aggregate") <- aggregate
  attr(out, "criterion") <- criterion
  attr(out, "threshold") <- threshold
  out
}
