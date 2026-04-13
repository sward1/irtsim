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
#' @param ... Additional arguments (ignored).
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{item}{Item number.}
#'     \item{param}{Parameter name.}
#'     \item{recommended_n}{Minimum sample size meeting the threshold,
#'       or `NA` if no tested sample size meets it.}
#'     \item{criterion}{The criterion used (echoed back for reference).}
#'     \item{threshold}{The threshold used (echoed back for reference).}
#'   }
#'
#' @examples
#' \dontrun{
#' results <- irt_simulate(study, iterations = 100, seed = 42)
#' s <- summary(results)
#'
#' # Minimum N for RMSE <= 0.20 on all items
#' recommended_n(s, criterion = "rmse", threshold = 0.20)
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
                                               param = NULL, item = NULL, ...) {


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

  # --- Assemble output ---
  data.frame(
    item          = combos$item,
    param         = combos$param,
    recommended_n = rec_n,
    criterion     = rep(criterion, nrow(combos)),
    threshold     = rep(threshold, nrow(combos)),
    stringsAsFactors = FALSE
  )
}
