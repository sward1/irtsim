# plot_irt_results.R
# S3 plot methods for irt_results and summary_irt_results objects.
#
# plot.irt_results() is the primary user-facing method: it calls summary()
# internally, then delegates to the shared plotting engine.
# plot.summary_irt_results() is a convenience method for users who
# already have a summary in hand.
#
# Default plot: criterion (y) by sample_size (x), line + point per param,
# faceted by item when multiple items are present.


#' Plot IRT Simulation Results
#'
#' Visualize performance criteria across sample sizes from an
#' [irt_simulate()] result. Calls [summary.irt_results()] internally,
#' then plots the requested criterion by sample size.
#'
#' @param x An `irt_results` object from [irt_simulate()].
#' @param criterion Character string. Which criterion to plot.
#'   Default `"rmse"`. Valid values: `"bias"`, `"empirical_se"`, `"mse"`,
#'   `"rmse"`, `"coverage"`, `"mcse_bias"`, `"mcse_mse"`.
#' @param param Optional character vector. Filter to specific parameter
#'   types (e.g., `"a"`, `"b"`, `"b1"`).
#' @param item Optional integer vector. Filter to specific item numbers.
#' @param threshold Optional numeric. If provided, draws a horizontal
#'   reference line at this value.
#' @param ... Additional arguments passed to [summary.irt_results()].
#'
#' @return A [ggplot2::ggplot] object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' results <- irt_simulate(study, iterations = 100, seed = 42)
#' plot(results)
#' plot(results, criterion = "bias", threshold = 0.05, param = "b")
#' }
#'
#' @seealso [summary.irt_results()] for the underlying criteria,
#'   [recommended_n()] for sample-size recommendations.
#' @export
plot.irt_results <- function(x, criterion = "rmse", param = NULL,
                             item = NULL, threshold = NULL, ...) {

  if (!inherits(x, "irt_results")) {
    stop("`x` must be an `irt_results` object.", call. = FALSE)
  }

  s <- summary(x, ...)
  build_criterion_plot(s, criterion = criterion, param = param,
                       item = item, threshold = threshold)
}


#' Plot Summary of IRT Simulation Results
#'
#' Visualize performance criteria from a [summary.irt_results()] object.
#' This is a convenience method for users who already have a summary;
#' [plot.irt_results()] is the primary interface.
#'
#' @param x A `summary_irt_results` object from [summary.irt_results()].
#' @param criterion Character string. Which criterion to plot.
#'   Default `"rmse"`.
#' @param param Optional character vector. Filter to specific parameter
#'   types.
#' @param item Optional integer vector. Filter to specific item numbers.
#' @param threshold Optional numeric. If provided, draws a horizontal
#'   reference line at this value.
#' @param ... Additional arguments (ignored).
#'
#' @return A [ggplot2::ggplot] object, returned invisibly.
#'
#' @examples
#' \dontrun{
#' results <- irt_simulate(study, iterations = 100, seed = 42)
#' s <- summary(results)
#' plot(s, criterion = "rmse", threshold = 0.15)
#' }
#'
#' @seealso [plot.irt_results()], [summary.irt_results()]
#' @export
plot.summary_irt_results <- function(x, criterion = "rmse", param = NULL,
                                     item = NULL, threshold = NULL, ...) {

  if (!inherits(x, "summary_irt_results")) {
    stop("`x` must be a `summary_irt_results` object.", call. = FALSE)
  }

  build_criterion_plot(x, criterion = criterion, param = param,
                       item = item, threshold = threshold)
}


#' Build Criterion Plot (Internal)
#'
#' Shared plotting engine used by both [plot.irt_results()] and
#' [plot.summary_irt_results()].
#'
#' @param summary_obj A `summary_irt_results` object.
#' @param criterion Character string. Criterion to plot.
#' @param param Optional character vector. Parameter filter.
#' @param item Optional integer vector. Item filter.
#' @param threshold Optional numeric. Horizontal reference line.
#'
#' @return A [ggplot2::ggplot] object, returned invisibly.
#' @keywords internal
build_criterion_plot <- function(summary_obj, criterion = "rmse",
                                 param = NULL, item = NULL,
                                 threshold = NULL) {

  # --- Validate criterion ---
  all_criteria <- valid_criteria()

  if (length(criterion) != 1L || !criterion %in% all_criteria) {
    stop("Invalid criterion: '", criterion, "'. Valid options: ",
         paste(all_criteria, collapse = ", "), call. = FALSE)
  }

  # --- Prepare data ---
  plot_data <- summary_obj$item_summary

  if (!is.null(param)) {
    plot_data <- plot_data[plot_data$param %in% param, , drop = FALSE]
  }

  if (!is.null(item)) {
    plot_data <- plot_data[plot_data$item %in% item, , drop = FALSE]
  }

  # Use absolute value for bias on the plot
  cfg <- get_criterion_config(criterion)
  if (cfg$use_abs) {
    plot_data[[criterion]] <- abs(plot_data[[criterion]])
  }

  # Convert item to factor for faceting labels
  plot_data$item_label <- paste0("Item ", plot_data$item)

  # --- Criterion label from registry ---
  y_label <- cfg$display_label

  # --- Determine if multiple params exist ---
  unique_params <- unique(plot_data$param)
  has_multiple_params <- length(unique_params) > 1L

  # --- Determine if multiple items exist ---
  unique_items <- unique(plot_data$item)
  has_multiple_items <- length(unique_items) > 1L

  # --- Build plot ---
  if (has_multiple_params) {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[["sample_size"]],
        y = .data[[criterion]],
        colour = .data[["param"]],
        linetype = .data[["param"]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[["sample_size"]],
        y = .data[[criterion]]
      )
    )
  }

  p <- p +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Sample Size",
      y = y_label,
      colour = "Parameter",
      linetype = "Parameter"
    ) +
    ggplot2::theme_minimal()

  # --- Threshold line ---
  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_hline(
      yintercept = threshold,
      linetype = "dashed",
      colour = "grey40"
    )
  }

  # --- Faceting ---
  if (has_multiple_items) {
    p <- p + ggplot2::facet_wrap(~ item_label)
  }

  invisible(p)
}
