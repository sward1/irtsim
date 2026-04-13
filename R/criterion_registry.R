# criterion_registry.R
#
# Central registry for all criterion-specific metadata. Each of the 7
# performance criteria (bias, empirical_se, mse, rmse, coverage, mcse_bias,
# mcse_mse) is defined as a named list with standardized metadata.
#
# This allows consistent handling of criteria across summary, plot, and
# recommended_n without code duplication.

# =============================================================================
# Criterion Registry: Centralized Metadata
# =============================================================================

#' Get a Criterion Configuration from the Registry (Internal)
#'
#' Retrieves the metadata for a specified criterion and validates that it exists.
#'
#' @param criterion Character string: one of "bias", "empirical_se", "mse",
#'   "rmse", "coverage", "mcse_bias", "mcse_mse".
#' @return A named list with criterion-specific metadata: `direction`
#'   (char: "lower_is_better" or "higher_is_better"), `use_abs` (logical),
#'   and `display_label` (char).
#' @keywords internal
get_criterion_config <- function(criterion) {
  registry <- .get_criterion_registry()

  if (!criterion %in% names(registry)) {
    supported <- names(registry)
    stop(
      "`criterion` must be one of: ", paste(supported, collapse = ", "),
      ". Got: '", criterion, "'.",
      call. = FALSE
    )
  }

  registry[[criterion]]
}


#' Get All Valid Criteria (Internal)
#'
#' Returns the names of all criteria in the registry.
#'
#' @return Character vector of criterion names, in registry order.
#' @keywords internal
valid_criteria <- function() {
  names(.get_criterion_registry())
}


#' Internal Criterion Registry
#'
#' @return A named list of criterion configurations.
#' @keywords internal
.get_criterion_registry <- function() {
  list(
    "bias"         = .criterion_bias(),
    "empirical_se" = .criterion_empirical_se(),
    "mse"          = .criterion_mse(),
    "rmse"         = .criterion_rmse(),
    "coverage"     = .criterion_coverage(),
    "mcse_bias"    = .criterion_mcse_bias(),
    "mcse_mse"     = .criterion_mcse_mse()
  )
}


# =============================================================================
# Criterion Configurations
# =============================================================================

.criterion_bias <- function() {
  list(
    direction     = "lower_is_better",
    use_abs       = TRUE,
    display_label = "Absolute Bias"
  )
}

.criterion_empirical_se <- function() {
  list(
    direction     = "lower_is_better",
    use_abs       = FALSE,
    display_label = "Empirical SE"
  )
}

.criterion_mse <- function() {
  list(
    direction     = "lower_is_better",
    use_abs       = FALSE,
    display_label = "MSE"
  )
}

.criterion_rmse <- function() {
  list(
    direction     = "lower_is_better",
    use_abs       = FALSE,
    display_label = "RMSE"
  )
}

.criterion_coverage <- function() {
  list(
    direction     = "higher_is_better",
    use_abs       = FALSE,
    display_label = "Coverage"
  )
}

.criterion_mcse_bias <- function() {
  list(
    direction     = "lower_is_better",
    use_abs       = FALSE,
    display_label = "MCSE (Bias)"
  )
}

.criterion_mcse_mse <- function() {
  list(
    direction     = "lower_is_better",
    use_abs       = FALSE,
    display_label = "MCSE (MSE)"
  )
}
