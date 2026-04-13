#' Define Study Conditions for an IRT Simulation
#'
#' Add study-level conditions to an IRT design specification. This captures
#' decisions 4--5 from the Schroeders & Gnambs (2025) framework: sample sizes
#' and missing data mechanism.
#'
#' @param design An \code{\link{irt_design}} object specifying the
#'   data-generating model.
#' @param sample_sizes Integer vector of sample sizes to evaluate. Values are
#'   coerced to integer, sorted in ascending order, and deduplicated.
#' @param missing Character string specifying the missing data mechanism. One
#'   of \code{"none"} (default), \code{"mcar"}, \code{"mar"}, \code{"booklet"},
#'   or \code{"linking"}.
#' @param missing_rate Numeric value in \eqn{[0, 1)} specifying the proportion
#'   of missing data. Required when \code{missing} is \code{"mcar"} or
#'   \code{"mar"}; ignored when \code{missing} is \code{"none"}.
#' @param test_design A list specifying the test design for structured
#'   missingness. Required when \code{missing} is \code{"booklet"} or
#'   \code{"linking"}.
#'   \describe{
#'     \item{booklet}{Must contain \code{booklet_matrix}: a binary matrix
#'       (n_booklets x n_items) where 1 indicates the item is administered.}
#'     \item{linking}{Must contain \code{linking_matrix}: a binary matrix
#'       (n_forms x n_items) where 1 indicates the item appears on the form.}
#'   }
#' @param estimation_model Character string specifying the IRT model to fit.
#'   One of \code{"1PL"}, \code{"2PL"}, or \code{"GRM"}. If \code{NULL}
#'   (default), defaults to \code{design$model} (i.e., the generation model is
#'   also the estimation model). Set to a different model to perform
#'   misspecification studies (e.g., generate 2PL, estimate 1PL). Cross-fits
#'   are only allowed within the same response format (binary: 1PL, 2PL;
#'   polytomous: GRM).
#'
#' @return An S3 object of class \code{irt_study} (a named list) with elements
#'   \code{design}, \code{missing}, \code{missing_rate}, \code{sample_sizes},
#'   \code{test_design}, and \code{estimation_model}.
#'
#' @examples
#' # Simple study with no missing data
#' d <- irt_design(
#'   model = "1PL", n_items = 20,
#'   item_params = list(b = seq(-2, 2, length.out = 20))
#' )
#' study <- irt_study(d, sample_sizes = c(100, 250, 500))
#'
#' # Study with MCAR missingness
#' study_mcar <- irt_study(d, sample_sizes = c(200, 400),
#'                         missing = "mcar", missing_rate = 0.2)
#'
#' # Model misspecification: generate 2PL, fit 1PL
#' d_2pl <- irt_design(
#'   model = "2PL", n_items = 15,
#'   item_params = list(a = rlnorm(15, 0, 0.25), b = rnorm(15))
#' )
#' study_misspec <- irt_study(d_2pl, sample_sizes = c(100, 300),
#'                            estimation_model = "1PL")
#'
#' @seealso [irt_design()] for the design specification,
#'   [irt_simulate()] to run the simulation.
#' @importFrom cli cli_abort
#' @export
irt_study <- function(design,
                      sample_sizes,
                      missing = "none",
                      missing_rate = NULL,
                      test_design = NULL,
                      estimation_model = NULL) {

  # --- Validate design --------------------------------------------------------
  if (!inherits(design, "irt_design")) {
    cli::cli_abort("{.arg design} must be an {.cls irt_design} object.")
  }

  # --- Validate sample_sizes --------------------------------------------------
  if (!is.numeric(sample_sizes)) {
    cli::cli_abort("{.arg sample_sizes} must be a numeric (integer) vector.")
  }
  if (length(sample_sizes) == 0L) {
    cli::cli_abort("{.arg sample_sizes} must have length >= 1.")
  }
  if (anyNA(sample_sizes)) {
    cli::cli_abort("{.arg sample_sizes} must not contain NA values.")
  }
  if (any(sample_sizes != floor(sample_sizes))) {
    cli::cli_abort("{.arg sample_sizes} must be whole integer values.")
  }
  sample_sizes <- as.integer(sample_sizes)
  if (any(sample_sizes < 1L)) {
    cli::cli_abort("All {.arg sample_sizes} must be positive integers.")
  }
  # Sort ascending and deduplicate

  sample_sizes <- sort(unique(sample_sizes))

  # --- Validate missing -------------------------------------------------------
  valid_missing <- valid_missing_mechanisms()
  if (!missing %in% valid_missing) {
    cli::cli_abort(c(
      "Invalid {.arg missing}.",
      "i" = "Supported mechanisms: {.val {valid_missing}}",
      "x" = "You passed: {.val {missing}}"
    ))
  }
  cfg <- get_missing_config(missing)

  # --- Validate missing_rate --------------------------------------------------
  if (missing == "none") {
    # Silently override any user-supplied rate
    missing_rate <- 0
  } else if (cfg$requires_rate) {
    if (is.null(missing_rate)) {
      cli::cli_abort(c(
        "{.arg missing_rate} is required.",
        "i" = "Set {.arg missing_rate} when {.arg missing} = {.val {missing}}."
      ))
    }
    if (!is.numeric(missing_rate) || length(missing_rate) != 1L) {
      cli::cli_abort("{.arg missing_rate} must be a single numeric value.")
    }
    if (missing_rate < 0 || missing_rate >= 1) {
      cli::cli_abort(c(
        "{.arg missing_rate} out of range.",
        "i" = "Must be in [0, 1).",
        "x" = "You passed: {.val {missing_rate}}"
      ))
    }
  } else {
    # booklet / linking — missing_rate not used directly
    if (is.null(missing_rate)) missing_rate <- NA_real_
  }

  # --- Validate test_design for booklet/linking -------------------------------
  n_items <- design$n_items

  if (cfg$requires_test_design) {
    if (is.null(test_design) || is.null(test_design[[cfg$test_design_key]])) {
      cli::cli_abort(c(
        "{.arg test_design} is required.",
        "i" = "Provide {.arg test_design} with a {.field {cfg$test_design_key}} when {.arg missing} = {.val {missing}}."
      ))
    }
    validate_design_matrix(test_design[[cfg$test_design_key]], n_items, cfg$test_design_key)
  }

  # If test_design is NULL and not needed, keep it NULL
  if (is.null(test_design)) test_design <- list()

  # --- Validate and set estimation_model ----------------------------------------
  if (is.null(estimation_model)) {
    estimation_model <- design$model
  } else {
    # get_model_config() raises cli_abort(...) on unknown models,
    # so no wrapper is needed — let its error surface directly.
    est_cfg <- get_model_config(estimation_model)
    gen_cfg <- get_model_config(design$model)

    if (gen_cfg$response_format != est_cfg$response_format) {
      cli::cli_abort(c(
        "Incompatible {.arg estimation_model}.",
        "i" = "Generation model {.val {design$model}} produces {.field {gen_cfg$response_format}} responses.",
        "x" = "Estimation model {.val {estimation_model}} requires {.field {est_cfg$response_format}} responses."
      ))
    }
  }

  # --- Construct S3 object ----------------------------------------------------
  structure(
    list(
      design            = design,
      missing           = missing,
      missing_rate      = missing_rate,
      sample_sizes      = sample_sizes,
      test_design       = test_design,
      estimation_model  = estimation_model
    ),
    class = "irt_study"
  )
}


#' Print an IRT Study
#'
#' Display a compact summary of an [irt_study] object, including model,
#' items, sample sizes, and missing data mechanism.
#'
#' @param x An \code{irt_study} object.
#' @param ... Additional arguments (ignored).
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' d <- irt_design("1PL", 10, list(b = seq(-2, 2, length.out = 10)))
#' s <- irt_study(d, sample_sizes = c(100, 500))
#' print(s)
#'
#' @seealso [irt_study()]
#' @export
print.irt_study <- function(x, ...) {
  cat("IRT Study\n")
  cat("  Model:         ", x$design$model, "\n")

  # Show estimation model if it differs from generation model
  if (x$estimation_model != x$design$model) {
    cat("  Estimation:    ", x$estimation_model, "\n")
  }

  cat("  Items:         ", x$design$n_items, "\n")
  cat("  Sample sizes:  ", paste(x$sample_sizes, collapse = ", "), "\n")

  cfg <- get_missing_config(x$missing)

  if (cfg$print_style == "plain") {
    # "none (complete data)" — use 3 spaces
    cat("  Missing data:   ", cfg$print_label, "\n", sep = "")
  } else if (cfg$print_style == "rate") {
    # "MCAR (rate = X)" — use 2 spaces because sep = ""
    cat("  Missing data:  ", toupper(cfg$print_label),
        " (rate = ", x$missing_rate, ")\n", sep = "")
  } else if (cfg$print_style == "design") {
    # "booklet design (N booklets)" — use 3 spaces
    n_designs <- nrow(x$test_design[[cfg$test_design_key]])
    design_unit <- cfg$design_unit
    design_word <- if (n_designs == 1L) {
      substring(design_unit, 1, nchar(design_unit) - 1)  # singularize
    } else {
      design_unit
    }
    cat("  Missing data:   ", cfg$print_label, " (", n_designs, " ",
        design_word, ")\n", sep = "")
  }

  invisible(x)
}
