#' Create an IRT Design Specification
#'
#' Define the data-generating model for an IRT simulation study. This
#' captures decisions 1--3 from the Schroeders & Gnambs (2025) framework:
#' dimensionality, item parameters, and item type.
#'
#' @param model Character string specifying the IRT model. One of
#'   `"1PL"`, `"2PL"`, or `"GRM"`.
#' @param n_items Positive integer. Number of items in the instrument.
#' @param item_params A named list of item parameters. Contents depend on
#'   `model`:
#'   \describe{
#'     \item{1PL}{`b` (numeric vector of length `n_items`). Discrimination
#'       is fixed at 1 for all items and added automatically.}
#'     \item{2PL}{`a` (discrimination, positive numeric vector or matrix) and
#'       `b` (difficulty, numeric vector), each of length `n_items`.}
#'     \item{GRM}{`a` (discrimination, positive numeric vector) of length
#'       `n_items` and `b` (threshold matrix, `n_items` rows by
#'       `n_categories - 1` columns).}
#'   }
#' @param theta_dist Either a character string (`"normal"` or `"uniform"`) or
#'   a function that takes a single argument `n` and returns a numeric vector
#'   of length `n`. Defaults to `"normal"`.
#' @param n_factors Positive integer specifying the number of latent factors.
#'   Defaults to `1L`.
#'
#' @return An S3 object of class `irt_design` (a named list) with elements
#'   `model`, `n_items`, `item_params`, `theta_dist`, and `n_factors`.
#'
#' @examples
#' # 1PL (Rasch) design with 20 items
#' design_1pl <- irt_design(
#'   model = "1PL",
#'   n_items = 20,
#'   item_params = list(b = seq(-2, 2, length.out = 20))
#' )
#'
#' # 2PL design
#' design_2pl <- irt_design(
#'   model = "2PL",
#'   n_items = 30,
#'   item_params = list(
#'     a = rlnorm(30, 0, 0.25),
#'     b = seq(-2, 2, length.out = 30)
#'   )
#' )
#'
#' @seealso [irt_study()] to add study conditions, [irt_params_2pl()] and
#'   [irt_params_grm()] to generate item parameters.
#' @importFrom cli cli_abort
#' @export
irt_design <- function(model,
                       n_items,
                       item_params,
                       theta_dist = "normal",
                       n_factors = 1L) {


  # --- Validate model ---------------------------------------------------------
  model_config <- get_model_config(model)  # Validates model name via registry


  # --- Validate n_items -------------------------------------------------------
  n_items <- as.integer(n_items)
  if (n_items < 1L) {
    cli::cli_abort(c(
      "Invalid {.arg n_items}.",
      "x" = "{.arg n_items} must be a positive integer.",
      "i" = "You passed: {.val {n_items}}"
    ))
  }

  # --- Validate item_params ---------------------------------------------------
  if (!is.list(item_params)) {
    cli::cli_abort("{.arg item_params} must be a list.")
  }

  # Check all elements are numeric and NA-free
  for (nm in names(item_params)) {
    val <- item_params[[nm]]
    if (!is.numeric(val)) {
      cli::cli_abort("{.arg item_params${nm}} must be numeric.")
    }
    if (anyNA(val)) {
      cli::cli_abort("{.arg item_params${nm}} must not contain NA values.")
    }
  }

  # --- Model-specific parameter validation ------------------------------------
  # Delegate to the model registry for validation
  item_params <- model_config$validate_params(item_params, n_items)

  # --- Validate theta_dist ----------------------------------------------------
  valid_theta_strings <- c("normal", "uniform")
  if (is.character(theta_dist)) {
    if (!theta_dist %in% valid_theta_strings) {
      cli::cli_abort(c(
        "Invalid {.arg theta_dist}.",
        "i" = "Must be one of {.val {valid_theta_strings}} or a function.",
        "x" = "You passed: {.val {theta_dist}}"
      ))
    }
  } else if (!is.function(theta_dist)) {
    cli::cli_abort(c(
      "Invalid {.arg theta_dist}.",
      "i" = "Must be a character string ({.val {valid_theta_strings}}) or a function."
    ))
  }

  # --- Validate n_factors -----------------------------------------------------
  n_factors <- as.integer(n_factors)

  # --- Construct S3 object ----------------------------------------------------
  structure(
    list(
      model = model,
      n_items = n_items,
      item_params = item_params,
      theta_dist = theta_dist,
      n_factors = n_factors
    ),
    class = "irt_design"
  )
}


#' Print an IRT Design
#'
#' Display a compact summary of an [irt_design] object, including model type,
#' number of items, theta distribution, and parameter ranges.
#'
#' @param x An `irt_design` object.
#' @param ... Additional arguments (ignored).
#'
#' @return `x`, invisibly.
#'
#' @examples
#' d <- irt_design("1PL", 10, list(b = seq(-2, 2, length.out = 10)))
#' print(d)
#'
#' @seealso [irt_design()]
#' @export
print.irt_design <- function(x, ...) {
  cat("IRT Design\n")
  cat("  Model:       ", x$model, "\n")
  cat("  Items:       ", x$n_items, "items\n")

  if (x$model == "GRM") {
    n_cat <- ncol(x$item_params$b) + 1L
    cat("  Categories:  ", n_cat, "per item\n")
  }

  if (is.character(x$theta_dist)) {
    cat("  Theta dist:  ", x$theta_dist, "\n")
  } else {
    cat("  Theta dist:   custom function\n")
  }

  cat("  Factors:     ", x$n_factors, "\n")

  # Parameter summary
  if (!is.null(x$item_params$a) && !is.matrix(x$item_params$a)) {
    cat("  a range:      [",
        round(min(x$item_params$a), 3), ", ",
        round(max(x$item_params$a), 3), "]\n", sep = "")
  }
  if (!is.null(x$item_params$b)) {
    if (is.matrix(x$item_params$b)) {
      cat("  b range:      [",
          round(min(x$item_params$b), 3), ", ",
          round(max(x$item_params$b), 3), "]\n", sep = "")
    } else {
      cat("  b range:      [",
          round(min(x$item_params$b), 3), ", ",
          round(max(x$item_params$b), 3), "]\n", sep = "")
    }
  }

  invisible(x)
}
