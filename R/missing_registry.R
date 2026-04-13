# missing_registry.R
#
# Central registry for all missing data mechanism-specific metadata. Each of the
# 5 mechanisms (none, mcar, mar, booklet, linking) is defined as a named list
# with standardized metadata.
#
# This allows consistent handling of mechanisms across irt_study, print.irt_study,
# and apply_missing without code duplication.

# =============================================================================
# Missing Mechanism Registry: Centralized Metadata
# =============================================================================

#' Get a Missing Mechanism Configuration from the Registry (Internal)
#'
#' Retrieves the metadata for a specified missing data mechanism and validates
#' that it exists.
#'
#' @param mechanism Character string: one of "none", "mcar", "mar", "booklet",
#'   or "linking".
#' @return A named list with mechanism-specific metadata: `requires_rate`
#'   (logical), `requires_test_design` (logical), `test_design_key` (character
#'   or NA), `print_label` (character), `print_style` (character: "plain",
#'   "rate", or "design"), and `design_unit` (character or NA).
#' @keywords internal
get_missing_config <- function(mechanism) {
  registry <- .get_missing_registry()

  if (!mechanism %in% names(registry)) {
    supported <- names(registry)
    stop(
      "`mechanism` must be one of: ", paste(supported, collapse = ", "),
      ". Got: '", mechanism, "'.",
      call. = FALSE
    )
  }

  registry[[mechanism]]
}


#' Get All Valid Missing Mechanisms (Internal)
#'
#' Returns the names of all mechanisms in the registry.
#'
#' @return Character vector of mechanism names, in registry order.
#' @keywords internal
valid_missing_mechanisms <- function() {
  names(.get_missing_registry())
}


#' Internal Missing Mechanism Registry
#'
#' @return A named list of missing mechanism configurations.
#' @keywords internal
.get_missing_registry <- function() {
  list(
    "none"    = .missing_none(),
    "mcar"    = .missing_mcar(),
    "mar"     = .missing_mar(),
    "booklet" = .missing_booklet(),
    "linking" = .missing_linking()
  )
}


# =============================================================================
# Missing Mechanism Configurations
# =============================================================================

.missing_none <- function() {
  list(
    requires_rate         = FALSE,
    requires_test_design  = FALSE,
    test_design_key       = NA_character_,
    print_label           = "none (complete data)",
    print_style           = "plain",
    design_unit           = NA_character_
  )
}

.missing_mcar <- function() {
  list(
    requires_rate         = TRUE,
    requires_test_design  = FALSE,
    test_design_key       = NA_character_,
    print_label           = "MCAR",
    print_style           = "rate",
    design_unit           = NA_character_
  )
}

.missing_mar <- function() {
  list(
    requires_rate         = TRUE,
    requires_test_design  = FALSE,
    test_design_key       = NA_character_,
    print_label           = "MAR",
    print_style           = "rate",
    design_unit           = NA_character_
  )
}

.missing_booklet <- function() {
  list(
    requires_rate         = FALSE,
    requires_test_design  = TRUE,
    test_design_key       = "booklet_matrix",
    print_label           = "booklet design",
    print_style           = "design",
    design_unit           = "booklets"
  )
}

.missing_linking <- function() {
  list(
    requires_rate         = FALSE,
    requires_test_design  = TRUE,
    test_design_key       = "linking_matrix",
    print_label           = "linking design",
    print_style           = "design",
    design_unit           = "forms"
  )
}
