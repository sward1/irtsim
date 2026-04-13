# generate_data.R
# Internal function: generate IRT response data via mirt::simdata
#
# Translates the user-facing b (difficulty) parameterization to
# mirt's slope-intercept (d) parameterization, generates theta,
# and returns a response matrix.


#' Generate IRT Response Data (Internal)
#'
#' Wraps [mirt::simdata()] to produce a response matrix from an
#' [irt_design] specification. Handles the b-to-d parameterization
#' translation and theta generation.
#'
#' @param design An `irt_design` object.
#' @param n Integer. Number of respondents.
#' @param seed Optional integer. Random seed for reproducibility. If `NULL`,
#'   the current RNG state drives draws (used by the parallel dispatch path
#'   in [irt_simulate()] so future.apply's L'Ecuyer-CMRG substreams are not
#'   clobbered by an explicit `set.seed()` call).
#' @param theta Optional numeric vector of length `n`. Pre-generated theta
#'   values. If `NULL`, theta is drawn from `design$theta_dist`.
#'
#' @return A numeric matrix with `n` rows and `design$n_items` columns.
#'
#' @keywords internal
generate_data <- function(design, n, seed = NULL, theta = NULL) {

  if (!is.null(seed)) set.seed(seed)


  # --- Generate or validate theta ---------------------------------------------
  if (!is.null(theta)) {
    if (length(theta) != n) {
      stop(
        "Length of `theta` (", length(theta),
        ") does not match `n` (", n, ").",
        call. = FALSE
      )
    }
  } else {
    theta <- generate_theta(design$theta_dist, n)
  }

  # mirt::simdata expects Theta as a matrix (N x n_factors)
  Theta <- matrix(theta, ncol = design$n_factors)


  # --- Extract item parameters ------------------------------------------------
  a <- design$item_params$a
  b <- design$item_params$b
  n_items <- design$n_items


  # --- Determine itemtype and convert b → d -----------------------------------
  # Delegate to the model registry for parameter conversion
  model_config <- get_model_config(design$model)
  mirt_params <- model_config$convert_to_mirt(design)
  d <- mirt_params$d
  itemtype <- mirt_params$itemtype


  # --- Ensure a is a matrix for mirt::simdata ---------------------------------
  if (!is.matrix(a)) {
    a <- matrix(a, ncol = design$n_factors)
  }


  # --- Call mirt::simdata -----------------------------------------------------
  dat <- mirt::simdata(
    a = a,
    d = d,
    N = n,
    itemtype = itemtype,
    Theta = Theta
  )

  # Ensure matrix output
  if (!is.matrix(dat)) {
    dat <- as.matrix(dat)
  }

  dat
}


#' Generate Theta Values from a Distribution Specification (Internal)
#'
#' @param theta_dist Character string (`"normal"`, `"uniform"`) or function.
#' @param n Integer. Number of values to generate.
#'
#' @return Numeric vector of length `n`.
#'
#' @keywords internal
generate_theta <- function(theta_dist, n) {
  if (is.character(theta_dist)) {
    switch(theta_dist,
      normal  = stats::rnorm(n, mean = 0, sd = 1),
      uniform = stats::runif(n, min = -3, max = 3),
      stop("Unknown theta_dist: '", theta_dist, "'.", call. = FALSE)
    )
  } else if (is.function(theta_dist)) {
    theta_dist(n)
  } else {
    stop("`theta_dist` must be a character string or function.", call. = FALSE)
  }
}
