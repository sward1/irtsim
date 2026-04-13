# apply_missing.R
# Internal function: introduce missingness into a complete response matrix
#
# Supports five mechanisms:
#   none     — return data unchanged
#   mcar     — each cell independently set to NA with probability = missing_rate
#   mar      — missingness probability depends on theta (logistic)
#   booklet  — respondents assigned to booklets; items not in booklet are NA
#   linking  — respondents assigned to forms; items not in form are NA


#' Apply Missing Data Mechanism (Internal)
#'
#' Takes a complete response matrix and introduces missingness according
#' to the study specification.
#'
#' @param data Numeric matrix (N x n_items). Complete response data.
#' @param study An `irt_study` object specifying the missing data mechanism.
#' @param seed Integer. Random seed for reproducibility.
#' @param theta Optional numeric vector of length `nrow(data)`. Required
#'   when `study$missing == "mar"`.
#'
#' @return Numeric matrix of same dimensions as `data`, with `NA` values
#'   introduced according to the missingness mechanism.
#'
#' @keywords internal
apply_missing <- function(data, study, seed = NULL, theta = NULL) {

  if (!is.null(seed)) set.seed(seed)

  mechanism <- study$missing

  if (!mechanism %in% valid_missing_mechanisms()) {
    stop(
      "`mechanism` must be one of: ",
      paste(valid_missing_mechanisms(), collapse = ", "),
      ". Got: '", mechanism, "'.",
      call. = FALSE
    )
  }

  switch(mechanism,
    none     = apply_missing_none(data),
    mcar     = apply_missing_mcar(data, study$missing_rate),
    mar      = apply_missing_mar(data, study$missing_rate, theta),
    booklet  = apply_missing_structured(data, study$test_design$booklet_matrix),
    linking  = apply_missing_structured(data, study$test_design$linking_matrix)
  )
}


# --- Mechanism implementations ------------------------------------------------


#' @noRd
apply_missing_none <- function(data) {
  data
}


#' @noRd
apply_missing_mcar <- function(data, missing_rate) {
  if (missing_rate == 0) return(data)

  # Generate a mask: TRUE = set to NA
  mask <- matrix(
    stats::runif(length(data)) < missing_rate,
    nrow = nrow(data),
    ncol = ncol(data)
  )
  data[mask] <- NA
  data
}


#' @noRd
apply_missing_mar <- function(data, missing_rate, theta) {
  if (is.null(theta)) {
    stop(
      "`theta` is required for MAR missingness mechanism.",
      call. = FALSE
    )
  }

  n <- nrow(data)
  n_items <- ncol(data)

  # Logistic MAR: probability of missing depends on theta.
  # Lower theta → higher probability of missing.
  # Calibrate the intercept so the marginal missing rate ≈ missing_rate.
  #
  # P(missing | theta) = 1 / (1 + exp(-(b0 + b1 * theta)))
  # With b1 < 0: lower theta → higher missing probability.
  # We use b1 = -1 and solve for b0 so that E[P(missing)] ≈ missing_rate.
  #
  # For standard normal theta, the average logistic probability is approximately:
  #   E[logistic(b0 + b1*theta)] ≈ logistic(b0 / sqrt(1 + b1^2 * pi/8))
  # (probit approximation). We invert to find b0.

  b1 <- -1
  # Solve: logistic(b0 / sqrt(1 + b1^2 * pi/8)) = missing_rate
  # → b0 / sqrt(1 + pi/8) = log(missing_rate / (1 - missing_rate))
  # → b0 = log(missing_rate / (1 - missing_rate)) * sqrt(1 + pi/8)
  scaling <- sqrt(1 + b1^2 * pi / 8)
  b0 <- stats::qlogis(missing_rate) * scaling

  # Compute per-person missing probability
  p_missing <- stats::plogis(b0 + b1 * theta)

  # Generate mask
  mask <- matrix(
    stats::runif(n * n_items),
    nrow = n,
    ncol = n_items
  )
  # Each row uses the same person-level probability
  threshold <- matrix(rep(p_missing, each = n_items), nrow = n, ncol = n_items,
                      byrow = TRUE)
  data[mask < threshold] <- NA
  data
}


#' @noRd
apply_missing_structured <- function(data, design_matrix) {
  # design_matrix: (n_booklets/forms x n_items), binary
  # Each respondent is assigned to one booklet/form (round-robin).
  # Items where the design_matrix row is 0 are set to NA.

  n <- nrow(data)
  n_forms <- nrow(design_matrix)

  # Assign respondents to forms: round-robin
  form_assignment <- ((seq_len(n) - 1L) %% n_forms) + 1L

  for (i in seq_len(n)) {
    form <- form_assignment[i]
    not_administered <- which(design_matrix[form, ] == 0)
    if (length(not_administered) > 0) {
      data[i, not_administered] <- NA
    }
  }

  data
}
