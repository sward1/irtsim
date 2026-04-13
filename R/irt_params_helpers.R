#' Generate 2PL Item Parameters
#'
#' Creates a list of discrimination (`a`) and difficulty (`b`) parameters
#' suitable for passing to [irt_design()].
#'
#' @param n_items Positive integer. Number of items.
#' @param a_dist Character string for the discrimination distribution.
#'   Currently only `"lnorm"` (log-normal) is supported. Default: `"lnorm"`.
#' @param a_mean Numeric. Mean of the log-normal distribution for `a`
#'   (i.e., `meanlog`). Default: `0`.
#' @param a_sd Numeric. SD of the log-normal distribution for `a`
#'   (i.e., `sdlog`). Default: `0.25`.
#' @param b_dist Character string for the difficulty distribution. One of
#'   `"normal"` or `"even"`. Default: `"normal"`.
#' @param b_mean Numeric. Mean of the normal distribution for `b`.
#'   Only used when `b_dist = "normal"`. Default: `0`.
#' @param b_sd Numeric. SD of the normal distribution for `b`.
#'   Only used when `b_dist = "normal"`. Default: `1`.
#' @param b_range Numeric vector of length 2. Range for evenly-spaced `b`
#'   values. Only used when `b_dist = "even"`. Default: `c(-2, 2)`.
#' @param seed Optional integer seed for reproducibility. If `NULL` (default),
#'   the current RNG state is used.
#'
#' @return A named list with elements `a` (numeric vector) and `b` (numeric
#'   vector), each of length `n_items`.
#'
#' @examples
#' # Default 2PL parameters for 30 items
#' params <- irt_params_2pl(n_items = 30, seed = 42)
#'
#' # Evenly-spaced difficulty
#' params <- irt_params_2pl(n_items = 20, b_dist = "even", b_range = c(-3, 3))
#'
#' @seealso [irt_params_grm()] for GRM parameters, [irt_design()] to use the
#'   generated parameters.
#' @export
irt_params_2pl <- function(n_items,
                           a_dist = "lnorm",
                           a_mean = 0,
                           a_sd = 0.25,
                           b_dist = "normal",
                           b_mean = 0,
                           b_sd = 1,
                           b_range = c(-2, 2),
                           seed = NULL) {

  # Validate
  n_items <- as.integer(n_items)
  if (n_items < 1L) {
    stop("`n_items` must be a positive integer. Got: ", n_items, ".",
         call. = FALSE)
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate discrimination
  a <- generate_discrimination(n_items, a_dist, a_mean, a_sd)

  # Generate difficulty
  b <- switch(b_dist,
    normal = stats::rnorm(n_items, mean = b_mean, sd = b_sd),
    even = seq(b_range[1], b_range[2], length.out = n_items),
    stop("`b_dist` must be 'normal' or 'even'. Got: '", b_dist, "'.",
         call. = FALSE)
  )

  list(a = a, b = b)
}


#' Generate GRM Item Parameters
#'
#' Creates a list of discrimination (`a`) and threshold (`b`) parameters
#' suitable for passing to [irt_design()] with `model = "GRM"`.
#'
#' @param n_items Positive integer. Number of items.
#' @param n_categories Positive integer >= 2. Number of response categories
#'   per item. Produces `n_categories - 1` threshold columns in `b`.
#' @param a_dist Character string for the discrimination distribution.
#'   Currently only `"lnorm"` is supported. Default: `"lnorm"`.
#' @param a_mean Numeric. `meanlog` for the log-normal distribution.
#'   Default: `0`.
#' @param a_sd Numeric. `sdlog` for the log-normal distribution.
#'   Default: `0.25`.
#' @param b_mean Numeric. Mean around which thresholds are centered.
#'   Default: `0`.
#' @param b_sd Numeric. SD of the base threshold distribution. Default: `1`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{a}{Numeric vector of length `n_items`.}
#'     \item{b}{Numeric matrix with `n_items` rows and
#'       `n_categories - 1` columns. Thresholds are ordered within each row.}
#'   }
#'
#' @examples
#' # GRM parameters: 15 items, 5 response categories
#' params <- irt_params_grm(n_items = 15, n_categories = 5, seed = 42)
#'
#' @seealso [irt_params_2pl()] for 2PL parameters, [irt_design()] to use the
#'   generated parameters.
#' @export
irt_params_grm <- function(n_items,
                           n_categories,
                           a_dist = "lnorm",
                           a_mean = 0,
                           a_sd = 0.25,
                           b_mean = 0,
                           b_sd = 1,
                           seed = NULL) {

  # Validate
  n_items <- as.integer(n_items)
  if (n_items < 1L) {
    stop("`n_items` must be a positive integer. Got: ", n_items, ".",
         call. = FALSE)
  }

  n_categories <- as.integer(n_categories)
  if (n_categories < 2L) {
    stop("`n_categories` must be >= 2. Got: ", n_categories, ".",
         call. = FALSE)
  }

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate discrimination
  a <- generate_discrimination(n_items, a_dist, a_mean, a_sd)

  # Generate thresholds: n_items x (n_categories - 1)
  n_thresholds <- n_categories - 1L
  b_raw <- matrix(
    stats::rnorm(n_items * n_thresholds, mean = b_mean, sd = b_sd),
    nrow = n_items,
    ncol = n_thresholds
  )

  # Sort each row to ensure ordered thresholds
  if (n_thresholds == 1L) {
    # apply + t would flip dimensions for single-column; no sorting needed
    b <- b_raw
  } else {
    b <- t(apply(b_raw, 1, sort))
  }

  list(a = a, b = b)
}
