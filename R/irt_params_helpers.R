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
  # Thin wrapper over the 2PL registry method (Obj 37 sub-step b refactor).
  # All generation and validation logic lives in
  # get_model_config("2PL")$generate_default_params(). This wrapper exists
  # purely as a user-facing API surface; its signature and defaults are the
  # single source of truth for the 2PL parameter-generation contract.
  get_model_config("2PL")$generate_default_params(
    n_items = n_items,
    a_dist  = a_dist,
    a_mean  = a_mean,
    a_sd    = a_sd,
    b_dist  = b_dist,
    b_mean  = b_mean,
    b_sd    = b_sd,
    b_range = b_range,
    seed    = seed
  )
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
  # Thin wrapper over the GRM registry method (Obj 37 sub-step b refactor).
  # See irt_params_2pl() for the parallel pattern. Generation and validation
  # logic lives in get_model_config("GRM")$generate_default_params().
  get_model_config("GRM")$generate_default_params(
    n_items      = n_items,
    n_categories = n_categories,
    a_dist       = a_dist,
    a_mean       = a_mean,
    a_sd         = a_sd,
    b_mean       = b_mean,
    b_sd         = b_sd,
    seed         = seed
  )
}
