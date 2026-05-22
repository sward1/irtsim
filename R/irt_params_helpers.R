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


#' Generate 3PL Item Parameters
#'
#' Creates a list of discrimination (`a`), difficulty (`b`), and guessing
#' (`c`) parameters suitable for passing to [irt_design()] with
#' `model = "3PL"`.
#'
#' @param n_items Positive integer. Number of items.
#' @param a_dist Character string for the discrimination distribution.
#'   Currently only `"lnorm"` (log-normal) is supported. Default: `"lnorm"`.
#' @param a_mean Numeric. `meanlog` for the log-normal distribution.
#'   Default: `0`.
#' @param a_sd Numeric. `sdlog` for the log-normal distribution.
#'   Default: `0.25`.
#' @param b_dist Character string for the difficulty distribution. One of
#'   `"normal"` or `"even"`. Default: `"normal"`.
#' @param b_mean Numeric. Mean of the normal distribution for `b`.
#'   Only used when `b_dist = "normal"`. Default: `0`.
#' @param b_sd Numeric. SD of the normal distribution for `b`.
#'   Only used when `b_dist = "normal"`. Default: `1`.
#' @param b_range Numeric vector of length 2. Range for evenly-spaced `b`
#'   values. Only used when `b_dist = "even"`. Default: `c(-2, 2)`.
#' @param c_shape1 Positive numeric. First shape parameter of the Beta
#'   distribution used to generate `c`. Default: `5`.
#' @param c_shape2 Positive numeric. Second shape parameter. Default: `17`.
#'   The default `Beta(5, 17)` has `E[c] ~= 0.227, SD ~= 0.087`, consistent
#'   with typical four-option multiple-choice items.
#' @param seed Optional integer seed for reproducibility. If `NULL`
#'   (default), the current RNG state is used.
#'
#' @return A named list with elements `a`, `b`, `c`, each a numeric vector
#'   of length `n_items`.
#'
#' @examples
#' # Default 3PL parameters for 30 items
#' params <- irt_params_3pl(n_items = 30, seed = 42)
#'
#' # Custom guessing distribution (e.g., 5-option items, lower chance level)
#' params <- irt_params_3pl(
#'   n_items = 30, c_shape1 = 4, c_shape2 = 16, seed = 42
#' )
#'
#' @seealso [irt_params_2pl()], [irt_params_grm()], [irt_design()].
#' @export
irt_params_3pl <- function(n_items,
                           a_dist = "lnorm",
                           a_mean = 0,
                           a_sd = 0.25,
                           b_dist = "normal",
                           b_mean = 0,
                           b_sd = 1,
                           b_range = c(-2, 2),
                           c_shape1 = 5,
                           c_shape2 = 17,
                           seed = NULL) {
  # Thin wrapper over the 3PL registry method (Obj 37 sub-step c). All
  # generation and validation logic lives in
  # get_model_config("3PL")$generate_default_params(); the wrapper exists
  # purely as a user-facing API surface.
  get_model_config("3PL")$generate_default_params(
    n_items  = n_items,
    a_dist   = a_dist,
    a_mean   = a_mean,
    a_sd     = a_sd,
    b_dist   = b_dist,
    b_mean   = b_mean,
    b_sd     = b_sd,
    b_range  = b_range,
    c_shape1 = c_shape1,
    c_shape2 = c_shape2,
    seed     = seed
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


#' Generate PCM Item Parameters
#'
#' Creates a list of discrimination (`a`, fixed at 1) and step (`b`)
#' parameters suitable for passing to [irt_design()] with `model = "PCM"`.
#'
#' The Partial Credit Model (Masters, 1982) is a Rasch-family polytomous
#' model: every item shares the same discrimination (fixed at 1), and the
#' step parameters within each item are NOT required to be ordered. This
#' is the defining contrast with the Graded Response Model — see
#' [irt_params_grm()] for the ordered-threshold alternative.
#'
#' @param n_items Positive integer. Number of items.
#' @param n_categories Positive integer >= 2. Number of response categories
#'   per item. Produces `n_categories - 1` step columns in `b`.
#' @param b_dist Character string for the item-center distribution: either
#'   `"normal"` (default) or `"even"`.
#' @param b_mean Numeric. Mean of item centers when `b_dist = "normal"`.
#'   Default: `0`.
#' @param b_sd Numeric. SD of item centers when `b_dist = "normal"`.
#'   Default: `1`.
#' @param b_range Length-2 numeric vector giving the minimum and maximum
#'   item-center values. Only used when `b_dist = "even"`. Default: `c(-2, 2)`.
#' @param step_dispersion Non-negative numeric. SD of the within-item step
#'   offsets drawn from `rnorm(0, step_dispersion)` and added to each item's
#'   center. Default: `1.0`, consistent with mirt::simdata's polytomous
#'   conventions and the PCM examples in Embretson & Reise (2000). `0` is
#'   allowed (all steps within an item equal the item center — degenerate
#'   but useful for design exploration).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{a}{Numeric vector of length `n_items`, all 1 (Rasch family).}
#'     \item{b}{Numeric matrix with `n_items` rows and
#'       `n_categories - 1` columns. Steps are NOT sorted within row.}
#'   }
#'
#' @examples
#' # PCM parameters: 15 items, 4 response categories
#' params <- irt_params_pcm(n_items = 15, n_categories = 4, seed = 42)
#'
#' # Tighter within-item step spread (steps closer to the item center)
#' params <- irt_params_pcm(
#'   n_items = 15, n_categories = 4, step_dispersion = 0.5, seed = 42
#' )
#'
#' @seealso [irt_params_grm()] for the ordered-threshold polytomous model,
#'   [irt_design()] to use the generated parameters.
#' @export
irt_params_pcm <- function(n_items,
                           n_categories,
                           b_dist = "normal",
                           b_mean = 0,
                           b_sd = 1,
                           b_range = c(-2, 2),
                           step_dispersion = 1.0,
                           seed = NULL) {
  # Thin wrapper over the PCM registry method (Obj 37 sub-step d). All
  # generation and validation logic lives in
  # get_model_config("PCM")$generate_default_params(); the wrapper exists
  # purely as a user-facing API surface.
  get_model_config("PCM")$generate_default_params(
    n_items         = n_items,
    n_categories    = n_categories,
    b_dist          = b_dist,
    b_mean          = b_mean,
    b_sd            = b_sd,
    b_range         = b_range,
    step_dispersion = step_dispersion,
    seed            = seed
  )
}


#' Generate GPCM Item Parameters
#'
#' Creates a list of discrimination (`a`) and step (`b`) parameters
#' suitable for passing to [irt_design()] with `model = "GPCM"`.
#'
#' The Generalized Partial Credit Model (Muraki, 1992) is partial-credit
#' family — like the Partial Credit Model, step parameters within each item
#' are NOT required to be ordered (the defining contrast with the Graded
#' Response Model). Unlike PCM, GPCM allows per-item discrimination: `a` is
#' a free positive vector rather than fixed at 1. See [irt_params_pcm()]
#' for the Rasch-family alternative.
#'
#' @param n_items Positive integer. Number of items.
#' @param n_categories Positive integer >= 2. Number of response categories
#'   per item. Produces `n_categories - 1` step columns in `b`.
#' @param a_dist Character string for the discrimination distribution.
#'   Currently only `"lnorm"` (log-normal) is supported. Default: `"lnorm"`.
#' @param a_mean Numeric. `meanlog` for the log-normal distribution.
#'   Default: `0`.
#' @param a_sd Numeric. `sdlog` for the log-normal distribution.
#'   Default: `0.25`.
#' @param b_dist Character string for the item-center distribution: either
#'   `"normal"` (default) or `"even"`.
#' @param b_mean Numeric. Mean of item centers when `b_dist = "normal"`.
#'   Default: `0`.
#' @param b_sd Numeric. SD of item centers when `b_dist = "normal"`.
#'   Default: `1`.
#' @param b_range Length-2 numeric vector giving the minimum and maximum
#'   item-center values. Only used when `b_dist = "even"`. Default: `c(-2, 2)`.
#' @param step_dispersion Non-negative numeric. SD of the within-item step
#'   offsets drawn from `rnorm(0, step_dispersion)` and added to each item's
#'   center. Default: `1.0`. `0` is allowed (all steps within an item equal
#'   the item center — degenerate but useful for design exploration).
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{a}{Positive numeric vector of length `n_items`.}
#'     \item{b}{Numeric matrix with `n_items` rows and
#'       `n_categories - 1` columns. Steps are NOT sorted within row.}
#'   }
#'
#' @examples
#' # GPCM parameters: 15 items, 4 response categories
#' params <- irt_params_gpcm(n_items = 15, n_categories = 4, seed = 42)
#'
#' # Tighter within-item step spread and a wider discrimination distribution
#' params <- irt_params_gpcm(
#'   n_items = 15, n_categories = 4,
#'   a_sd = 0.50, step_dispersion = 0.5, seed = 42
#' )
#'
#' @seealso [irt_params_pcm()] for the Rasch-family (a fixed at 1)
#'   alternative, [irt_params_grm()] for the ordered-threshold polytomous
#'   model, [irt_design()] to use the generated parameters.
#' @export
irt_params_gpcm <- function(n_items,
                            n_categories,
                            a_dist = "lnorm",
                            a_mean = 0,
                            a_sd = 0.25,
                            b_dist = "normal",
                            b_mean = 0,
                            b_sd = 1,
                            b_range = c(-2, 2),
                            step_dispersion = 1.0,
                            seed = NULL) {
  # Thin wrapper over the GPCM registry method (Obj 37 sub-step e). All
  # generation and validation logic lives in
  # get_model_config("GPCM")$generate_default_params(); the wrapper exists
  # purely as a user-facing API surface.
  get_model_config("GPCM")$generate_default_params(
    n_items         = n_items,
    n_categories    = n_categories,
    a_dist          = a_dist,
    a_mean          = a_mean,
    a_sd            = a_sd,
    b_dist          = b_dist,
    b_mean          = b_mean,
    b_sd            = b_sd,
    b_range         = b_range,
    step_dispersion = step_dispersion,
    seed            = seed
  )
}
