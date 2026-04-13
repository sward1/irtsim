# model_registry.R
#
# Central registry for all model-specific logic. Each model (1PL, 2PL, GRM)
# is defined as a named list with standardized functions and metadata.
#
# This allows adding a new model with minimal changes to other files.

# =============================================================================
# Model Registry: Centralized Configurations
# =============================================================================

#' Get a Model Configuration from the Registry (Internal)
#'
#' Retrieves the configuration for a specified model and validates that it exists.
#'
#' @param model Character string: "1PL", "2PL", or "GRM".
#' @return A named list with model-specific functions and metadata.
#' @keywords internal
get_model_config <- function(model) {
  registry <- .get_model_registry()

  if (!model %in% names(registry)) {
    supported <- names(registry)
    stop(
      "`model` must be one of: ", paste(supported, collapse = ", "),
      ". Got: '", model, "'.",
      call. = FALSE
    )
  }

  registry[[model]]
}


#' Internal Model Registry
#'
#' @return A named list of model configurations.
#' @keywords internal
.get_model_registry <- function() {
  list(
    "1PL" = .model_1pl(),
    "2PL" = .model_2pl(),
    "GRM" = .model_grm()
  )
}


# =============================================================================
# 1PL (Rasch) Model Configuration
# =============================================================================

.model_1pl <- function() {
  list(
    param_schema = list(
      b = "numeric vector of length n_items"
    ),
    response_format = "binary",
    mirt_itemtype = "Rasch",
    validate_params = function(item_params, n_items) {
      if (is.null(item_params$b)) {
        stop("1PL model requires `b` in `item_params`.", call. = FALSE)
      }
      if (length(item_params$b) != n_items) {
        stop(
          "Length of `item_params$b` (", length(item_params$b),
          ") does not match `n_items` (", n_items, ").",
          call. = FALSE
        )
      }
      # 1PL fixes discrimination at 1
      item_params$a <- rep(1, n_items)
      item_params
    },
    build_true_params = function(design) {
      n_items <- design$n_items
      b <- design$item_params$b

      # 1PL: only b (a is fixed at 1, not estimated)
      data.frame(
        item       = seq_len(n_items),
        param      = "b",
        true_value = b,
        stringsAsFactors = FALSE
      )
    },
    convert_to_mirt = function(design) {
      # 1PL uses d = -a * b, where a = 1
      a <- design$item_params$a
      b <- design$item_params$b
      d <- -a * b
      itemtype <- rep("2PL", design$n_items)

      list(a = a, d = d, itemtype = itemtype)
    },
    extract_params = function(mod, design, iteration, sample_size,
                               true_params, true_params_lookup, se = TRUE) {
      n_items <- design$n_items

      # mirt's coef() returns a named list:
      #   $Item_1 = matrix with rows: par, CI_2.5, CI_97.5 (if SE=TRUE) or just par (if SE=FALSE)
      #                         cols: a1, d (for Rasch)
      coefs <- mirt::coef(mod)

      result_rows <- vector("list", n_items)

      for (i in seq_len(n_items)) {
        item_name <- paste0("Item_", i)
        item_mat <- coefs[[item_name]]

        if (is.null(item_mat)) next

        # Use pre-indexed lookup instead of vector scan
        true_val <- true_params_lookup[[paste0(i, "_b")]]

        if (se) {
          # Rasch: mirt estimates d (intercept). Convert to b = -d (a = 1)
          d <- extract_one_param(item_mat, "d")

          result_rows[[i]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "b",
            true_value  = true_val,
            estimate    = -d$est,
            se          = d$se,
            ci_lower    = -d$ci_upper,
            ci_upper    = -d$ci_lower,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )
        } else {
          # Extract point estimate directly from coefficient matrix
          d_est <- item_mat["par", "d"]

          result_rows[[i]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "b",
            true_value  = true_val,
            estimate    = -d_est,
            se          = NA_real_,
            ci_lower    = NA_real_,
            ci_upper    = NA_real_,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }

      do.call(rbind, result_rows)
    }
  )
}


# =============================================================================
# 2PL Model Configuration
# =============================================================================

.model_2pl <- function() {
  list(
    param_schema = list(
      a = "positive numeric vector or matrix of length/rows n_items",
      b = "numeric vector of length n_items"
    ),
    response_format = "binary",
    mirt_itemtype = "2PL",
    validate_params = function(item_params, n_items) {
      if (is.null(item_params$a)) {
        stop("2PL model requires `a` (discrimination) in `item_params`.",
             call. = FALSE)
      }
      if (is.null(item_params$b)) {
        stop("2PL model requires `b` (difficulty) in `item_params`.",
             call. = FALSE)
      }

      # Length checks — a can be a vector or matrix (multidimensional)
      a_len <- if (is.matrix(item_params$a)) nrow(item_params$a) else length(item_params$a)
      if (a_len != n_items) {
        stop(
          "Length of `item_params$a` (", a_len,
          ") does not match `n_items` (", n_items, ").",
          call. = FALSE
        )
      }
      if (length(item_params$b) != n_items) {
        stop(
          "Length of `item_params$b` (", length(item_params$b),
          ") does not match `n_items` (", n_items, ").",
          call. = FALSE
        )
      }

      # Discrimination must be positive
      if (any(item_params$a <= 0)) {
        stop("All discrimination (`a`) values must be positive.", call. = FALSE)
      }

      item_params
    },
    build_true_params = function(design) {
      n_items <- design$n_items
      a <- design$item_params$a
      b <- design$item_params$b

      # 2PL: a and b per item
      rows <- list()
      rows[[1]] <- data.frame(
        item       = seq_len(n_items),
        param      = "a",
        true_value = a,
        stringsAsFactors = FALSE
      )
      rows[[2]] <- data.frame(
        item       = seq_len(n_items),
        param      = "b",
        true_value = b,
        stringsAsFactors = FALSE
      )

      do.call(rbind, rows)
    },
    convert_to_mirt = function(design) {
      # 2PL: d = -a * b
      a <- design$item_params$a
      b <- design$item_params$b
      d <- -a * b
      itemtype <- rep("2PL", design$n_items)

      list(a = a, d = d, itemtype = itemtype)
    },
    extract_params = function(mod, design, iteration, sample_size,
                               true_params, true_params_lookup, se = TRUE) {
      n_items <- design$n_items

      coefs <- mirt::coef(mod)

      result_rows <- vector("list", 2L * n_items)
      row_idx <- 0L

      for (i in seq_len(n_items)) {
        item_name <- paste0("Item_", i)
        item_mat <- coefs[[item_name]]

        if (is.null(item_mat)) next

        if (se) {
          # 2PL: a1 and d → convert d to b = -d/a
          a <- extract_one_param(item_mat, "a1")
          d <- extract_one_param(item_mat, "d")
          b <- convert_d_to_b(a, d)

          # a row — use pre-indexed lookup
          true_a <- true_params_lookup[[paste0(i, "_a")]]
          row_idx <- row_idx + 1L
          result_rows[[row_idx]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "a",
            true_value  = true_a,
            estimate    = a$est,
            se          = a$se,
            ci_lower    = a$ci_lower,
            ci_upper    = a$ci_upper,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )

          # b row — use pre-indexed lookup
          true_b <- true_params_lookup[[paste0(i, "_b")]]
          row_idx <- row_idx + 1L
          result_rows[[row_idx]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "b",
            true_value  = true_b,
            estimate    = b$est,
            se          = b$se,
            ci_lower    = b$ci_lower,
            ci_upper    = b$ci_upper,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )
        } else {
          # Extract point estimates directly from coefficient matrix
          a_est <- item_mat["par", "a1"]
          d_est <- item_mat["par", "d"]
          b_est <- -d_est / a_est

          # a row — use pre-indexed lookup
          true_a <- true_params_lookup[[paste0(i, "_a")]]
          row_idx <- row_idx + 1L
          result_rows[[row_idx]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "a",
            true_value  = true_a,
            estimate    = a_est,
            se          = NA_real_,
            ci_lower    = NA_real_,
            ci_upper    = NA_real_,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )

          # b row — use pre-indexed lookup
          true_b <- true_params_lookup[[paste0(i, "_b")]]
          row_idx <- row_idx + 1L
          result_rows[[row_idx]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "b",
            true_value  = true_b,
            estimate    = b_est,
            se          = NA_real_,
            ci_lower    = NA_real_,
            ci_upper    = NA_real_,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }

      result_rows <- result_rows[seq_len(row_idx)]
      do.call(rbind, result_rows)
    }
  )
}


# =============================================================================
# GRM (Graded Response Model) Configuration
# =============================================================================

.model_grm <- function() {
  list(
    param_schema = list(
      a = "positive numeric vector of length n_items",
      b = "numeric matrix (n_items x n_thresholds)"
    ),
    response_format = "polytomous",
    mirt_itemtype = "graded",
    validate_params = function(item_params, n_items) {
      if (is.null(item_params$a)) {
        stop("GRM model requires `a` (discrimination) in `item_params`.",
             call. = FALSE)
      }
      if (is.null(item_params$b)) {
        stop("GRM model requires `b` (threshold matrix) in `item_params`.",
             call. = FALSE)
      }
      if (!is.matrix(item_params$b)) {
        stop(
          "For GRM, `item_params$b` must be a matrix ",
          "(n_items rows x n_thresholds columns).",
          call. = FALSE
        )
      }
      if (length(item_params$a) != n_items) {
        stop(
          "Length of `item_params$a` (", length(item_params$a),
          ") does not match `n_items` (", n_items, ").",
          call. = FALSE
        )
      }
      if (nrow(item_params$b) != n_items) {
        stop(
          "Number of rows in `item_params$b` (", nrow(item_params$b),
          ") does not match `n_items` (", n_items, ").",
          call. = FALSE
        )
      }

      # Discrimination must be positive
      if (any(item_params$a <= 0)) {
        stop("All discrimination (`a`) values must be positive.", call. = FALSE)
      }

      item_params
    },
    build_true_params = function(design) {
      n_items <- design$n_items
      a <- design$item_params$a
      b <- design$item_params$b

      # GRM: a per item, plus indexed thresholds b1, b2, ...
      rows <- list()
      rows[[1]] <- data.frame(
        item       = seq_len(n_items),
        param      = "a",
        true_value = a,
        stringsAsFactors = FALSE
      )
      n_thresh <- ncol(b)
      for (k in seq_len(n_thresh)) {
        rows[[length(rows) + 1L]] <- data.frame(
          item       = seq_len(n_items),
          param      = paste0("b", k),
          true_value = b[, k],
          stringsAsFactors = FALSE
        )
      }

      do.call(rbind, rows)
    },
    convert_to_mirt = function(design) {
      # GRM: d_k = -a * b_k for each threshold column
      a <- design$item_params$a
      b <- design$item_params$b
      n_thresholds <- ncol(b)

      if (n_thresholds == 1L) {
        # Binary GRM (2 categories) — mirt's "graded" itemtype has a known bug
        # with single-threshold items. Since a GRM with 2 categories is
        # mathematically equivalent to a 2PL, use "2PL" itemtype.
        d <- -a * as.vector(b)
        itemtype <- rep("2PL", design$n_items)
      } else {
        d <- -a * b
        itemtype <- rep("graded", design$n_items)
      }

      list(a = a, d = d, itemtype = itemtype)
    },
    extract_params = function(mod, design, iteration, sample_size,
                               true_params, true_params_lookup, se = TRUE) {
      n_items <- design$n_items

      coefs <- mirt::coef(mod)

      result_rows <- vector("list", nrow(true_params))
      row_idx <- 0L

      for (i in seq_len(n_items)) {
        item_name <- paste0("Item_", i)
        item_mat <- coefs[[item_name]]

        if (is.null(item_mat)) next

        if (se) {
          # GRM: a1 and d1, d2, ... → convert d_k to b_k = -d_k/a
          a <- extract_one_param(item_mat, "a1")

          # a row — use pre-indexed lookup
          true_a <- true_params_lookup[[paste0(i, "_a")]]
          row_idx <- row_idx + 1L
          result_rows[[row_idx]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "a",
            true_value  = true_a,
            estimate    = a$est,
            se          = a$se,
            ci_lower    = a$ci_lower,
            ci_upper    = a$ci_upper,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )

          # Threshold rows
          n_thresh <- ncol(design$item_params$b)
          for (k in seq_len(n_thresh)) {
            d_name <- paste0("d", k)
            if (!d_name %in% colnames(item_mat)) next

            d <- extract_one_param(item_mat, d_name)
            b <- convert_d_to_b(a, d)

            # Use pre-indexed lookup for b_k
            true_b_k <- true_params_lookup[[paste0(i, "_b", k)]]
            row_idx <- row_idx + 1L
            result_rows[[row_idx]] <- data.frame(
              iteration   = iteration,
              sample_size = sample_size,
              item        = i,
              param       = paste0("b", k),
              true_value  = true_b_k,
              estimate    = b$est,
              se          = b$se,
              ci_lower    = b$ci_lower,
              ci_upper    = b$ci_upper,
              converged   = TRUE,
              stringsAsFactors = FALSE
            )
          }
        } else {
          # Extract point estimates directly from coefficient matrix
          a_est <- item_mat["par", "a1"]

          # a row — use pre-indexed lookup
          true_a <- true_params_lookup[[paste0(i, "_a")]]
          row_idx <- row_idx + 1L
          result_rows[[row_idx]] <- data.frame(
            iteration   = iteration,
            sample_size = sample_size,
            item        = i,
            param       = "a",
            true_value  = true_a,
            estimate    = a_est,
            se          = NA_real_,
            ci_lower    = NA_real_,
            ci_upper    = NA_real_,
            converged   = TRUE,
            stringsAsFactors = FALSE
          )

          # Threshold rows
          n_thresh <- ncol(design$item_params$b)
          for (k in seq_len(n_thresh)) {
            d_name <- paste0("d", k)
            if (!d_name %in% colnames(item_mat)) next

            d_est <- item_mat["par", d_name]
            b_est <- -d_est / a_est

            # Use pre-indexed lookup for b_k
            true_b_k <- true_params_lookup[[paste0(i, "_b", k)]]
            row_idx <- row_idx + 1L
            result_rows[[row_idx]] <- data.frame(
              iteration   = iteration,
              sample_size = sample_size,
              item        = i,
              param       = paste0("b", k),
              true_value  = true_b_k,
              estimate    = b_est,
              se          = NA_real_,
              ci_lower    = NA_real_,
              ci_upper    = NA_real_,
              converged   = TRUE,
              stringsAsFactors = FALSE
            )
          }
        }
      }

      result_rows <- result_rows[seq_len(row_idx)]
      do.call(rbind, result_rows)
    }
  )
}
