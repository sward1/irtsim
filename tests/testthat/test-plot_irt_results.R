# test-plot_irt_results.R
# TDD tests for plot.irt_results() and plot.summary_irt_results()
#
# Design decisions:
#   - plot.irt_results() is the primary user-facing method; calls summary()
#     internally, then builds the ggplot
#   - plot.summary_irt_results() also exists for users who already have a summary
#   - Default plot: criterion value (y) by sample_size (x), faceted by item,
#     with lines per param. Default criterion is "rmse".
#   - Optional threshold argument draws a horizontal reference line
#   - Returns a ggplot object (invisible)
#
# These tests use mock objects — no simulation required.


# =============================================================================
# Helper: reuse the mock builders from test-summary_irt_results.R and
# test-recommended_n.R. We define them locally for test isolation.
# =============================================================================

make_mock_results_for_plot <- function(
  n_items = 3,
  sample_sizes = c(100, 200, 500),
  iterations = 10,
  model = "2PL",
  params = c("a", "b")
) {
  rows <- list()
  for (ss in sample_sizes) {
    for (item in seq_len(n_items)) {
      for (param in params) {
        tv <- if (param == "a") 1.0 else seq(-1, 1, length.out = n_items)[item]

        set.seed(item * 100 + ss + match(param, params))
        ests <- tv + rnorm(iterations, mean = 0.02, sd = 0.15)
        ses  <- rep(0.15, iterations)

        rows[[length(rows) + 1]] <- data.frame(
          iteration   = seq_len(iterations),
          sample_size = rep(ss, iterations),
          item        = rep(item, iterations),
          param       = rep(param, iterations),
          true_value  = rep(tv, iterations),
          estimate    = ests,
          se          = ses,
          ci_lower    = ests - 1.96 * ses,
          ci_upper    = ests + 1.96 * ses,
          converged   = rep(TRUE, iterations),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  item_results <- do.call(rbind, rows)

  theta_results <- data.frame(
    iteration   = rep(seq_len(iterations), each = length(sample_sizes)),
    sample_size = rep(sample_sizes, times = iterations),
    theta_cor   = runif(iterations * length(sample_sizes), 0.8, 0.99),
    theta_rmse  = runif(iterations * length(sample_sizes), 0.2, 0.5),
    converged   = TRUE,
    stringsAsFactors = FALSE
  )

  design_stub <- list(
    model = model,
    n_items = n_items,
    item_params = list(
      a = rep(1.0, n_items),
      b = seq(-1, 1, length.out = n_items)
    )
  )
  class(design_stub) <- "irt_design"

  study_stub <- list(
    design = design_stub,
    sample_sizes = sample_sizes,
    missing_type = "none"
  )
  class(study_stub) <- "irt_study"

  structure(
    list(
      item_results  = item_results,
      theta_results = theta_results,
      study         = study_stub,
      iterations    = iterations,
      seed          = 42L,
      elapsed       = 1.0
    ),
    class = "irt_results"
  )
}

make_mock_summary_for_plot <- function(
  n_items = 3,
  sample_sizes = c(100, 200, 500),
  model = "2PL",
  iterations = 100
) {
  rows <- list()
  for (ss in sample_sizes) {
    for (item in seq_len(n_items)) {
      for (param in c("a", "b")) {
        # Decreasing RMSE with larger N
        rmse_base <- 0.5 * (100 / ss)^0.5
        rows[[length(rows) + 1]] <- data.frame(
          sample_size  = ss,
          item         = item,
          param        = param,
          true_value   = ifelse(param == "a", 1.0,
                                seq(-1, 1, length.out = n_items)[item]),
          bias         = 0.05 * (100 / ss),
          empirical_se = 0.3 * (100 / ss)^0.5,
          mse          = rmse_base^2,
          rmse         = rmse_base,
          coverage     = 0.80 + 0.15 * (1 - 100 / ss),
          mcse_bias    = 0.02,
          mcse_mse     = 0.01,
          n_converged  = iterations,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  item_summary <- do.call(rbind, rows)

  theta_summary <- data.frame(
    sample_size = sample_sizes,
    mean_cor    = c(0.85, 0.90, 0.95),
    sd_cor      = rep(0.02, 3),
    mean_rmse   = c(0.40, 0.30, 0.20),
    sd_rmse     = rep(0.03, 3),
    n_converged = rep(iterations, 3),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      item_summary  = item_summary,
      theta_summary = theta_summary,
      iterations    = iterations,
      seed          = 42L,
      model         = model
    ),
    class = "summary_irt_results"
  )
}


# =============================================================================
# Section 1: plot.irt_results — returns a ggplot object
# =============================================================================

test_that("plot.irt_results returns a ggplot object", {
  results <- make_mock_results_for_plot()
  p <- plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("plot.irt_results returns invisibly", {
  results <- make_mock_results_for_plot()
  out <- withVisible(plot(results))
  expect_false(out$visible)
})


# =============================================================================
# Section 2: plot.irt_results — default behavior
# =============================================================================

test_that("plot.irt_results default criterion is rmse", {
  results <- make_mock_results_for_plot()
  p <- plot(results)

  # The y-axis label should reference RMSE
  expect_true(grepl("[Rr][Mm][Ss][Ee]", p$labels$y, ignore.case = TRUE))
})

test_that("plot.irt_results x-axis is sample_size", {
  results <- make_mock_results_for_plot()
  p <- plot(results)

  # x mapping references sample_size via .data[["sample_size"]]
  # which is stored as a call, not a symbol — deparse to check
  x_expr <- deparse(rlang::quo_get_expr(p$mapping$x))
  expect_true(grepl("sample_size", x_expr))
})

test_that("plot.irt_results includes a line geom", {
  results <- make_mock_results_for_plot()
  p <- plot(results)

  # Check that at least one layer is a line or path geom
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1],
                         character(1))
  has_line <- any(grepl("Line|Path", geom_classes))
  expect_true(has_line)
})

test_that("plot.irt_results includes a point geom", {
  results <- make_mock_results_for_plot()
  p <- plot(results)

  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1],
                         character(1))
  has_point <- any(grepl("Point", geom_classes))
  expect_true(has_point)
})


# =============================================================================
# Section 3: plot.irt_results — criterion argument
# =============================================================================

test_that("plot.irt_results accepts criterion argument", {
  results <- make_mock_results_for_plot()
  p <- plot(results, criterion = "bias")

  expect_true(grepl("[Bb]ias", p$labels$y, ignore.case = TRUE))
})

test_that("plot.irt_results errors on invalid criterion", {
  results <- make_mock_results_for_plot()
  expect_error(plot(results, criterion = "r_squared"))
})


# =============================================================================
# Section 4: plot.irt_results — param filter
# =============================================================================

test_that("plot.irt_results param argument filters plotted parameters", {
  results <- make_mock_results_for_plot(params = c("a", "b"))
  p <- plot(results, param = "b")

  # Build the plot to access computed data
  built <- ggplot2::ggplot_build(p)

  # The underlying data should only contain param = "b" rows
  # (Facets or groups should reflect the filtering)
  plot_data <- built$data[[1]]

  # Should have fewer data points than full plot
  p_full <- plot(results)
  built_full <- ggplot2::ggplot_build(p_full)
  expect_true(nrow(built$data[[1]]) <= nrow(built_full$data[[1]]))
})


# =============================================================================
# Section 5: plot.irt_results — item filter
# =============================================================================

test_that("plot.irt_results item argument filters plotted items", {
  results <- make_mock_results_for_plot(n_items = 5)
  p <- plot(results, item = c(1, 3))

  built <- ggplot2::ggplot_build(p)

  p_full <- plot(results)
  built_full <- ggplot2::ggplot_build(p_full)

  expect_true(nrow(built$data[[1]]) < nrow(built_full$data[[1]]))
})


# =============================================================================
# Section 6: plot.irt_results — threshold line
# =============================================================================

test_that("plot.irt_results with threshold adds a horizontal reference line", {
  results <- make_mock_results_for_plot()
  p <- plot(results, threshold = 0.20)

  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1],
                         character(1))
  has_hline <- any(grepl("Hline", geom_classes))
  expect_true(has_hline)
})

test_that("plot.irt_results without threshold has no hline", {
  results <- make_mock_results_for_plot()
  p <- plot(results)

  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1],
                         character(1))
  has_hline <- any(grepl("Hline", geom_classes))
  expect_false(has_hline)
})


# =============================================================================
# Section 7: plot.irt_results — faceting
# =============================================================================

test_that("plot.irt_results facets by item when multiple items present", {
  results <- make_mock_results_for_plot(n_items = 3)
  p <- plot(results)

  # Check that facet specification exists
  facet_class <- class(p$facet)[1]
  expect_true(facet_class != "FacetNull",
              info = "Expected faceting when multiple items present")
})

test_that("plot.irt_results with single item has no faceting", {
  results <- make_mock_results_for_plot(n_items = 1, params = "b")
  p <- plot(results)

  facet_class <- class(p$facet)[1]
  expect_equal(facet_class, "FacetNull")
})


# =============================================================================
# Section 8: plot.summary_irt_results — convenience method
# =============================================================================

test_that("plot.summary_irt_results returns a ggplot object", {
  s <- make_mock_summary_for_plot()
  p <- plot(s)
  expect_s3_class(p, "ggplot")
})

test_that("plot.summary_irt_results accepts same arguments as plot.irt_results", {
  s <- make_mock_summary_for_plot()

  p1 <- plot(s, criterion = "bias")
  expect_s3_class(p1, "ggplot")

  p2 <- plot(s, threshold = 0.20)
  expect_s3_class(p2, "ggplot")

  p3 <- plot(s, param = "b")
  expect_s3_class(p3, "ggplot")
})

test_that("plot.summary_irt_results returns invisibly", {
  s <- make_mock_summary_for_plot()
  out <- withVisible(plot(s))
  expect_false(out$visible)
})


# =============================================================================
# Section 9: plot.irt_results — 1PL model (single param, no facets needed)
# =============================================================================

test_that("plot.irt_results works for 1PL model with single param", {
  results <- make_mock_results_for_plot(
    n_items = 5, model = "1PL", params = "b"
  )
  p <- plot(results)
  expect_s3_class(p, "ggplot")
})


# =============================================================================
# Section 10: plot.irt_results — GRM model (multiple threshold params)
# =============================================================================

test_that("plot.irt_results works for GRM model with threshold params", {
  results <- make_mock_results_for_plot(
    n_items = 3, model = "GRM", params = c("a", "b1", "b2", "b3")
  )
  p <- plot(results)
  expect_s3_class(p, "ggplot")
})


# =============================================================================
# Section 11: plot.irt_results — input validation
# =============================================================================

test_that("plot.irt_results errors on non-irt_results input", {
  expect_error(plot.irt_results(list(a = 1)))
})

test_that("plot.summary_irt_results errors on non-summary_irt_results input", {
  expect_error(plot.summary_irt_results(list(a = 1)))
})
