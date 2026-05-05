# Planning a Sample Size for an IRT Study

## What is irtsim for?

You are planning an item response theory (IRT) study and you need an
answer to one question: **how many examinees do I need?** Power-analysis
formulas exist for simple designs, but real assessments combine multiple
items, multiple parameters per item, missing-data mechanisms, and —
increasingly — model misspecification you cannot fully characterize a
priori. `irtsim` answers the question by simulation: you specify a
plausible data-generating model, sweep across candidate sample sizes,
fit the estimation model many times, and report the sample size at which
a chosen performance criterion (mean squared error, bias, coverage, …)
crosses a target threshold.

The package implements the 10-decision framework from Schroeders &
Gnambs (2025). This vignette walks you through the abridged version:
pick a design, pick sample sizes, run, interpret.

``` r

library(irtsim)
library(ggplot2)
```

## The pipeline

Three function calls, three S3 objects:

    irt_design()    →   irt_study()    →   irt_simulate()    →   summary() / plot()
    data-generating     conditions:        Monte Carlo         performance
    model               sample sizes,      iterations          criteria,
                        missing data                            recommendations

Every step is immutable: you can re-use a `design` across many `study`
objects, and re-run
[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
without rebuilding upstream state.

## Step 1 — Specify the data-generating model

[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
takes three required arguments: the IRT `model` (`"1PL"`, `"2PL"`, or
`"GRM"`), the number of items, and a list of true item parameters. There
are three common ways to supply the parameters.

### Path A: by hand

If you have specific values in mind — from a content blueprint, a prior
pilot, or a paper you are replicating — pass them directly.

``` r

design_byhand <- irt_design(
  model     = "2PL",
  n_items   = 10,
  item_params = list(
    a = c(0.8, 1.0, 1.1, 1.2, 1.3, 0.9, 1.4, 1.0, 1.2, 1.1),
    b = seq(-2, 2, length.out = 10)
  )
)
design_byhand
#> IRT Design
#>   Model:        2PL 
#>   Items:        10 items
#>   Theta dist:   normal 
#>   Factors:      1 
#>   a range:      [0.8, 1.4]
#>   b range:      [-2, 2]
```

### Path B: from a helper

For a typical I/O or education assessment, you usually want
discriminations drawn from a lognormal and difficulties spanning the
trait range.
[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md)
does this in one line:

``` r

set.seed(2026)
ip <- irt_params_2pl(
  n_items = 10,
  a_mean  = 0,    a_sd  = 0.25,   # log-normal: median a = 1
  b_mean  = 0,    b_sd  = 1,
  b_range = c(-2, 2)
)
design_helper <- irt_design(
  model       = "2PL",
  n_items     = 10,
  item_params = ip
)
```

Use
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
for graded-response items.

### Path C: from a prior fit

If you have already calibrated a similar instrument, treat the prior
estimates as the truth for planning purposes.
[`mirt::LSAT7`](https://philchalmers.github.io/mirt/reference/LSAT7.html)
ships with `mirt` and gives a clean, fast worked example.

``` r

prior_data <- mirt::expand.table(mirt::LSAT7)
prior_fit  <- mirt::mirt(prior_data, 1, "2PL", verbose = FALSE)
co <- mirt::coef(prior_fit, IRTpars = TRUE, simplify = TRUE)$items

design_prior <- irt_design(
  model       = "2PL",
  n_items     = nrow(co),
  item_params = list(a = co[, "a"], b = co[, "b"])
)
co
#>                a          b g u
#> Item.1 0.9879254 -1.8787456 0 1
#> Item.2 1.0808847 -0.7475160 0 1
#> Item.3 1.7058006 -1.0576962 0 1
#> Item.4 0.7651853 -0.6351358 0 1
#> Item.5 0.7357980 -2.5204102 0 1
```

For the rest of this vignette we use `design_helper` — a generic 2PL
with 10 items.

## Step 2 — Add study conditions

[`irt_study()`](https://sward1.github.io/irtsim/reference/irt_study.md)
adds the things that vary across the simulation grid: the sample sizes
you want to compare, optionally a missing-data mechanism, and optionally
an estimation model that differs from the data-generating model (model
misspecification studies). For a no-missing-data planning question, two
arguments are enough.

``` r

study <- irt_study(
  design       = design_helper,
  sample_sizes = c(100, 250, 500, 1000)
)
study
#> IRT Study
#>   Model:          2PL 
#>   Items:          10 
#>   Sample sizes:   100, 250, 500, 1000 
#>   Missing data:   none (complete data)
```

The four sample sizes span a typical planning range: 100 is small for a
10-item 2PL, 1000 should be ample. The simulation will give us the curve
in between.

## Step 3 — Run the simulation

[`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
is where the work happens: for each `(sample_size, iteration)` cell, it
generates data under `design`, fits the estimation model, and stores
parameter estimates. Two arguments control runtime: `iterations` (more
iterations → tighter Monte Carlo standard errors, longer runtime) and
`parallel` (off by default; turn on for production-scale runs).

``` r

results <- irt_simulate(
  study      = study,
  iterations = 50,
  seed       = 1,
  progress   = FALSE
)
```

A fixed `seed` is required — every simulation is reproducible by
default. `progress = FALSE` suppresses the cli progress bar so the
vignette renders cleanly. For real studies, leave it on.

`iterations = 50` is small — chosen here to keep the vignette build
fast. For production planning use 500–1000 iterations (or use
[`irt_iterations()`](https://sward1.github.io/irtsim/reference/irt_iterations.md)
to compute the count needed for a target Monte Carlo standard error).
Set `parallel = TRUE` to dispatch iterations across `future` workers;
reproducibility is preserved within mode.

## Step 4 — Interpret the results

[`summary()`](https://rdrr.io/r/base/summary.html) returns one row per
`(sample_size, item, parameter)` combination, with all performance
criteria attached.

``` r

res_summary <- summary(results,
                       criterion = c("mse", "bias", "rmse", "coverage"))
head(res_summary$item_summary)
#>   sample_size item param true_value         mse         bias      rmse
#> 1         100    1     a  1.1389961  0.32611950  0.065844261 0.5710687
#> 2         100    1     b -0.4082147  0.08559994 -0.023437985 0.2925747
#> 3         100    2     a  0.7634385  0.40789706  0.006876364 0.6386682
#> 4         100    2     b -0.7304333 22.44647061  0.678859473 4.7377706
#> 5         100    3     a  1.0354225  0.22133080  0.117301867 0.4704581
#> 6         100    3     b -0.2214366  0.07274307 -0.091348376 0.2697092
#>    coverage n_converged
#> 1 0.9791667          48
#> 2 0.9166667          48
#> 3 0.9375000          48
#> 4 0.7708333          48
#> 5 0.9791667          48
#> 6 0.9583333          48
```

Plot the criterion of interest against sample size to see where the
curve flattens. Each line is one item’s `b` (difficulty) MSE trajectory.

``` r

plot(res_summary,
     criterion = "mse",
     param     = "b",
     threshold = 0.05)
```

The dashed horizontal line is the planning threshold (here 0.05 — a
common default for parameter recovery). Items whose lines cross below
the threshold are adequately recovered at that sample size; items still
above need a larger N.

## Step 5 — Get a recommendation

[`recommended_n()`](https://sward1.github.io/irtsim/reference/recommended_n.md)
reads off the smallest sample size at which the criterion crosses the
threshold. The default rolls the per-item recommendations up to a single
number (the maximum, so no item is left under-powered):

``` r

n_rec <- recommended_n(res_summary,
                       criterion = "mse",
                       threshold = 0.05,
                       param     = "b")
#> Warning: No tested sample size meets mse <= 0.05 for some item/param combinations.
#> ℹ Affected: (item 5, param b) and (item 6, param b)
#> ℹ Aggregate returned as NA. Inspect `attr(result, "details")` for per-item
#>   values.
n_rec
#> [1] NA
#> attr(,"details")
#>    item param recommended_n criterion threshold
#> 1     1     b           250       mse      0.05
#> 2     2     b           500       mse      0.05
#> 3     3     b           250       mse      0.05
#> 4     4     b           250       mse      0.05
#> 5     5     b            NA       mse      0.05
#> 6     6     b            NA       mse      0.05
#> 7     7     b           250       mse      0.05
#> 8     8     b           250       mse      0.05
#> 9     9     b           500       mse      0.05
#> 10   10     b           250       mse      0.05
#> attr(,"aggregate")
#> [1] "max"
#> attr(,"criterion")
#> [1] "mse"
#> attr(,"threshold")
#> [1] 0.05
```

The scalar return is the headline answer. The `details` attribute
preserves the per-item table so you can inspect which items drove the
recommendation:

``` r

attr(n_rec, "details")
#>    item param recommended_n criterion threshold
#> 1     1     b           250       mse      0.05
#> 2     2     b           500       mse      0.05
#> 3     3     b           250       mse      0.05
#> 4     4     b           250       mse      0.05
#> 5     5     b            NA       mse      0.05
#> 6     6     b            NA       mse      0.05
#> 7     7     b           250       mse      0.05
#> 8     8     b           250       mse      0.05
#> 9     9     b           500       mse      0.05
#> 10   10     b           250       mse      0.05
```

If you want a less conservative summary, pass `aggregate = "mean"` or
`aggregate = "median"`. To get the legacy per-item data frame back, use
`aggregate = "none"`.

## Where to next

- **Reproduce the paper.** The four `paper-example-*` vignettes walk
  through the worked examples from Schroeders & Gnambs (2025).
- **Missing data.** Pass `missing = "mcar"`, `"mar"`, `"booklet"`, or
  `"linking"` to
  [`irt_study()`](https://sward1.github.io/irtsim/reference/irt_study.md).
  See
  [`?irt_study`](https://sward1.github.io/irtsim/reference/irt_study.md)
  and the `paper-example-2-mcar` vignette.
- **Model misspecification.** Pass `estimation_model` to
  [`irt_study()`](https://sward1.github.io/irtsim/reference/irt_study.md)
  to fit a different model than you generated under. See
  `paper-example-1b-misspecification`.
- **Custom criteria.** Pass `criterion_fn` to
  [`summary()`](https://rdrr.io/r/base/summary.html) to compute a
  user-defined performance criterion alongside the built-ins.
- **Parallel execution.** Set `parallel = TRUE` in
  [`irt_simulate()`](https://sward1.github.io/irtsim/reference/irt_simulate.md)
  and configure a
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  for the workers. Reproducibility is preserved within mode.

## References

Schroeders, U., & Gnambs, T. (2025). Sample size planning for item
response models: A tutorial for the quantitative researcher.
*Methodology, 21*(1), 1–28. <https://doi.org/10.1177/25152459251314798>
