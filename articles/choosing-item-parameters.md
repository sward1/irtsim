# Choosing Item Parameters for Sample-Size Planning

## Why this matters

Every `irtsim` sample-size recommendation rests on the item parameters
you hand to
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md).
Discrimination (`a`) and difficulty (`b`) values determine the
test-information curve, which in turn determines how quickly each
criterion (mean squared error, bias, coverage, …) tightens as N grows.
Wrong parameters do not produce wrong code — they produce a
plausible-looking N that is too small or too large for the test you
actually plan to administer.

The getting-started vignette
([`vignette("irtsim")`](https://sward1.github.io/irtsim/articles/irtsim.md))
sketches three ways to specify parameters: by hand, via a helper, or
from a prior fit. This vignette is the deeper reference for **applied**
users who need a worked example for each path plus a reference table of
typical values for common assessment domains.

``` r

library(irtsim)
```

## Path A — Import from a prior fit

If you already have a calibrated instrument (or a similar one), the
fastest planning input is its parameter estimates. Two common cases: you
have a saved `mirt` model object, or you have a parameter table in a CSV
/ Excel file.

### A1. From a `mirt` fit object

`mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)$items` returns a
matrix in the IRT parameterization that
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
expects. For a 2PL fit the columns are `a`, `b`, `g` (guessing, 0 for
2PL), `u` (upper, 1 for 2PL); pull `a` and `b` and you are done.

``` r

prior_data <- mirt::expand.table(mirt::LSAT7)
prior_fit  <- mirt::mirt(prior_data, 1, "2PL", verbose = FALSE)

co <- mirt::coef(prior_fit, IRTpars = TRUE, simplify = TRUE)$items
co
#>                a          b g u
#> Item.1 0.9879254 -1.8787456 0 1
#> Item.2 1.0808847 -0.7475160 0 1
#> Item.3 1.7058006 -1.0576962 0 1
#> Item.4 0.7651853 -0.6351358 0 1
#> Item.5 0.7357980 -2.5204102 0 1

design_from_fit <- irt_design(
  model       = "2PL",
  n_items     = nrow(co),
  item_params = list(a = co[, "a"], b = co[, "b"])
)
design_from_fit
#> IRT Design
#>   Model:        2PL 
#>   Items:        5 items
#>   Theta dist:   normal 
#>   Factors:      1 
#>   a range:      [0.736, 1.706]
#>   b range:      [-2.52, -0.635]
```

**If your saved fit only has slope-intercept (`d`) values.** Some older
`mirt` workflows store `coef(fit, IRTpars = FALSE)` output, which gives
`a1` and `d` rather than `a` and `b`. Convert with the standard identity
`b = -d / a`:

``` r

co_si <- mirt::coef(prior_fit, IRTpars = FALSE, simplify = TRUE)$items
a_vec <- co_si[, "a1"]
b_vec <- -co_si[, "d"] / a_vec
all.equal(b_vec, co[, "b"])  # same as IRTpars=TRUE path
#> [1] TRUE
```

For graded-response (GRM) fits, the `b` columns are the category
thresholds (`b1`, `b2`, …). Pass them as a matrix to
`irt_design(model = "GRM", item_params = list(a = ..., b = ...))`.

### A2. From a CSV or Excel parameter table

Technical manuals and prior calibration reports often publish parameter
tables as CSV or Excel. Read them in and reshape to the list form
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
expects.

``` r

# Imagine this CSV came from a prior calibration report.
csv_text <- "
item,a,b
i01,1.05,-1.80
i02,0.92,-0.95
i03,1.18,-0.20
i04,1.40, 0.45
i05,0.88, 1.15
i06,1.22, 1.95
"
params_df <- read.csv(text = csv_text, strip.white = TRUE)

design_from_csv <- irt_design(
  model       = "2PL",
  n_items     = nrow(params_df),
  item_params = list(a = params_df$a, b = params_df$b)
)
design_from_csv
#> IRT Design
#>   Model:        2PL 
#>   Items:        6 items
#>   Theta dist:   normal 
#>   Factors:      1 
#>   a range:      [0.88, 1.4]
#>   b range:      [-1.8, 1.95]
```

For Excel, swap [`read.csv()`](https://rdrr.io/r/utils/read.table.html)
for
[`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
(or your preferred reader); the rest of the pattern is identical. The
only contract
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md)
enforces is that `a` and `b` are numeric vectors of length `n_items`
(matrix `b` for GRM).

## Path B — Domain-typical preset values

When you do **not** have a prior fit and are planning a brand-new test,
the next-best input is a distribution drawn from values typical of your
assessment domain. The reference table below summarises four common
domains; the calls that follow show how to instantiate each with
[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md)
(or
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
for polytomous clinical scales).

A worked example per domain — same `n_items = 20`, distinct distribution
arguments:

``` r

# Cognitive ability — high discriminations, broad difficulty range
ip_cog <- irt_params_2pl(
  n_items = 20,
  a_mean  = 0.20, a_sd  = 0.30,   # log-normal: median a ~ 1.22
  b_mean  = 0,    b_sd  = 1.20,
  seed    = 1
)
summary(ip_cog$a); summary(ip_cog$b)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.6285  1.0893  1.3606  1.3372  1.5338  1.9711
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -2.387222 -0.479359 -0.065960 -0.007766  0.786796  1.630415
```

``` r

# Personality — moderate discriminations, broader trait coverage
ip_pers <- irt_params_2pl(
  n_items = 20,
  a_mean  = -0.20, a_sd = 0.30,   # log-normal: median a ~ 0.82
  b_mean  = 0,     b_sd = 1.50,
  seed    = 1
)
summary(ip_pers$a); summary(ip_pers$b)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.4213  0.7302  0.9121  0.8964  1.0282  1.3213
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -2.984028 -0.599199 -0.082450 -0.009707  0.983495  2.038019
```

``` r

# Clinical (PROMIS-style GRM, 5 categories) — high discriminations,
# trait-spanning thresholds
ip_clin <- irt_params_grm(
  n_items      = 20,
  n_categories = 5,
  a_mean       = 0.10, a_sd = 0.30,   # median a ~ 1.10
  b_mean       = 0,    b_sd = 1.20,
  seed         = 1
)
summary(ip_clin$a)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.5687  0.9856  1.2311  1.2100  1.3879  1.7835
```

``` r

# Achievement / large-scale educational — moderate discriminations,
# difficulty distribution centered on the cut score (here 0)
ip_ach <- irt_params_2pl(
  n_items = 20,
  a_mean  = 0,    a_sd  = 0.25,   # median a = 1.00
  b_mean  = 0,    b_sd  = 1.00,
  seed    = 1
)
summary(ip_ach$a); summary(ip_ach$b)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.5748  0.9089  1.0941  1.0737  1.2090  1.4901
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -1.989352 -0.399466 -0.054967 -0.006472  0.655663  1.358680
```

Each list goes straight into
[`irt_design()`](https://sward1.github.io/irtsim/reference/irt_design.md):

``` r

design_cog <- irt_design(model = "2PL", n_items = 20, item_params = ip_cog)
design_cog
#> IRT Design
#>   Model:        2PL 
#>   Items:        20 items
#>   Theta dist:   normal 
#>   Factors:      1 
#>   a range:      [0.629, 1.971]
#>   b range:      [-2.387, 1.63]
```

## Path C — Hypothesized / content-based

When you have neither a prior fit nor a tight domain prior, you *do*
still have content knowledge: item-review judgements, the target trait
range, expected pass rates. Translate those into distribution arguments.

A worked example. Suppose you are planning a 12-item screener for a
narrow construct. Content review suggests:

- Items target the **mid-to-upper trait range** (most respondents will
  pass the easiest items, the hardest items will separate the high end).
  Translation: `b_dist = "even"`, `b_range = c(-0.5, 2)`.
- Items came from a single SME team with consistent quality; expect
  **moderate, narrow-spread** discriminations. Translation:
  `a_mean = 0`, `a_sd = 0.20` (median a = 1, narrow lognormal).

``` r

ip_screener <- irt_params_2pl(
  n_items = 12,
  a_mean  = 0,    a_sd  = 0.20,
  b_dist  = "even", b_range = c(-0.5, 2),
  seed    = 1
)
ip_screener
#> $a
#>  [1] 0.8822403 1.0374115 0.8460932 1.3758286 1.0681216 0.8486625 1.1023958
#>  [8] 1.1591245 1.1220488 0.9407502 1.3530431 1.0810888
#> 
#> $b
#>  [1] -0.50000000 -0.27272727 -0.04545455  0.18181818  0.40909091  0.63636364
#>  [7]  0.86363636  1.09090909  1.31818182  1.54545455  1.77272727  2.00000000

design_screener <- irt_design(
  model       = "2PL",
  n_items     = 12,
  item_params = ip_screener
)
design_screener
#> IRT Design
#>   Model:        2PL 
#>   Items:        12 items
#>   Theta dist:   normal 
#>   Factors:      1 
#>   a range:      [0.846, 1.376]
#>   b range:      [-0.5, 2]
```

The trick is to keep the translation explicit: each distribution
argument should map back to a content-review statement you can defend.
If you cannot justify `b_sd = 1.5` over `b_sd = 1.0`, run the
sample-size simulation under both and report the more conservative
recommendation.

## Reference table

Typical parameter ranges for four common assessment domains. These are
**starting points, not standards** — confirm against your instrument’s
technical manual or a published calibration in the same domain before
locking a planning N.

| Domain | Model | Discrimination (`a`) | Difficulty / threshold (`b`) | Source |
|----|----|----|----|----|
| **Cognitive ability** (e.g., ASVAB-type, ability tests) | 2PL or 3PL | log-normal, `meanlog ≈ 0.0–0.4`, `sdlog ≈ 0.30–0.50` (median a ~ 1.0–1.5) | Normal, mean 0, SD 1.0–1.5 | Hambleton & Swaminathan (1985); Hambleton, Swaminathan, & Rogers (1991) |
| **Personality** (Big Five, narrow trait scales) | 2PL or GRM | log-normal, `meanlog ≈ −0.3 to 0.0`, `sdlog ≈ 0.25–0.40` (median a ~ 0.7–1.2) | Normal, mean 0, SD 1.2–1.8 (broader trait spread) | Reise & Waller (2009) |
| **Clinical / health** (e.g., PROMIS, depression / anxiety scales) | GRM, 4–7 categories | log-normal, `meanlog ≈ 0.0–0.4`, `sdlog ≈ 0.30–0.40` (median a ~ 1.0–1.5) | Normal, mean 0, SD 1.0–1.5; thresholds ordered within item | Embretson & Reise (2000); PROMIS technical reports |
| **Achievement / large-scale educational** (e.g., NAEP, K-12 assessments) | 2PL or 3PL | log-normal, `meanlog ≈ 0.0–0.2`, `sdlog ≈ 0.20–0.30` (median a ~ 0.8–1.2) | Normal, mean 0, SD 1.0; centred on the cut score | Mislevy & Bock (1990); typical large-scale assessment manuals |

**Caveats.** (1) Ranges above are rules of thumb drawn from the cited
literature; exact values vary by population, content domain, and
calibration sample. (2) 3PL guessing parameters (relevant for
multiple-choice cognitive items) and the upper-asymptote (4PL) are not
yet supported in `irtsim` — planned for v0.2.0. (3) When unsure,
simulate under both an optimistic and a pessimistic parameter assumption
and report the more conservative N.

## When the reference table is not enough

If you find yourself typing the same six numbers from the reference
table at the start of every planning project, that is a signal — it
suggests a future helper of the form
`irt_params_typical(domain, n_items, ...)` would be worth shipping.
Until that helper exists (planned consideration in a future release once
the 3PL / PCM / GPCM helpers land), the explicit
[`irt_params_2pl()`](https://sward1.github.io/irtsim/reference/irt_params_2pl.md)
/
[`irt_params_grm()`](https://sward1.github.io/irtsim/reference/irt_params_grm.md)
calls above are the recommended pattern.

## References

Embretson, S. E., & Reise, S. P. (2000). *Item response theory for
psychologists*. Lawrence Erlbaum Associates.

Hambleton, R. K., & Swaminathan, H. (1985). *Item response theory:
Principles and applications*. Kluwer-Nijhoff.

Hambleton, R. K., Swaminathan, H., & Rogers, H. J. (1991). *Fundamentals
of item response theory*. Sage.

Mislevy, R. J., & Bock, R. D. (1990). *BILOG 3: Item analysis and test
scoring with binary logistic models* (2nd ed.). Scientific Software.

Reise, S. P., & Waller, N. G. (2009). Item response theory and clinical
measurement. *Annual Review of Clinical Psychology, 5*(1), 27–48.
<https://doi.org/10.1146/annurev.clinpsy.032408.153553>

Schroeders, U., & Gnambs, T. (2025). Sample size planning for item
response models: A tutorial for the quantitative researcher.
*Methodology, 21*(1), 1–28. <https://doi.org/10.1177/25152459251314798>
