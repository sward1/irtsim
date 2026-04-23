# irtsim

[![CRAN status](https://www.r-pkg.org/badges/version/irtsim)](https://CRAN.R-project.org/package=irtsim)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/irtsim)](https://CRAN.R-project.org/package=irtsim)
[![License: GPL v3](https://img.shields.io/badge/License-GPL_v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Monte Carlo simulation-based sample-size planning for item response theory.

## Installation

```r
install.packages("irtsim")
```

## Quick start

```r
library(irtsim)

design  <- irt_design(
  model = "1PL", n_items = 20,
  item_params = list(b = seq(-2, 2, length.out = 20))
)
study   <- irt_study(design, sample_sizes = c(200, 400, 800))
results <- irt_simulate(study, iterations = 100, seed = 42)

summary(results)
plot(results)
recommended_n(summary(results), criterion = "rmse", threshold = 0.20)
```

## What it supports

- **Models:** 1PL, 2PL, graded response model (GRM)
- **Missingness:** MCAR, MAR, booklet designs, linking designs
- **Criteria:** MSE, bias, RMSE, SE, coverage, Monte Carlo SE, plus user-defined via `criterion_fn`
- **Misspecification:** generate with one model, fit with another
- **Parallelization:** `irt_simulate(parallel = TRUE)` via `future.apply`

## Reference

Schroeders, U., & Gnambs, T. (2025). Sample size planning in item response theory: A 10-decision framework. *Advances in Methods and Practices in Psychological Science.* <https://doi.org/10.1177/25152459251314798>

## License

GPL (>= 3)
