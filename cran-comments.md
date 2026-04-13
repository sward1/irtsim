# irtsim 0.1.0 — Initial CRAN submission

## Test environments

* Local: macOS (aarch64-apple-darwin20), R 4.2.2 — 0 errors, 0 warnings, 1 note
* Win-builder: Windows Server, R-devel 4.6.0 beta (2026-04-12 r89872 ucrt) — 0 errors, 0 warnings, 1 note
* Win-builder: Windows Server, R-release 4.5.3 (2026-03-11 ucrt) — 0 errors, 0 warnings, 1 note
* Win-builder: Windows Server, R-oldrelease 4.4.3 Patched (2026-02-12 r89426 ucrt) — 0 errors, 0 warnings, 1 note

## R CMD check results

0 errors | 0 warnings | 1 note

### Note: CRAN incoming feasibility (new submission)

This is a new submission.

**Possibly misspelled words in DESCRIPTION** — all are legitimate
domain vocabulary, not misspellings:

* `Gnambs`, `Schroeders` — surnames of the authors whose methodological
  framework the package implements (Schroeders & Gnambs, 2025).
* `IRT` — standard acronym for "Item Response Theory".
* `MCAR` — standard acronym for "Missing Completely At Random"
  (Rubin, 1976; Little & Rubin, 2002).
* `MSE`, `RMSE` — standard acronyms for "Mean Squared Error" and
  "Root Mean Squared Error" (Morris et al., 2019).
* `missingness` — standard term in the missing-data literature.

**DOI URLs reported as HTTP 403 Forbidden** — all three DOIs are
valid and resolve correctly in a browser. Wiley and SAGE DOI resolvers
are known to return HTTP 403 to automated HEAD requests used by
`urlchecker` / R CMD check. These are false positives.

* `https://doi.org/10.1002/sim.2673` (Burton et al., 2006, *Statistics
  in Medicine*, Wiley)
* `https://doi.org/10.1002/sim.8086` (Morris et al., 2019, *Statistics
  in Medicine*, Wiley)
* `https://doi.org/10.1177/25152459251314798` (Schroeders & Gnambs,
  2025, *Advances in Methods and Practices in Psychological Science*,
  SAGE)

The same SAGE DOI is additionally reported under "Found the following
(possibly) invalid DOIs".

**LICENSE-year note** ("YEAR: 2026 / COPYRIGHT HOLDER: Stephen Ward")
is standard for a GPL (>= 3) + file LICENSE package.

### Additional ignorable note on the local macOS environment only

```
* checking for future file timestamps ... NOTE
  unable to verify current time
```

This reflects a temporary inability of the local check environment to
reach the `worldclockapi.com` time server; it is unrelated to package
contents and does not reproduce on Win-builder.

## Static vignettes

Vignettes are shipped as pre-rendered HTML using the `R.rsp::asis`
vignette engine. The reason is compute cost: the vignettes demonstrate
Monte Carlo IRT simulation studies (up to 438 iterations × 11 sample
sizes × 30 items), which would substantially exceed CRAN's vignette
build-time budget if re-executed during package checks. The source
`.Rmd` files and the simulation precompute script
(`data-raw/precompute_vignettes.R`) are available in the GitHub
repository, allowing users to reproduce all vignette results locally.
This pattern follows other simulation-study packages on CRAN (e.g.,
`simhelpers`, `SimDesign`).

## Downstream dependencies

This is the initial CRAN release of irtsim; there are no downstream
dependencies.

## Additional notes

* The package depends on `mirt`, `future.apply`, `cli`, `ggplot2`, and
  `rlang`. All are on CRAN.
* `parallel = TRUE` in `irt_simulate()` is opt-in and uses the
  `future.apply` framework; the default serial path imposes no parallel
  backend on the user and is fully reproducible under Mersenne-Twister
  RNG.
* The package contains no compiled code.
* License: GPL (>= 3), inherited from the `mirt` dependency.
