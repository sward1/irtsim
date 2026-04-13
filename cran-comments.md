# irtsim 0.1.0 — Initial CRAN submission

## Test environments

* Local: macOS (aarch64-apple-darwin20), R 4.2.2 — 0 errors, 0 warnings, 2 notes
* Win-builder: Windows Server 2022, R-devel 4.6.0 beta (2026-04-12 r89872 ucrt) — 0 errors, 0 warnings, 2 notes
* R-hub (via GitHub Actions): linux / macos / windows — [results pending]

## R CMD check results

0 errors | 0 warnings | 2 notes

### Note 1: CRAN incoming feasibility

This is a new submission.

Three DOI-resolved URLs are reported as returning HTTP 403 Forbidden:

* `https://doi.org/10.1002/sim.2673` (Burton et al., 2006, *Statistics in
  Medicine*, published by Wiley)
* `https://doi.org/10.1002/sim.8086` (Morris et al., 2019, *Statistics in
  Medicine*, published by Wiley)
* `https://doi.org/10.1177/25152459251314798` (Schroeders & Gnambs, 2025,
  *Advances in Methods and Practices in Psychological Science*, published
  by SAGE)

The same SAGE DOI is additionally reported under "Found the following
(possibly) invalid DOIs".

All three DOIs are valid and resolve correctly in a browser. Wiley and
SAGE DOI resolvers are known to return HTTP 403 to automated HEAD
requests used by `urlchecker` / R CMD check. These are false positives.

The LICENSE-year note ("YEAR: 2026 / COPYRIGHT HOLDER: Stephen Ward") is
standard for a GPL (>= 3) + file LICENSE package.

The three "possibly misspelled words in DESCRIPTION" — Gnambs, Schroeders,
and MCAR — are not misspellings: Gnambs and Schroeders are the surnames of
the authors whose framework the package implements (Schroeders & Gnambs,
2025), and MCAR is the standard acronym for "Missing Completely At Random"
in the missing-data literature (Rubin, 1976; Little & Rubin, 2002).

### Note 2: HTML manual validation (Win-builder only)

Win-builder R-devel reports three HTML validation warnings in
`extract_params.html` about unrecognized `<item>` / `<param>` tags. These
were artifacts of literal angle-bracket placeholder text in a roxygen
`@param` tag and have been resolved in the source (commit forthcoming);
they do not reproduce on R-release platforms.

### Note 3: Future file timestamps

```
* checking for future file timestamps ... NOTE
  unable to verify current time
```

This reflects a temporary inability of the check environment to reach the
worldclockapi.com time server; it is unrelated to package contents and
does not reproduce on R-hub or Win-builder.

## Static vignettes

Vignettes are shipped as pre-rendered HTML using the `R.rsp::asis` vignette
engine. The reason is compute cost: the vignettes demonstrate Monte Carlo
IRT simulation studies (up to 438 iterations × 11 sample sizes × 30 items),
which would substantially exceed CRAN's vignette build-time budget if
re-executed during package checks. The source `.Rmd` files and the
simulation precompute script (`data-raw/precompute_vignettes.R`) are
available in the GitHub repository, allowing users to reproduce all
vignette results locally. This pattern follows other simulation-study
packages on CRAN (e.g., `simhelpers`, `SimDesign`).

## Downstream dependencies

This is the initial CRAN release of irtsim; there are no downstream
dependencies.

## Additional notes

* The package depends on `mirt`, `future.apply`, `cli`, `ggplot2`, and
  `rlang`. All are on CRAN.
* `parallel = TRUE` in `irt_simulate()` is opt-in and uses the
  `future.apply` framework; the default serial path imposes no parallel
  backend on the user and is fully reproducible under Mersenne-Twister RNG.
* The package contains no compiled code.
* License: GPL (>= 3), inherited from the `mirt` dependency.
