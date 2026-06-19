# irtsim 0.2.0 — Feature release

This is a feature release. Headline changes:

* Completed the `irt_params_*` helper family across all registered IRT
  models: added `irt_params_1pl()`, `irt_params_3pl()`, `irt_params_pcm()`,
  and `irt_params_gpcm()` alongside the existing `irt_params_2pl()` and
  `irt_params_grm()`. All six share one distribution-aware signature
  pattern and delegate to a single model-registry method.
* `irt_design()` and `irt_simulate()` now support 3PL, PCM, and GPCM models
  in addition to 1PL, 2PL, and GRM.
* Two crash fixes in `irt_simulate()` for GRM studies at large test lengths
  (~60+ items): an `NA` convergence flag from `mirt` and an internal
  `mirt::fscores()` error are now handled gracefully (the affected
  iteration degrades to `NA` rather than aborting the whole simulation).

## Vignette engine note (hybrid `R.rsp` + `knitr`)

`VignetteBuilder` is `R.rsp, knitr`. The four paper-example vignettes
(`paper-example-1-linked-design`, `paper-example-1b-misspecification`,
`paper-example-2-mcar`, `paper-example-3-grm`) continue to ship as
pre-rendered HTML via `R.rsp::asis` because they run Monte Carlo
simulations (hundreds of `mirt` fits) that exceed the CRAN vignette
build-time budget. The two getting-started vignettes (`irtsim`,
`choosing-item-parameters`) use the standard `knitr` engine and build live
during R CMD check.

## Test environments

* **Local**: macOS Sequoia 15.7.3 (aarch64-apple-darwin23), R 4.6.0
  (2026-04-24), checked 2026-06-19 — 0 errors, 0 warnings, 0 notes
  (duration 2m 15s; `--as-cran`).
* **Win-builder R-release**: R 4.6.0 (2026-04-24 ucrt), checked
  2026-06-19 — Status: 1 NOTE (CRAN incoming feasibility; see below).
* **Win-builder R-devel**: R-devel (2026-06-18 r90173 ucrt), checked
  2026-06-19 — Status: 1 NOTE (CRAN incoming feasibility; see below).

## R CMD check results

0 errors | 0 warnings. The local macOS check is clean (0 notes). Both
Win-builder checks report a single NOTE from the CRAN incoming-feasibility
spell check:

```
Possibly misspelled words in DESCRIPTION:
  GPCM (15:59)
  GRM (14:70)
  PCM (15:21)
```

These are standard item response theory model acronyms — generalized
partial credit model (GPCM), graded response model (GRM), and partial
credit model (PCM) — newly added to the Description in this release. They
are correct domain terminology, not misspellings.

## Installed size

The four precomputed simulation-result objects (`.rds`) that back the
pre-rendered paper-example vignettes are excluded from the build
(`.Rbuildignore`); the static, self-contained vignette HTML already
embeds all results, so the objects are only needed at render time. This
keeps the installed package small (no `extdata` payload) and avoids the
installed-size NOTE that the shipped `.rds` produced in 0.1.2.

## Known URL-check pattern (publisher-side bot block)

The DOI `https://doi.org/10.1177/25152459251314798` (Schroeders & Gnambs,
2025; appears in `DESCRIPTION`, `README.md`, `NEWS.md`, and the two
`knitr`-engine vignettes) can return HTTP 403 to the URL checker because
the SAGE publisher (`journals.sagepub.com`, where the DOI redirects)
blocks HEAD requests from non-browser user agents. The DOI is valid and
resolves in browsers. This is the same publisher-side behaviour documented
in the v0.1.1 and v0.1.2 submissions and accepted by CRAN at those times.
(It did not surface on the 2026-06-19 Win-builder checks.)

## Reverse dependencies

`irtsim` has no reverse dependencies on CRAN as of this submission.

## Additional notes

* No compiled code in the package; no changes to dependencies (Imports:
  `cli`, `future.apply`, `ggplot2`, `mirt`, `rlang`).
* `parallel = TRUE` in `irt_simulate()` remains opt-in; the default serial
  path is unchanged and fully reproducible under Mersenne-Twister RNG.
