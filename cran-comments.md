# irtsim 0.1.2 — Feature release (2026-05-04)

This is a feature release. Headline changes:

* New `irtsim` getting-started vignette and new
  `choosing-item-parameters` reference vignette. Both build live via
  the `knitr` engine during R CMD check (target render <30s each).
* `recommended_n()` gains an `aggregate` parameter
  (`"max"` / `"mean"` / `"median"` / `"none"`, default `"max"`). The
  default return is now an integer scalar instead of a per-item data
  frame. **Behaviour change** — pass `aggregate = "none"` for the
  previous return shape.
* `irt_design()` now aborts with an informative error if
  `n_factors != 1`. Multidimensional IRT support is planned for a
  future release; the prior silent-acceptance produced cryptic
  downstream errors from `mirt`.
* Removed the `paper-reproduction-gaps` vignette. Its content
  pointed at deferred objectives that are now superseded by a
  planned pluggable hook (targeted for a future release).

## Vignette engine note (hybrid `R.rsp` + `knitr`)

`VignetteBuilder` is `R.rsp, knitr`. The four pre-existing paper-
example vignettes (`paper-example-1-linked-design`,
`paper-example-1b-misspecification`, `paper-example-2-mcar`,
`paper-example-3-grm`) continue to ship as pre-rendered HTML using
`R.rsp::asis` because they demonstrate Monte Carlo simulations
(hundreds of `mirt` fits) that exceed the CRAN vignette build-time
budget. The two new vignettes use the standard `knitr` engine and
build live during R CMD check; their combined fresh-build cost on
the local environment is ~25 seconds.

## Test environments

* **Local**: macOS Sequoia 15.7.3 (aarch64-apple-darwin23),
  R 4.6.0 (2026-04-24), checked 2026-05-04 — 0 errors, 0 warnings,
  0 notes (duration 2m 27s).
* **Win-builder R-devel**: Windows Server, R-devel
  (2026-05-03 r89994 ucrt), checked 2026-05-04 — Status: OK
  (0 errors, 0 warnings, 0 notes; check time 451s).
* **Win-builder R-release**: Windows Server, R 4.6.0
  (2026-04-24 ucrt), checked 2026-05-04 — Status: OK
  (0 errors, 0 warnings, 0 notes; check time 439s).

## R CMD check results

0 errors | 0 warnings | 0 notes on all three checked environments
(local macOS, Win-builder R-devel, Win-builder R-release).

The `Possibly misspelled words in DESCRIPTION` note from the 0.1.1
submission did not re-surface on any 0.1.2 check. The affected words
(`Gnambs`, `Schroeders`, `missingness`) are unchanged in the
`Description` field and remain legitimate domain vocabulary if they
are re-flagged by CRAN's incoming check.

## Known URL-check pattern (publisher-side bot block)

The DOI `https://doi.org/10.1177/25152459251314798`
(Schroeders & Gnambs, 2025; appears in `DESCRIPTION`, `README.md`,
`NEWS.md`, and the two new `knitr`-engine vignettes) returns
HTTP 403 to the URL checker because the SAGE publisher
(`journals.sagepub.com`, where the DOI redirects) blocks HEAD
requests from non-browser user agents. The DOI is valid and
resolves correctly in browsers — verified manually 2026-05-04.
This is the same publisher-side behaviour documented in the
v0.1.1 submission and accepted by CRAN at that time.

## Reverse dependencies

`irtsim` has no reverse dependencies on CRAN as of this submission.

## Additional notes

* No changes to compiled code (the package contains none).
* No changes to dependencies (Imports: `cli`, `future.apply`,
  `ggplot2`, `mirt`, `rlang`).
* `recommended_n()` default-return-shape change is documented in
  `NEWS.md` and surfaced in the `irtsim` getting-started vignette;
  users requiring the previous return shape can pass
  `aggregate = "none"`.
* `parallel = TRUE` in `irt_simulate()` remains opt-in; the default
  serial path is unchanged and fully reproducible under
  Mersenne-Twister RNG.
