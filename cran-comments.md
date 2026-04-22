# irtsim 0.1.1 — CRAN resubmission (2026-04-22)

## Response to CRAN reviewer feedback (2026-04-22)

* **Acronyms in DESCRIPTION.** All acronyms are now spelled out on
  first use in the `Description` field: application programming
  interface (API), item response theory (IRT), one-parameter logistic
  (1PL), two-parameter logistic (2PL), missing-completely-at-random
  (MCAR), missing-at-random (MAR), mean squared error (MSE), root
  mean squared error (RMSE), and standard error (SE).

* **`\dontrun{}` → `\donttest{}`.** Replaced `\dontrun{}` with
  `\donttest{}` in all seven affected `.Rd` files:
  `irt_simulate.Rd`, `print.irt_results.Rd`, `summary.irt_results.Rd`,
  `print.summary_irt_results.Rd`, `plot.irt_results.Rd`,
  `plot.summary_irt_results.Rd`, and `recommended_n.Rd`. These
  examples are fully executable but exceed the 5-second threshold
  because each depends on `irt_simulate()` running 100 Monte Carlo
  iterations across 3 sample sizes (~300 `mirt` fits). They are
  therefore wrapped in `\donttest{}` rather than unwrapped; the
  faster pipeline-head examples (`irt_design`, `irt_study`,
  `irt_iterations`, `irt_params_2pl`, `irt_params_grm`) are already
  unwrapped and runnable.

## Response to CRAN reviewer feedback (2026-04-14, retained)

* **License.** Changed `License: GPL (>= 3) + file LICENSE` to
  `License: GPL (>= 3)` and removed the `LICENSE` file from the
  package. The prior file only carried a YEAR/COPYRIGHT-HOLDER
  stub, which is the MIT template and is not appropriate for a
  GPL-licensed package. The package now uses the standard CRAN
  GPL-3 license template with no package-specific restrictions.

* **DOIs.** All three DOIs have been re-verified and are correct
  and resolving:
    * `10.1002/sim.2673` — Burton, Altman, Royston, & Holder
      (2006). *The design of simulation studies in medical
      statistics.* Statistics in Medicine, 25, 4279–4292.
    * `10.1002/sim.8086` — Morris, White, & Crowther (2019).
      *Using simulation studies to evaluate statistical methods.*
      Statistics in Medicine, 38, 2074–2102.
    * `10.1177/25152459251314798` — Schroeders & Gnambs (2025).
      *Sample-size planning in item-response theory: A tutorial.*
      Advances in Methods and Practices in Psychological Science.

  The 403/invalid-DOI warnings from the automated URL checker are
  publisher-side bot blocks (Wiley onlinelibrary.wiley.com and
  SAGE journals.sagepub.com both return 403 to HEAD requests from
  non-browser user agents). Each DOI loads correctly in a browser
  and in the PubMed / SAGE landing-page records above.

## Test environments

* Local: macOS (aarch64-apple-darwin20), R 4.2.2 — 0 errors, 0 warnings, 2 notes
* Win-builder: Windows Server, R-devel 4.6.0 RC (2026-04-21 r89932 ucrt), checked 2026-04-22 — 0 errors, 0 warnings, 1 note
* Win-builder: Windows Server, R-release 4.5.3 (2026-03-11 ucrt), checked 2026-04-22 — 0 errors, 0 warnings, 1 note
* Win-builder: Windows Server, R-oldrelease 4.4.3 Patched (2026-02-12 r89426 ucrt), checked 2026-04-22 — 0 errors, 0 warnings, 1 note

## R CMD check results

0 errors | 0 warnings | 1 note

### Note: CRAN incoming feasibility

**Possibly misspelled words in DESCRIPTION** — all are legitimate
domain vocabulary, not misspellings:

* `Gnambs`, `Schroeders` — surnames of the authors whose methodological
  framework the package implements (Schroeders & Gnambs, 2025).
* `missingness` — standard term in the missing-data literature.

(IRT, MCAR, MAR, MSE, RMSE, and SE now appear spelled out in
`Description`, so the acronym flags from the prior submission no
longer apply.)

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
* License: GPL (>= 3), inherited from the `mirt` dependency. No
  `file LICENSE` component; the package carries no additional
  GPL restrictions.
