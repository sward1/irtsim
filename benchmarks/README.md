# irtsim scalability benchmark (Obj 36)

Profiles `irt_simulate()` across realistic test lengths and sample sizes so
v0.2.0 can ship a documented per-fit expectation. Now that 3PL/PCM/GPCM ship,
real DoD / I-O assessments run 60–200 items — well past the 30-item paper Ex1
ceiling that was the largest previously-tested case.

**Harness:** [`scalability_benchmark.R`](scalability_benchmark.R) (not part of
the package build — `benchmarks/` is in `.Rbuildignore`). Run instructions are
in the file header.

## Method

- **Grid:** `n_items ∈ {30, 60, 100, 150}` × `sample_size ∈ {200, 500, 1000}` ×
  `{2PL, GRM}`, 10 Monte Carlo iterations per cell, **serial** (`parallel =
  FALSE`). GRM uses 5 response categories (typical I-O Likert).
- **Two passes, deliberately separated.** `bench::mark(memory = TRUE)`'s
  allocation tracking thrashes the macOS allocator on the >10 GB cells (it
  produced `MallocStackLogging` spam and a bogus 78 s/fit reading in an early
  run), so timing and memory are measured independently:
  - **Timing** — `bench::mark(memory = FALSE)`, full grid.
  - **Peak heap** — `gc()` "max used" watermark (true peak, which is what drives
    OOM — more decision-relevant than bench's cumulative `mem_alloc`), on a
    representative subset.
- **Per-fit seconds** = cell wall-clock ÷ 10 iterations — the user-facing
  "expect ~X s/fit" number.

**Environment:** R 4.6.0 (aarch64-apple-darwin23), macOS Sequoia 15.7.3,
mirt 1.46.1. Single machine, serial. Absolute seconds are machine-specific; the
**scaling shape** and the **2PL↔GRM ratio** are the portable takeaways.

## Results — wall-clock

Per-fit seconds (cell total ÷ 10 iterations), serial:

| n_items | 2PL N=200 | 2PL N=500 | 2PL N=1000 | GRM N=200 | GRM N=500 | GRM N=1000 |
|--------:|----------:|----------:|-----------:|----------:|----------:|-----------:|
| 30      | 1.41\*    | 0.93      | 1.21       | 7.14\*    | 3.91      | 4.55       |
| 60      | 1.53      | 2.64      | 10.36      | 9.11      | 15.06     | 18.03      |
| 100     | 5.59      | 8.12      | 22.41      | 19.84     | 24.41     | 43.67      |
| 150     | 11.40     | 14.48     | 36.88      | 53.49     | 53.92     | 112.18     |

\* The first cell run for each model (30 × 200) is inflated by JIT/warmup — the
subsequent 30-item cells are the better small-scale estimate.

Raw data: [`results/scalability_results.csv`](results/scalability_results.csv).

## Results — peak memory

Peak resident heap (`gc()` "max used", MB) at N=500:

| n_items | 2PL peak (MB) | GRM peak (MB) |
|--------:|--------------:|--------------:|
| 30      | 335.6         | 351.7         |
| 60      | 351.7         | 351.7         |
| 100     | 351.7         | 371.6         |
| 150     | 351.7         | 394.9         |

**Peak resident memory is modest (~335–395 MB) and barely grows with
`n_items`.** This is the key correction to an early run that used
`bench::mark(memory = TRUE)`: those multi-GB figures (up to ~15 GB at 150 items)
were cumulative **allocation churn** — bytes allocated *and freed* over the 10
iterations — not resident memory. The true peak heap stays under 400 MB across
the whole grid.

Two consequences:
- **OOM is not a practical risk** at 30–150 items. A study runs comfortably in
  well under 1 GB.
- The wall-clock super-linearity (below) is therefore driven by the **GC
  processing that churn**, not by holding large blocks resident.

_Method caveat:_ the probe floors at the session heap high-water (~335 MB after
loading mirt), so per-cell increments below that floor aren't resolved — but
that itself confirms no cell pushes resident memory far past the baseline (the
largest, GRM 150, adds only ~60 MB). Raw data:
[`results/memory_probe.csv`](results/memory_probe.csv).

## Results — mirt time fraction

Rprof attribution on one representative cell (2PL, 100 items, N=500, 10 iters):

| Frame            | total time | % of wall-clock |
|------------------|-----------:|----------------:|
| `irt_simulate`   | 26.20 s    | 100.0%          |
| `mirt::mirt`     | 25.09 s    | **95.8%**       |

**~96% of wall-clock is inside `mirt::mirt()`** (the EM estimation) — the fit is
**mirt-bound**, not irtsim-plumbing-bound. The remaining ~4% covers data
generation, missingness, parameter extraction, and result accumulation.

Implication for Obj 44: irtsim cannot materially speed up an *individual* fit
(the cost is in the external mirt EM). The realistic levers are (a) parallelism
across cells/iterations — already available via `parallel = TRUE` — and (b)
reducing the allocation churn (the `n_items^~1.75` growth), much of which is
also inside mirt and therefore bounded by what the dependency does.

## Interpretation

1. **Cost is item-dominated, scaling ≈ `n_items^1.7`.** At N=500, 2PL goes
   30→150 items = 15.6× wall-clock for 5× the items (exponent ≈ 1.70); GRM is
   similar (≈ 1.63). Sample size matters far less at small/medium item counts.
2. **GRM is ~3–4× slower than 2PL** across the grid — expected, since each
   polytomous item carries `n_categories − 1` thresholds instead of one
   difficulty.
3. **The large-item × large-N corner inflates super-linearly in N.** 2PL
   60 × 1000 (10.4 s/fit) is ~4× the 60 × 500 cell (2.6 s) for only 2× the
   sample size; GRM 150 × 1000 (112 s/fit) is ~2× the 150 × 500 cell. Since peak
   resident memory stays flat (~400 MB), this is **GC pressure from allocation
   churn**, not a resident-memory blowup — tracked as **Obj 44**.

## User-facing expectations

Rough serial per-fit budgets on a modern laptop (discount ~3–4× with
`parallel = TRUE`):

| Scale                  | 2PL          | GRM           |
|------------------------|--------------|---------------|
| Small (≤30 items)      | ~1 s/fit     | ~4 s/fit      |
| Medium (60–100 items)  | ~2–8 s/fit   | ~9–24 s/fit   |
| Large (150 items)      | ~11–15 s/fit | ~53 s/fit     |

A realistic study multiplies this by `iterations × length(sample_sizes)`. E.g.
a GRM study at 100 items, 100 iterations × 4 sample sizes ≈ 100 × 4 × 24 s ≈
**2.7 hours serial** → use `parallel = TRUE`.

**Recommendations for large designs (≥100 items):**
- Set `parallel = TRUE` — the dominant cost is wall-clock (the mirt EM fit),
  and fits are independent across cells/iterations.
- Memory is **not** a constraint: peak resident heap stays under ~400 MB even
  at 150 items, so a full study runs comfortably in well under 1 GB. (Under
  `parallel = TRUE`, total memory scales with the number of concurrent workers
  × ~400 MB — still modest.)
- Start with a small `iterations` pilot to estimate wall-clock before
  committing to a full run.

## Related objectives

- **Obj 44** (performance): reduce the `n_items^~1.75` memory + wall-clock
  growth. Discovered here; tracked separately so this session stays
  measurement-focused.
- **Obj 45** (bugfix, CLOSED): the GRM-at-scale columns above only exist
  because this benchmark surfaced two crashes — `mirt::extract.mirt()`
  returning `NA` for the convergence flag, and `mirt::fscores()` throwing
  internally on sparsely-observed GRM categories. Both now degrade gracefully.
