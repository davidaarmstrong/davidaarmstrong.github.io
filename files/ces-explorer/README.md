# app-js — CES Explorer JS port

JS rewrite of the Shiny `ces-explorer` app. All four tabs are built:
**Descriptives**, **Cross-Tabs**, **Models** (logistic regression +
average-case marginal effects), and **Documentation**, plus a shared
**Subset Sample** control (None / Year / Region) that filters the first
three tabs at once.

## Why no DuckDB-WASM (or any WASM)

Unlike `cp3` (a ~36MB survey database queried live via DuckDB-WASM), the
CES cumulative file is small: 31,732 rows x 38 columns, ~1MB as `.rda` and
~8MB as JSON. It just ships as a static file (`data/ces_data.json`) and
every table/plot/model is computed with plain JS -- no SQL engine, no
matrix/stats library, no WASM at all. Even the logistic regression (see
Models below) is hand-rolled IRLS over plain JS arrays-of-arrays; at this
data size (tens of thousands of rows, a few dozen coefficients) it runs in
well under a second in-browser.

## Scope changes from the old Shiny app

Per the migration decision, three simplifications vs. the original
`ui.r`/`server.R`:

- **Unweighted.** The old app used `svydesign(weights=~weight, ...)` and
  survey-adjusted statistics throughout (`svymean`, `svyglm`, `svychisq`,
  ...). This port uses plain unweighted statistics/glm everywhere.
  Cross-checked against R directly and matched exactly: overall and
  by-province means/SDs for `leader_con`; the `vote x province` chi-square
  (χ²=8261.93, df=15); and `glm(..., family=binomial)` coefficients/SEs
  for several Models-tab specifications (see Validated below).
- **No multiple imputation.** The old `server.R` loaded `mitools` and had
  leftover doc text about "Rubin's rules with five imputed datasets," but
  never actually called `MIcombine`/`imputationList` — the active data
  file (`ces0421.rda`) already has no imputation step applied to it (the
  imputed variant, `ces0419imp.rda`, was already disabled in the old
  server.R). So this was already true before the port; nothing to remove.
- **Average-case only.** The average-case-vs-average-effect switch was
  already commented out in the old `ui.r` and the server always called
  `probci(..., type="aveCase")` — the `aveEff` branch was dead code, not
  ported. `model.js`'s average-case simulation is a direct port of the old
  `probci2` aveCase branch (see Models below).

## Files

- `data.js` — loads `data/ces_data.json`, zips columns into row objects,
  and holds the picker lists for every tab (`NUM_VARS`/`STRAT_VARS`/
  `ROW_VARS` for Descriptives & Cross-Tabs; `DV_VARS`/`IV_BLOCKS`/
  `VARY_BY_OPTIONS` for Models), all ported from the old `ui.r`/`server.R`
  variable catalogs.
- `stats.js` — pure functions, no DOM: mean/sd/quantile (R's type-7
  algorithm), `summarise()` (the Descriptives table), `pairwiseComparisons()`
  (one row per unordered pair of group means -- a z-test difference, its
  unadjusted "analytical" p-value, and a Bonferroni-adjusted p-value that
  multiplies it by the number of comparisons; originally a factorplot-style
  significance grid, replaced with this table on request since a grid
  doesn't show the actual p-values or scale well past a handful of groups),
  `freqTable()`/`contingency()`/`chiSquareTest()` (Cross-Tabs), plus a
  hand-rolled regularized incomplete gamma function for the chi-square
  p-value.
- `matrix.js` — minimal dense linear algebra for the Models tab: matrix
  multiply/transpose, a weighted cross-product helper (for IRLS), a
  Gauss-Jordan inverse, and a Cholesky decomposition (for simulating
  multivariate-normal coefficient draws). No external dependency.
- `model.js` — the Models tab's statistical core: builds a dummy-coded
  design matrix (reference = first *observed* level, matching the old
  app's `droplevels()` before modeling) from a variable list plus an
  optional "vary effect by" interaction; fits binomial logistic regression
  via IRLS with R's own `glm.fit` convergence criterion (relative deviance
  change, so near-separation cases converge the same way R's does);
  computes PRE/ePRE fit statistics (`computePRE`, a closed-form port of
  the old `pre()` that avoids re-fitting a null model); and runs the
  average-case predicted-probability simulation (central tendency for
  held-constant variables, `[min, max]` "first difference" grid for a
  continuous focus variable, N(coef, vcov) draws via Cholesky, quantiles
  for the median + 95% CI) -- a direct port of the old `probci2` aveCase
  branch. Flags likely quasi-complete separation (e.g. a region-only
  predictor of a region-specific party's vote) via a simple
  large-coefficient heuristic, surfaced as a warning in the UI rather than
  failing silently.
- `charts.js` — rendering, via Observable Plot for the histogram/
  group-means/predicted-probability charts and hand-built D3 SVG for the
  mosaic plot (Plot has no native mosaic mark). The Descriptives tab's
  pairwise comparisons render as a plain HTML table directly in
  `descriptives.js` rather than through this file, since it's just a
  table, not a chart. Category order everywhere follows the R factor level
  order from
  `data/ces_data.json`'s `levels` object, not alphabetical.
- `descriptives.js` / `crosstabs.js` / `models.js` — wire each tab's
  inputs to the stats/model/chart functions. Each exposes a `setRows()`
  so the shared subset control (`subset.js`) can re-render whichever
  tab(s) are affected when the sample subset changes.
- `subset.js` — the shared Subset Sample control (None / Year / Region),
  wired in `app.js` to call every tab's `setRows()`.
- The **Documentation** tab's content lives directly as static HTML inside
  `index.html` (`#panel-documentation`) rather than a separate Markdown
  file rendered client-side -- it doesn't need to react to app state, so a
  markdown-parser dependency wasn't worth adding. Rewritten from the old
  `Documentation.md` to describe what this version actually does
  (unweighted, no imputation, average-case only, no Region/Year checkboxes
  -- covered by the shared Subset Sample control instead) rather than
  porting the old text as-is. Also documents two real data quirks
  surfaced while writing it: the three Party ID variants that fold
  Green/BQ into Other don't have a curated level order in the exported
  data (so their reference category is whichever sorts first
  alphabetically, not Liberal like the main Party ID variable), and
  `year_fac`'s levels are coded 4/6/8/11/15/19/21 rather than full years.
- `data-export.R` (repo root) — one-time/rerun-on-data-change export from
  `ces0421.rda` to `data/ces_data.json`. Column-oriented JSON (not
  row-oriented) to keep the file smaller; `data.js` zips it into rows on
  load. Drops `weight`.
- `devserver.py` — the local dev server (see Running it locally). A plain
  `python -m http.server` turned out to cache JS modules across edits in a
  way that survived even hard-reloads and new tabs (confirmed via `curl`
  that the server was always serving current content — it was purely a
  browser HTTP-cache issue); this sends `Cache-Control: no-store` on every
  response instead.

## Models tab design notes

- **One trigger, not partial reactivity.** In the old Shiny app, changing
  "vary effect by" (which changes the model formula) re-fit the model
  automatically, but changing the focus variable alone did not always
  require a re-fit — a distinction that depended on exactly which reactive
  a given input fed into. This port simplifies to a single rule: *any*
  input change after a successful estimate invalidates the results and
  requires clicking "Refresh Model Output" again. Simpler to reason about
  (including for the students this app is for), at the cost of an extra
  click in the cases where the old app could've skipped the re-fit.
- **Focus-variable list is live-restricted** to variables actually in the
  model(s) -- the intersection of Model 1 and Model 2's variables when
  both are active, matching the old app's `inst_chc` logic -- but this
  restriction updates immediately as Model 1/2 selections change, without
  requiring a re-estimate first.
- **`vote_incumbent`** is referenced in the old `ui.r`/`server.R` DV
  dropdown but that column doesn't exist in `ces0421.rda` -- a
  pre-existing bug in the Shiny app. Dropped from `DV_VARS` here rather
  than ported forward.

## Running it locally

ES module imports need a real HTTP server (browsers block `fetch()` and
`import` on `file://`). From `app-js/`:

```
python3 devserver.py 8733
```

Then open `http://localhost:8733`.

## Validated

- Descriptives/Cross-Tabs: numbers cross-checked directly against R
  (`mean`/`sd`/`aggregate`/`chisq.test` on `ces0421.rda`, unweighted) —
  matched exactly.
- Models: `fitLogistic`'s coefficients and standard errors matched R's
  `glm(dv ~ ..., family=binomial)` to displayed precision for two
  well-behaved specifications (`vote_lib ~ gender + agegrp + educ`,
  `vote_con ~ gender + leader_con`); a third specification with a
  quasi-complete-separation predictor (`vote_bloc ~ gender + agegrp +
  province*gender` — BQ vote is ~0 outside Quebec) converged to the same
  substantive story as R (near-0% outside Quebec, ~39% in Quebec) with
  coefficients in the same ballpark, which is the expected residual
  difference for an ill-conditioned/separated fit, not a bug (verified via
  a standalone Node harness against `ces0421.rda` directly, and separately
  in-browser: a `glm()`-computed average-case prediction, 0.3449/0.3765/
  0.4098 across age groups for a `vote_lib` model, matched the app's UI
  output of 0.345/0.376/0.409 exactly). All three tabs' UIs exercised
  live in-browser (single model, two-model comparison, interaction/"vary
  effect by", input validation, subset integration) with no console
  errors, in both light and dark emulated themes.

## Mobile responsiveness

Pattern borrowed from `cp3/app-js-remote`'s off-canvas sidebar (the plain
`cp3/app-js` doesn't have it), adapted for this app's different shell: `cp3`
is a single-page tool with a floating fixed toggle button; this app has
persistent chrome above the fold (topbar/tabs/subset-bar) that a floating
button would overlap, so `#sidebar-toggle` sits in normal document flow
instead, right below that chrome. It resolves which `.sidebar` to open
dynamically (`.tab-panel.active .sidebar`) rather than hardcoding per-tab
ids, so it's automatically hidden on Documentation (no sidebar there) and
closes/re-evaluates on every tab switch.

Below the 760px breakpoint: each tab's sidebar becomes a slide-in drawer
with a backdrop (tap outside, the ✕ button, or Escape to close); the
topbar/tabs/subset-bar stack or scroll instead of overflowing; form
controls go to 16px font (avoids iOS Safari's auto-zoom-on-focus below
that size) and bigger touch targets; and every chart (`charts.js`) sizes
itself to its container's actual measured width via `measuredWidth()`
instead of a fixed pixel literal, so plots shrink to fit a phone screen
rather than forcing horizontal scroll. The mosaic plot's whole D3-built
layout (height, legend width, tick-label/axis-title spacing) scales by the
same factor its width shrinks from the 560px design, with legend width
additionally sized to the longest row-level label so long ones (e.g.
"Conservative") don't clip.

One real bug found and fixed along the way: `.sidebar-toggle`'s mobile
media-query rule originally set `display: flex` unconditionally, which
silently overrode the `hidden` attribute app.js sets on it for
Documentation -- an explicit author-CSS `display` declaration beats the
`[hidden]` UA-stylesheet rule at equal selector specificity. Fixed with
`.sidebar-toggle:not([hidden])`.

**Confirmed on a real phone** (not just this session's emulated-viewport
tooling, which drives clicks via mouse-event semantics and so couldn't
actually test this): tapping multiple options in the native `<select
multiple>` Model 1/Model 2 pickers correctly adds to the selection rather
than replacing it, as real mobile Safari/Chrome are supposed to do (no
ctrl/cmd-click equivalent needed on touch). The emulated testing tool's
own second-click-replaces-first behavior earlier was a testing-tool
artifact, not a real issue -- no Tom Select or other multi-select combobox
library needed here.

## Bug fix: Models tab predicted-probability CIs were invisible

`renderPredictedProbabilities` used `Plot.ruleY(sub, {x, y1: "lower", y2:
"upper", ...})` for the vertical confidence-interval bars. That's backwards:
`ruleX` draws a vertical segment (fixed x, spanning y1..y2); `ruleY` draws
a horizontal one (fixed y, spanning x1..x2). With no x1/x2 channels
supplied, the ruleY mark silently rendered nothing at all -- confirmed by
inspecting the rendered SVG directly (zero `<line>`/`<path>` elements for
it, versus three once fixed). `renderGroupMeans` (the Descriptives tab's
mean +/- CI chart) already used `ruleX` correctly and was never affected.

Found because a user reported the CIs looked missing; a quick look at the
predicted-probability table showed real, non-degenerate intervals (e.g.
0.288-0.315), so "the interval exists but rendered too small to see" was
tested and ruled out via a minimal reproduction before finding the actual
mark-name swap. Fixed alongside a second, independent improvement: with
these sample sizes a 95% CI is often only a few pixels tall regardless, so
a solid-filled point marker the same size as the CI would still visually
hide it even correctly rendered -- the dot is now hollow (`fill: "none"`,
colored stroke ring) so the line is visible through its center at any
length.

## Known gaps / next steps

- No plot-customization controls (colors, legend position, axis title
  offsets, rotation, pixel sizing) that the old Shiny app had for the
  Models "Plot" tab -- Observable Plot handles legends/facets natively, so
  these felt like R/ggplot-specific workarounds rather than something
  worth porting as-is. Worth revisiting if you find yourself wanting to
  export a specific plot for a slide deck or handout.
- No `rintrojs` walkthroughs ("Walk Me Through" buttons) -- the
  Documentation tab (see below) covers the same ground as static prose
  instead.
- Deployment/hosting is unresolved, same as `cp3`.
