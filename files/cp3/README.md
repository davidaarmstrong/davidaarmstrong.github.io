# app-js — CP3 DuckDB-WASM app

First working version of the JS rewrite (2026-08-31). Runs entirely client-side:
no backend, no build step. DuckDB-WASM loads `../surveydb.duckdb` (the real
database file at the repo root) directly in the browser and every chart is a
live SQL query — nothing here is a static data snapshot (the old
`prototypes/` proof-of-concepts used a static JSON sample; this is the real
thing, and it superseded them once it worked).

## Running it locally

DuckDB-WASM needs `fetch()` and a Worker, which browsers block on `file://`.
You need a local HTTP server. From the `cp3` repo root (not inside `app-js/`,
since `app.js` fetches the database via `../surveydb.duckdb`):

```
python3 -m http.server 8000
```

Then open `http://localhost:8000/app-js/index.html`.

## What it does

- Domain -> policy issue picker, driven entirely by `vbl_data` (no hardcoded
  question list — new questions just need a `vbl_data` row, per the existing
  pipeline).
- Grouping-variable picker matching the current Shiny app's list (region,
  province, gender, age category, religion, degree, language, union
  household, occupation, vote intention/choice).
- Diverging stacked bars (centered on the neutral middle category) with a
  weighted-average line, for the three Likert response types (`b`/`nsa`/`lsm`).
- Plain stacked bars (no diverging split, no average line — no natural
  neutral point) for `party`-type questions (`vote_intention`/`vote_choice`).
- Every chart's labels (response categories and grouping-variable categories)
  come from a SQL `JOIN` to `code_labels` at query time — nothing is relabeled
  in JS, per the earlier project decision.
- Resolves `vbl_data.type` per-question the same way the prototypes did:
  picks the dominant type across the question's survey waves and excludes any
  minority-type waves rather than mislabeling them (see
  `docs/data_quality_check.md` item 3), flagging the exclusion in the UI when
  it applies.
- `year` comes from `survey.year`, not `indiv.year` like the current Shiny
  app — a deliberate small fix, not a straight port (see
  `docs/data_quality_check.md` item 2).
- **Year-range slider** (added 2026-08-31, converted to a single two-headed
  slider the same day): one track with two draggable handles, not two
  separate stacked sliders — built from two overlaid native
  `<input type="range">` elements (transparent tracks, only the thumbs are
  clickable) plus a highlighted bar between them, since HTML has no native
  dual-handle range input. Whichever thumb is grabbed is raised above its
  sibling for that drag so the two handles never get stuck when their values
  are close together. Bounds are set to the selected question's own min/max
  `survey.year` (recomputed on every question change, not fixed globally — a
  question that only ran 1994-1996 gets a 1994-1996 slider, not the full
  dataset range). Filters server-side via `WHERE s.year BETWEEN ... AND ...`
  in the SQL, not a client-side row filter.
- **Party picker** (added 2026-08-31, `party`-type questions only): checkbox
  list of every party `code_labels` knows for the question (11 for
  `vote_intention`/`vote_choice`), capped at 6 selections — the 7th+
  checkbox disables itself rather than silently rejecting the click. Defaults
  to the top 6 parties by total weight across the question's full year range,
  recomputed only when the question changes (not on every year-slider tweak,
  so picker state doesn't reset while exploring a range). Unselected parties
  fold into "Other" via a SQL `CASE` expression (server-side, not JS) rather
  than being dropped, so the stack still sums to 100%.
- **Official party brand colors** (added 2026-08-31): party charts use each
  party's real brand color (e.g. Liberal red, NDP orange) instead of the
  generic 8-slot categorical palette, via a `PARTY_COLORS` lookup in `app.js`
  keyed to the exact `code_labels` label and a `--party-*` CSS custom
  property per party in `style.css`. A few of the given hex values needed a
  lightness tweak on one theme (light or dark) to clear a ~3.3:1 contrast
  floor against the chart surface — the given hex is kept exactly as-is on
  whichever surface it already cleared, and only the failing surface gets an
  adjusted (same hue/saturation) variant. Most visibly: Social Credit's given
  pale green (`#90EE90`) was nearly invisible on the light surface (1.38:1),
  so light mode uses a darker green (`#189a18`) while dark mode keeps the
  original. Any label without a mapped color falls back to the generic
  categorical palette. "Other" reuses the app's existing muted-gray text
  color; "Undecided" is theme-aware (black on light, white on dark) since a
  fixed black would vanish on the dark surface. See
  `docs/data_quality_check.md` items 6-7 for two gaps this surfaced: PPC
  voters have no dedicated code in the data (folded into "Other"), and
  "Undecided" (code 99) never actually appears in the data despite being a
  defined label.
- **Interactive hover tooltips** (added 2026-08-31, replacing the earlier
  native-`<title>`-attribute tooltips, which turned out to be unreliable in
  practice -- long hover delay, easy to miss, inconsistent across browsers).
  Every bar segment and average-line point now has a paired
  `Plot.tip(..., Plot.pointer({...}))` mark that tracks the pointer and
  shows a small styled box (year, category, share -- or year and average)
  immediately on hover, themed to match the app's light/dark surface colors.
  This is a real (if partial) restoration of the old echarts4r app's
  `e_tooltip(trigger = "axis")` behavior -- not identical (the old tooltip
  showed every category's value at once for the hovered year; this shows
  the one nearest segment), but reliably visible, which the previous
  native-title tooltip was not. The average-line dots on Likert charts
  intentionally do *not* get their own Plot.tip -- they sit close enough to
  the bar segments that the pointer-proximity tip usually grabbed the
  average instead of the segment actually being hovered, so they keep only
  the native `<title>` as a fallback.

## Known gaps / next steps

- **Question labels use `wording`, not `title`** — `vbl_data.title` is
  currently a placeholder for every row (`docs/data_quality_check.md` item 5).
- **No `ncols`-style layout control** for grouped charts yet — currently one
  row of facets, which gets wide with many groups (e.g. `province`, 13
  levels). The old app let you set columns; this doesn't yet.
- **Deployment/hosting is unresolved** — this works over any static file
  server; where it actually gets hosted (GitHub Pages, etc.) hasn't been
  decided.

## Validation

Query logic was cross-checked against real data before being wired into the
UI: `crime_2`/1966 unweighted counts (282/193/45 out of 520) closely match
the weighted shares the app computes (54.08%/37.44%/8.48%, weights average
~1.0) — confirming the query is correct. (For the record: the earlier static
prototype's sample JSON for this same question/year had a bug and doesn't
match either — treat `prototypes/data/multi_question_sample.json` as
superseded, not a reference.) Tested end-to-end in headless Chromium against
the real 35.7MB database file: catalog load, domain/question/group-var
picker wiring, both diverging and party chart rendering, grouped and
ungrouped, with zero console errors after fixing one bug found this way (a
`BigInt` from a `BIGINT` column crashing `Array.sort`'s comparator). The
year-slider and party-picker additions were tested the same way: per-question
year bounds, the 6-party cap (including that the 7th checkbox actually
disables rather than just silently failing), the "Other" fold-in via SQL
`CASE`, and year-range filtering — all verified against the real database
with screenshots before shipping.
