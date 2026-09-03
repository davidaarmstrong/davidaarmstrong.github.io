# app-js-remote — CP3 DuckDB-WASM app (remote-Parquet variant)

Fork of `../app-js` (2026-09-01), built to solve one problem: the whole-file
`surveydb.duckdb` that `../app-js` downloads on load had grown to 76.6MB+ as
more questions were added, and only gets bigger — a real problem for anyone
on a slow or mobile connection, since nothing renders until that whole file
is in. `../app-js` is left completely untouched as a working backup; this
is a from-scratch fork of it, not an in-place edit.

## What's different from `../app-js`

Everything **except** the query layer is byte-for-byte identical to
`../app-js` — same `index.html`, same `style.css`, same chart-building /
rendering / UI-wiring code in `app.js`. Only these functions changed
(`app.js`, search for these names): `initDB()`, `getYearBounds()`,
`getPartyOptions()`, `fetchChartData()`, plus the top-of-file constants.

- **No more single database file.** `populate_db.R`'s "Export Parquet
  files" step (near the end of that script) writes three artifacts to
  `parquet_out/`: `vbl_data.parquet` and `code_labels.parquet` (both tiny —
  ~117KB and ~2KB), and `resp/`, hive-partitioned by `question` into one
  file per question (~59KB median, ~5.2MB max, per the 2026-09-01 sizing
  pass — see project memory for the full numbers and the comparison against
  the old normalized shape).
- **Only `vbl_data` and `code_labels` load up front** (as DuckDB views over
  their Parquet files, via `read_parquet('https://.../vbl_data.parquet')`
  etc.) — small enough that this is instant. `resp` is **not** loaded at
  startup at all.
- **`resp` is fetched one question at a time.** Selecting a question is the
  only action that triggers a new network fetch (of that question's ~59KB
  file, typically). Changing the grouping variable or narrowing the year
  range is pure local recomputation on the same in-hand slice — no new
  fetch — because every grouping column (region, province, age_cats, etc.)
  and the display year are already denormalized directly onto each `resp`
  row at build time (see `populate_db.R`'s "Denormalize onto resp"
  section). That's also why there's no more `indiv` or `survey` table
  anywhere in this app: nothing here ever needs to join to them.
- Where the old `surveydb.duckdb` needed a `JOIN indiv` to look up a
  grouping variable's raw value, this version reads it straight off `resp`
  (`r.<groupVar>` instead of `i.<groupVar>`) — one join removed, not added.

Nothing about chart rendering, colors, tooltips, the year slider, the party
picker, or facet-wrapping changed — those all still work exactly as
documented in `../app-js/README.md`.

## Deployment layout — this is load-bearing

This app expects a `cp3_parquet/` folder to exist as a **sibling** of
wherever this `app-js-remote/` folder itself is served from, both nested
inside a common `files/` parent — e.g.:

```
you.github.io/
  files/
    app-js-remote/   <- this folder (index.html, app.js, style.css)
    cp3_parquet/     <- the data (see structure below)
```

The exact path is computed in `app.js` as
`new URL("../cp3_parquet/", import.meta.url)`, resolved relative to
`app.js`'s own URL — this app folder can be renamed to anything (`app/`,
`dashboard/`, whatever) and it'll still find `cp3_parquet/` correctly, as
long as the sibling relationship holds and the data folder is still called
`cp3_parquet`. If you rename the data folder too, update the
`DATA_BASE_URL` line near the top of `app.js` to match — it's the one
place this path is set.

`cp3_parquet/` needs this exact structure (exactly what `parquet_out/`
already looks like after running `populate_db.R` — copy it in wholesale,
just renamed):

```
cp3_parquet/
  vbl_data.parquet
  code_labels.parquet
  resp/
    question=<code>/data_0.parquet   (one such folder per question, ~651 of them)
```

Two deployment notes, since GitHub Pages is the target host (see project
memory, 2026-09-01):

- **Add a `.nojekyll` file at the site root** if one doesn't already exist.
  GitHub Pages runs everything through Jekyll by default; an empty
  `.nojekyll` file skips that entirely, which is standard practice for a
  folder of pure data files like this (nothing here needs or benefits from
  Jekyll processing, and it avoids any surprise behavior).
- **Repo size will grow on every rebuild.** `parquet_out/` is regenerated
  from scratch by every full `populate_db.R` run (~75MB currently), and
  since it's almost entirely new binary content each time, git can't diff
  it efficiently — committing it straight into a repo's normal history
  means every rebuild adds another ~75MB permanently. Not a problem at
  current size or rebuild frequency (GitHub Pages' soft limits are ~1GB
  site size / ~100GB bandwidth per month, both comfortably clear of this),
  but worth knowing if rebuilds become frequent over a long period — an
  orphan/squashed branch is the usual fix, not something this app needs to
  care about either way.

## Running it locally

Same as `../app-js`: DuckDB-WASM needs `fetch()` and a Worker, which
browsers block on `file://`, so you need a local HTTP server, rooted
somewhere that reproduces the `files/app-js-remote/` + `files/cp3_parquet/`
layout above. From the `cp3` repo root:

```
mkdir -p files
ln -s ../app-js-remote files/app-js-remote   # or cp -r if your OS dislikes symlinks
ln -s ../parquet_out files/cp3_parquet
python3 -m http.server 8000
```

Then open `http://localhost:8000/files/app-js-remote/index.html`.

## Validation status (2026-09-01)

- **Folder-naming correction, same day**: the first version of this app
  assumed `files/` sat as a sibling of `app-js-remote/` at the site root.
  The actual planned layout nests both `app-js-remote/` and the data folder
  (named `cp3_parquet/`) *inside* a common `files/` parent instead (see
  "Deployment layout" above) — `DATA_BASE_URL` was fixed accordingly
  (`../cp3_parquet/` relative to `app.js`, not `../files/`), re-verified
  with a plain Node `new URL(...)` check against a simulated
  `files/app-js-remote/app.js` path, and the SQL validation below was
  re-run unchanged (query text didn't change, only the URL constant).
- **Query logic**: every SQL query this rewrite touches (`getYearBounds`,
  `getPartyOptions`, and `fetchChartData` in both its Likert/ungrouped,
  Likert/grouped, party/ungrouped, and party/grouped forms) was run
  directly against the real exported Parquet files (via Python's `duckdb`
  bindings, same query text as `app.js` constructs, question codes
  `env_soim_018` [type `b`], `vote_intention` [type `party`]) and checked
  that every year/group's response shares sum to exactly 100% — they did,
  with zero deviation across 19-662 groups per test. This confirms the
  join logic, the denormalized-grouping-column change, and the
  party "Other" fold-in are all correct.
- **NOT verified**: actual execution inside DuckDB-WASM in a browser.
  `node --check` confirms `app.js` is syntactically valid JS, and every
  other code path (rendering, UI) is unchanged from the already-shipped,
  already-tested `../app-js`. But `read_parquet()` over `https://` in
  DuckDB-WASM needs to fetch DuckDB's `parquet` extension (a `.wasm`
  module) from `extensions.duckdb.org` on first use, and the sandboxed
  environment this was built in blocks that host entirely — so the one
  thing that could not be tested here is whether that extension fetch (and
  the whole remote-Parquet read path built on it) actually completes in a
  real browser. This is DuckDB-WASM's standard, default, widely-used
  mechanism for reading remote Parquet — not something unusual to this
  setup — so it should just work with normal internet access, but it
  genuinely wasn't exercised end-to-end here. **Do the local-server smoke
  test above (which has normal internet access, unlike this sandbox)
  before trusting this in production** — open the browser console and
  confirm the status line reaches "Ready", then click through a couple of
  questions (try one Likert question and `vote_intention`, grouped and
  ungrouped) and confirm charts render with no console errors.
