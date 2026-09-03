// CP3 — DuckDB-WASM app (remote-Parquet variant).
//
// Architecture (see project memory / docs/data-model.md, docs/data_quality_check.md
// for the reasoning behind each of these; this file is a fork of ../app-js —
// see README.md in this directory for exactly how it differs and why):
//  - There is no single database file anymore. populate_db.R exports three
//    artifacts to parquet_out/ (see the "Export Parquet files" section near
//    the end of that script): vbl_data.parquet and code_labels.parquet (both
//    tiny, loaded in full at startup below) and resp/, hive-partitioned by
//    `question` into one file per question (~59KB median, ~5.2MB max, per
//    the 2026-09-01 sizing pass). Those get deployed as a `cp3_parquet/`
//    folder that sits *next to this app folder*, both inside a common
//    `files/` parent (see DATA_BASE_URL below) — resolved relative to this
//    script's own URL, so it doesn't matter what this folder itself is
//    named, only that cp3_parquet/ is its sibling.
//  - Only vbl_data + code_labels are loaded up front. `resp` is fetched one
//    question at a time, straight off the hosted Parquet file for that
//    question — see respUrlFor()/fetchChartData() below. Changing the
//    selected question is the only thing that triggers a new network fetch;
//    changing the grouping variable or the year range is pure local
//    recomputation on whatever question-slice is already in hand, since
//    every grouping column (and the display year) is denormalized directly
//    onto each resp row at build time now — there is no `indiv` or `survey`
//    table in this app at all, and nothing here ever joins to them.
//  - Label lookups (code_labels) are still applied via SQL JOIN at query
//    time, not in JS — per-decision 2026-08-31, unchanged by this rewrite.
//  - vbl_data.type can vary by survey_obs within one question (data_quality_check.md
//    item 3); every query here resolves the question's *dominant* type first and
//    filters to only the waves matching it, flagging any excluded waves in the UI,
//    exactly like prototypes/plot-diverging-poc-v2.html did.
//  - "year" is resp.year directly, which populate_db.R already sources from
//    survey.year (not indiv.year) at build time — see the denormalization
//    note in populate_db.R. That used to be an app.js-level JOIN choice;
//    it's baked into the data itself now, so this app never needs to choose.

import * as duckdb from "https://cdn.jsdelivr.net/npm/@duckdb/duckdb-wasm@1.32.0/+esm";
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm";
import * as Plot from "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6.17/+esm";

// Resolved relative to this script's own URL, one level up then into
// cp3_parquet/ — e.g. files/app-js-remote/app.js finds files/cp3_parquet/.
// Works unchanged whether this app folder is served as app-js-remote/,
// app/, or renamed to anything else, as long as cp3_parquet/ is genuinely
// its sibling (see README.md in this directory).
const DATA_BASE_URL = new URL("../cp3_parquet/", import.meta.url).href;
const VBL_DATA_URL = DATA_BASE_URL + "vbl_data.parquet";
const CODE_LABELS_URL = DATA_BASE_URL + "code_labels.parquet";
function respUrlFor(question) {
  return `${DATA_BASE_URL}resp/question=${encodeURIComponent(question)}/data_0.parquet`;
}

const DOMAIN_LABELS = {
  econ:        "Economic Policy",
  energy:      "Energy & Environment",
  health:      "Health, Welfare & Public Services",
  identity:    "Identity, Immigration & Multiculturalism",
  intlaffairs: "International Affairs & Defence",
  values:      "Social Values & Morality",
  vote:        "Vote Intention & Choice",
};

const GROUP_VARS = [
  { value: "",                label: "None" },
  { value: "vote_intention",  label: "Vote Intention" },
  { value: "vote_choice",     label: "Vote Choice" },
  { value: "region",          label: "Region" },
  { value: "province",        label: "Province" },
  { value: "com_500",         label: "Community Size > 500,000" },
  { value: "com_100",         label: "Community Size > 100,000" },
  { value: "woman",           label: "Gender" },
  { value: "age_cats",        label: "Age Category" },
  { value: "religion",        label: "Religion" },
  { value: "degree",          label: "University Degree" },
  { value: "language",        label: "Language" },
  { value: "union_household", label: "Union Household" },
  { value: "occupation",      label: "Occupation" },
];

// Likert-style response types: diverging stacked bars centered on the
// neutral middle category, plus a re-derived weighted-average line.
// `values` are the raw resp.response codes in low->high order — used to
// remap the 0-100 native average onto the shared -100..100 diverging axis.
const TYPE_CONFIG = {
  b:   { categories: ["Disagree", "Agree"],            values: [0, 1] },
  nsa: { categories: ["Never", "Sometimes", "Always"],  values: [0, 0.5, 1] },
  lsm: { categories: ["Less", "Same", "More"],          values: [-1, 0, 1] },
};

const POLE_COLORS = {
  2: ["var(--never)", "var(--always)"],
  3: ["var(--never)", "var(--neutral)", "var(--always)"],
};

const CATEGORICAL_COLORS = [
  "var(--cat-1)", "var(--cat-2)", "var(--cat-3)", "var(--cat-4)",
  "var(--cat-5)", "var(--cat-6)", "var(--cat-7)", "var(--cat-8)",
];

// Official party brand colors, keyed by the exact label code_labels uses for
// each party (labels merge multiple historical party names -- e.g. today's
// PPC-era "Conservative" and the older "PC" both fall under "PC/Conservative"
// in this codebook, so the color applies to the merged label). Any label not
// listed here (there shouldn't be any, but just in case) falls back to the
// generic categorical palette. See docs/data_quality_check.md for the two
// gaps this surfaced: "Undecided" (code 99) never actually appears in the
// data, and there's no code at all for the People's Party (PPC) -- the
// coding scheme predates its founding, so PPC voters are folded into "Other".
const PARTY_COLORS = {
  "Liberal":                  "var(--party-liberal)",
  "PC/Conservative":          "var(--party-conservative)",
  "CCF/NDP":                  "var(--party-ndp)",
  "Social Credit/Creditiste": "var(--party-social-credit)",
  "Labour Progressive":       "var(--party-labour)",
  "Bloc Quebecois":           "var(--party-bloc-quebecois)",
  "Bloc Populaire":           "var(--party-bloc-populaire)",
  "Reform/Alliance":          "var(--party-reform)",
  "Green":                    "var(--party-green)",
  "Other":                    "var(--party-other)",
  "Undecided":                "var(--party-undecided)",
};

const statusEl = document.getElementById("status");
const cardEl = document.getElementById("card");
const domainSelect = document.getElementById("domain-select");
const questionSelect = document.getElementById("question-select");
const groupSelect = document.getElementById("group-select");
const yearMinInput = document.getElementById("year-min");
const yearMaxInput = document.getElementById("year-max");
const yearRangeLabel = document.getElementById("year-range-label");
const yearSliderRangeEl = document.getElementById("year-slider-range");
const partyFieldEl = document.getElementById("party-field");
const partyChecksEl = document.getElementById("party-checks");
const partyCountEl = document.getElementById("party-count");

const MAX_PARTIES = 6;

let conn = null;
let catalog = []; // [{question, domain, category, title, issue_label, wording}]
let currentQuestion = null; // question the year bounds / party options were last loaded for
let partySelection = new Set();

function setStatus(text, isError = false) {
  statusEl.textContent = text;
  statusEl.classList.toggle("error", isError);
}

function escapeSql(s) {
  return String(s).replace(/'/g, "''");
}

// ---------------------------------------------------------------------------
// 1. Boot DuckDB-WASM and load the database file.
// ---------------------------------------------------------------------------

async function initDB() {
  const bundles = duckdb.getJsDelivrBundles();
  const bundle = await duckdb.selectBundle(bundles);
  const workerUrl = URL.createObjectURL(
    new Blob([`importScripts("${bundle.mainWorker}");`], { type: "text/javascript" })
  );
  const worker = new Worker(workerUrl);
  const logger = new duckdb.ConsoleLogger(duckdb.LogLevel.WARNING);
  const db = new duckdb.AsyncDuckDB(logger, worker);
  await db.instantiate(bundle.mainModule, bundle.pthreadWorker);
  URL.revokeObjectURL(workerUrl);

  setStatus("Loading question catalog…");
  const connection = await db.connect();
  // vbl_data and code_labels are tiny (a few hundred KB combined) and every
  // query below needs one or both, so load them in full as views over the
  // hosted Parquet files right away. `resp` deliberately has no equivalent
  // here — it's ~72MB across all questions, and the whole point of this
  // rewrite is to never fetch more of it than the one selected question
  // needs (see respUrlFor() above and fetchChartData() below).
  await connection.query(`CREATE VIEW vbl_data AS SELECT * FROM read_parquet('${VBL_DATA_URL}')`);
  await connection.query(`CREATE VIEW code_labels AS SELECT * FROM read_parquet('${CODE_LABELS_URL}')`);
  return connection;
}

async function runQuery(sql) {
  const result = await conn.query(sql);
  return result.toArray().map(row => row.toJSON());
}

// ---------------------------------------------------------------------------
// 2. Catalog (domain / question picker), driven entirely by vbl_data.
// ---------------------------------------------------------------------------

async function loadCatalog() {
  const rows = await runQuery(`
    SELECT
      question,
      any_value(domain)   AS domain,
      any_value(category) AS category,
      any_value(title)       AS title,
      any_value(issue_label) AS issue_label,
      any_value(question_wording) AS wording  -- vbl_data's real column name
    FROM vbl_data
    GROUP BY question
    ORDER BY domain, category, question
  `);
  return rows;
}

function populateDomainPicker() {
  const domains = [...new Set(catalog.map(r => r.domain))].filter(Boolean);
  domainSelect.innerHTML = "";
  for (const d of domains) {
    const opt = document.createElement("option");
    opt.value = d;
    opt.textContent = DOMAIN_LABELS[d] || d;
    domainSelect.appendChild(opt);
  }
}

function populateQuestionPicker() {
  const domain = domainSelect.value;
  const qs = catalog.filter(r => r.domain === domain);
  questionSelect.innerHTML = "";
  for (const q of qs) {
    const opt = document.createElement("option");
    opt.value = q.question;
    // The dropdown shows the short policy-issue label (issue_label); the
    // full question wording is too long to show inline, so it's set as a
    // hover tooltip instead. vbl_data.title is still a placeholder (see
    // populate_db.R) - not used here.
    opt.textContent = q.issue_label || q.question;
    opt.title = q.wording || q.issue_label || "";
    questionSelect.appendChild(opt);
  }
}

function populateGroupPicker() {
  groupSelect.innerHTML = "";
  for (const g of GROUP_VARS) {
    const opt = document.createElement("option");
    opt.value = g.value;
    opt.textContent = g.label;
    groupSelect.appendChild(opt);
  }
}

// ---------------------------------------------------------------------------
// 3. Query layer.
// ---------------------------------------------------------------------------

// Resolves the question's dominant vbl_data.type and how many (question,
// survey_obs) rows use a different type (excluded from the chart below —
// see docs/data_quality_check.md item 3).
async function getQuestionMeta(question) {
  const rows = await runQuery(`
    SELECT type, COUNT(*) AS n
    FROM vbl_data
    WHERE question = '${escapeSql(question)}'
    GROUP BY type
    ORDER BY n DESC
  `);
  const type = rows[0].type;
  const excludedWaves = rows.slice(1).reduce((sum, r) => sum + Number(r.n), 0);
  return { type, excludedWaves };
}

// Question's overall year coverage (across all its survey waves), used to
// set the year-range slider's bounds whenever the question changes.
async function getYearBounds(question) {
  // `year` lives directly on resp now (denormalized in populate_db.R), and
  // this file is already scoped to exactly this question, so no WHERE/JOIN
  // is needed at all — a real simplification over the old survey-join version.
  const rows = await runQuery(`
    SELECT MIN(year) AS lo, MAX(year) AS hi
    FROM read_parquet('${respUrlFor(question)}')
  `);
  return { lo: Math.floor(rows[0].lo), hi: Math.ceil(rows[0].hi) };
}

// For "party" questions: every party code_labels knows about for this
// question (fixed list, e.g. vote_intention's 11 historical parties), plus a
// sensible default selection — the top MAX_PARTIES by total weight across
// the question's full year range. Recomputed only when the question changes,
// not on every year-slider tweak, so picker state stays put while the user
// explores a range.
async function getPartyOptions(question) {
  const q = escapeSql(question);
  const options = await runQuery(`
    SELECT label, sort_order FROM code_labels
    WHERE variable = '${q}' ORDER BY sort_order
  `);
  const totals = await runQuery(`
    SELECT cl.label AS label, SUM(r.weight) AS w
    FROM read_parquet('${respUrlFor(question)}') r
    JOIN code_labels cl ON cl.variable = r.question AND cl.code = r.response
    GROUP BY cl.label ORDER BY w DESC
    LIMIT ${MAX_PARTIES}
  `);
  return { options, defaultSelection: new Set(totals.map(r => r.label)) };
}

// Builds and runs the main weighted-aggregation query. `type` is the
// question's dominant type (from getQuestionMeta); `groupVar` is "" for no
// grouping or one of GROUP_VARS' values otherwise; `yearLo`/`yearHi` filter
// to the selected year range; `parties` (Set, "party" type only) folds every
// party not in the set into "Other" rather than dropping it.
//
// Label application happens entirely via JOIN to code_labels (kept out of
// JS, per project decision): response categories join on
// response_fac_<type> for Likert questions, or on the question code itself
// for "party" questions (vote_intention/vote_choice reuse the same
// code_labels rows whether they're the chart subject or a grouping var).
async function fetchChartData(question, type, groupVar, yearLo, yearHi, parties) {
  const q = escapeSql(question);
  const isParty = type === "party";
  const respVariable = isParty ? q : `response_fac_${type}`;
  const grouped = groupVar !== "";

  const grpSelect = grouped ? `cl_grp.label AS grp, cl_grp.sort_order AS grp_sort,` : "";
  // No more `indiv` join: every GROUP_VARS column is already denormalized
  // directly onto each resp row (see populate_db.R's "Denormalize onto
  // resp" section), so the grouping value is just r.<groupVar> — one join
  // (to code_labels, for the display label) instead of two.
  const grpJoin = grouped
    ? `JOIN code_labels cl_grp ON cl_grp.variable = '${escapeSql(groupVar)}' AND cl_grp.code = r.${groupVar}`
    : "";
  const grpPartition = grouped ? ", cl_grp.label" : "";
  const grpGroupBy = grouped ? "cl_grp.label, cl_grp.sort_order," : "";
  const grpOrderBy = grouped ? "cl_grp.sort_order," : "";

  // Parties not in the picker's selection get folded into "Other" (matching
  // the "pick up to 6, rest -> Other" design) rather than dropped, so the
  // stack still sums to 100%.
  let respLabelExpr = "cl_resp.label";
  let respSortExpr = "cl_resp.sort_order";
  if (isParty && parties && parties.size > 0) {
    const inList = [...parties].map(p => `'${escapeSql(p)}'`).join(", ");
    respLabelExpr = `CASE WHEN cl_resp.label IN (${inList}) THEN cl_resp.label ELSE 'Other' END`;
    respSortExpr = `CASE WHEN cl_resp.label IN (${inList}) THEN cl_resp.sort_order ELSE 999999 END`;
  }

  const lo = Number.isFinite(yearLo) ? yearLo : -999999;
  const hi = Number.isFinite(yearHi) ? yearHi : 999999;

  // FROM is a single question-partitioned Parquet file (one network fetch),
  // not the old whole-table `resp` — so there's no `r.question = ...` filter
  // to write (every row in the file already is this question) and no
  // `survey` join for year (resp.year is already the authoritative
  // survey.year value, baked in at build time).
  const sql = `
    SELECT
      r.year AS year,
      ${grpSelect}
      ${respLabelExpr} AS response_fac,
      ${respSortExpr} AS resp_sort,
      SUM(r.weight) AS w,
      SUM(SUM(r.weight)) OVER (PARTITION BY r.year${grpPartition}) AS group_total_w,
      100.0 * SUM(SUM(r.weight * r.response)) OVER (PARTITION BY r.year${grpPartition})
            / SUM(SUM(r.weight)) OVER (PARTITION BY r.year${grpPartition}) AS avg_native
    FROM read_parquet('${respUrlFor(question)}') r
    JOIN vbl_data v ON v.question = r.question AND v.survey_obs = r.survey_obs
    ${grpJoin}
    JOIN code_labels cl_resp ON cl_resp.variable = '${escapeSql(respVariable)}' AND cl_resp.code = r.response
    WHERE v.type = '${escapeSql(type)}'
      AND r.year BETWEEN ${lo} AND ${hi}
    GROUP BY r.year, ${grpGroupBy} response_fac, resp_sort
    ORDER BY r.year, ${grpOrderBy} resp_sort
  `;
  return runQuery(sql);
}

// ---------------------------------------------------------------------------
// 4. Shaping + chart rendering.
// ---------------------------------------------------------------------------

// Generic diverging-stack transform: given a {category: share} map and the
// categories in low->high order, returns rect segments centered on zero.
// n odd -> the exact middle category is split in half, one half per side.
// n even -> no split; the low half of categories stacks negative, high half positive.
function divergingSegments(shareByCategory, orderedCats) {
  const n = orderedCats.length;
  const midIdx = n % 2 === 1 ? (n - 1) / 2 : null;
  const segs = [];
  let negCursor = 0, posCursor = 0;

  if (midIdx !== null) {
    const cat = orderedCats[midIdx];
    const v = shareByCategory[cat] || 0;
    const half = v / 2;
    segs.push({ category: cat, y0: 0, y1: -half, share: v });
    segs.push({ category: cat, y0: 0, y1: half, share: v });
    negCursor = -half;
    posCursor = half;
  }

  const negIdx = [], posIdx = [];
  for (let i = 0; i < n; i++) {
    if (i === midIdx) continue;
    (i < (midIdx !== null ? midIdx : n / 2) ? negIdx : posIdx).push(i);
  }
  for (let k = negIdx.length - 1; k >= 0; k--) {
    const cat = orderedCats[negIdx[k]];
    const v = shareByCategory[cat] || 0;
    segs.push({ category: cat, y0: negCursor, y1: negCursor - v, share: v });
    negCursor -= v;
  }
  for (const i of posIdx) {
    const cat = orderedCats[i];
    const v = shareByCategory[cat] || 0;
    segs.push({ category: cat, y0: posCursor, y1: posCursor + v, share: v });
    posCursor += v;
  }
  return segs;
}

// Plain (non-diverging) stacking, low category first — used for "party" type,
// which has no natural neutral midpoint.
function stackedSegments(shareByCategory, orderedCats) {
  let cursor = 0;
  const segs = [];
  for (const cat of orderedCats) {
    const v = shareByCategory[cat] || 0;
    segs.push({ category: cat, y0: cursor, y1: cursor + v, share: v });
    cursor += v;
  }
  return segs;
}

function divergingAverage(avgNative, values) {
  const minN = Math.min(...values) * 100, maxN = Math.max(...values) * 100;
  return ((avgNative - minN) / (maxN - minN)) * 200 - 100;
}

function renderChart(question, meta, rows, groupVar, wording, issueLabel) {
  const isParty = meta.type === "party";
  const grouped = groupVar !== "";

  // Show the card before measuring cardEl.clientWidth below -- on the very
  // first render it starts as display:none (0 width), which would otherwise
  // wreck the facet-wrap column math.
  cardEl.style.display = "block";

  // Bold heading = issue_label (short policy-issue label, same text as the
  // picker). Regular-weight subhead = the full question wording. The raw
  // question code and type are no longer shown to the user.
  document.getElementById("title").textContent = issueLabel || question;
  document.getElementById("wording").textContent =
    (wording || question) + (grouped ? `  ·  grouped by ${groupVar}` : "");

  const flagEl = document.getElementById("flag");
  if (meta.excludedWaves > 0) {
    flagEl.hidden = false;
    flagEl.textContent = `Note: ${meta.excludedWaves} survey wave(s) for this question used a ` +
      `different response scale and were excluded rather than mislabeled — see docs/data_quality_check.md.`;
  } else {
    flagEl.hidden = true;
  }

  // Determine category order: fixed for Likert types, derived from the data
  // (via sort_order, which for "party" questions matches code_labels' vote
  // display order) for "party" questions since the party set varies by era.
  let orderedCats;
  if (isParty) {
    const seen = new Map();
    for (const r of rows) {
      const sortVal = Number(r.resp_sort);
      seen.set(r.response_fac, Math.max(seen.get(r.response_fac) ?? -Infinity, sortVal));
    }
    orderedCats = [...seen.entries()].sort((a, b) => a[1] - b[1]).map(e => e[0]);
  } else {
    orderedCats = TYPE_CONFIG[meta.type].categories;
  }

  const groupKeyOf = grouped ? (r => `${r.year}||${r.grp}`) : (r => String(r.year));
  const groups = d3.group(rows, groupKeyOf);

  const segments = [];
  const avgRows = [];
  for (const [, groupRows] of groups) {
    const year = +groupRows[0].year;
    const grp = grouped ? groupRows[0].grp : undefined;
    // BIGINT from DuckDB -- Number()-wrap per the same gotcha as resp_sort.
    const grpSort = grouped ? Number(groupRows[0].grp_sort) : undefined;
    const shareByCat = {};
    // Sum (not overwrite) per response_fac -- some questions' real
    // "type: party" response codes already include a literal "Other" party
    // (e.g. vote_choice's code_labels has its own "Other" row), which
    // collides with the synthetic "rest folded into Other" bucket built by
    // fetchChartData's CASE WHEN: same response_fac string, different
    // resp_sort, so `GROUP BY ..., response_fac, resp_sort` emits two rows
    // for the same year/label. Overwriting silently dropped whichever row
    // was processed first -- that's why some stacked bars weren't reaching
    // 100% (the dropped row's weight vanished from the chart, not the data).
    for (const r of groupRows) {
      shareByCat[r.response_fac] = (shareByCat[r.response_fac] || 0) + (r.w / r.group_total_w) * 100;
    }
    const segs = isParty
      ? stackedSegments(shareByCat, orderedCats)
      : divergingSegments(shareByCat, orderedCats);
    for (const s of segs) segments.push(Object.assign({ year, grp, grpSort }, s));
    if (!isParty) {
      const avgNative = groupRows[0].avg_native;
      avgRows.push({ year, grp, grpSort, divAvg: divergingAverage(avgNative, TYPE_CONFIG[meta.type].values), avgNative });
    }
  }

  const container = document.getElementById("chart");
  container.innerHTML = "";

  // Groups in their intended display order (code_labels.sort_order) rather
  // than Plot's default alphabetical inference for an unspecified fx domain
  // -- without this, e.g. "region" facets read Atlantic, British Columbia,
  // Ontario, Prairies, Quebec instead of the geographic Atlantic, Quebec,
  // Ontario, Prairies, British Columbia order code_labels defines.
  let groupsSorted = [];
  if (grouped) {
    const sortByGrp = new Map();
    for (const s of segments) sortByGrp.set(s.grp, s.grpSort);
    groupsSorted = [...sortByGrp.entries()].sort((a, b) => a[1] - b[1]).map(e => e[0]);
  }

  const colorRange = isParty
    ? orderedCats.map((cat, i) => PARTY_COLORS[cat] || CATEGORICAL_COLORS[i % CATEGORICAL_COLORS.length])
    : POLE_COLORS[orderedCats.length];

  const baseOpts = {
    height: 320,
    marginLeft: 52,
    marginRight: 20,
    marginBottom: 70, // extra room below the rotated tick labels so "Year" doesn't overlap them
    marginTop: 28,
    x: { type: "band", label: "Year", tickRotate: -90, padding: 0.35, tickFormat: v => String(v) },
    y: isParty
      ? { domain: [0, 100], tickFormat: v => `${v}%`, label: "Share", grid: true }
      : {
          domain: [-100, 100],
          ticks: [-100, -50, 0, 50, 100],
          tickFormat: v => `${Math.abs(v)}%`,
          label: null,
          grid: true,
        },
  };

  function buildMarks(segmentsSubset, avgRowsSubset, faceted) {
    return [
      ...(isParty ? [] : [Plot.ruleY([0], { stroke: "var(--baseline)" })]),
      Plot.rectY(segmentsSubset, {
        x: "year", y1: "y0", y2: "y1", fill: "category",
        stroke: "var(--surface-1)", strokeWidth: 1.5, rx: 2,
        ...(faceted ? { fx: "grp" } : {}),
        title: d => `${faceted ? d.grp + " · " : ""}${d.year}\n${d.category}: ${d.share.toFixed(1)}%`,
      }),
      // A native SVG <title> tooltip (set via the `title` channel above) is
      // unreliable in practice -- long hover delay, easy to miss, some
      // browsers barely show it at all -- so pair every mark with a real
      // Plot.tip that tracks the pointer and appears immediately.
      Plot.tip(segmentsSubset, Plot.pointer({
        x: "year", y: d => (d.y0 + d.y1) / 2,
        ...(faceted ? { fx: "grp" } : {}),
        title: d => `${faceted ? d.grp + " · " : ""}${d.year}\n${d.category}: ${d.share.toFixed(1)}%`,
        fill: "var(--surface-1)", stroke: "var(--border)",
      })),
      ...(isParty ? [] : [
        Plot.lineY(avgRowsSubset, {
          x: "year", y: "divAvg", stroke: "var(--text-primary)", strokeWidth: 2, curve: "linear",
          ...(faceted ? { fx: "grp" } : {}),
        }),
        Plot.dot(avgRowsSubset, {
          x: "year", y: "divAvg", r: 3, fill: "var(--text-primary)",
          stroke: "var(--surface-1)", strokeWidth: 1.5,
          ...(faceted ? { fx: "grp" } : {}),
          title: d => `${faceted ? d.grp + " · " : ""}${d.year}\nAverage: ${d.avgNative.toFixed(1)}%`,
        }),
        // No Plot.tip for the average line: its dots sit close enough to the
        // bar segments that the pointer-proximity tip more often than not
        // grabbed the average instead of whichever segment was actually
        // being hovered. The native <title> on the dot above still covers
        // it as a fallback; the segment tip is the one that matters.
      ]),
    ];
  }

  // Facet width: wide enough that a long-running series (e.g. vote_intention
  // spans ~70 annual bars) doesn't get crushed into hairlines, but capped so
  // a short series doesn't waste space. ncols is how many of those facets
  // fit across the card's actual on-screen width -- when a grouping variable
  // has more levels than that (province: 13, occupation: 10, ...), the
  // facets wrap onto additional rows instead of forcing one long
  // horizontally-scrolling strip with illegibly narrow columns.
  const nYearsDistinct = new Set(rows.map(r => r.year)).size;
  const facetMinWidth = Math.max(220, Math.min(420, nYearsDistinct * 4));
  const availableWidth = Math.max(cardEl.clientWidth - 40, 400);
  const ncols = grouped
    ? (isParty
        // Party questions (vote_choice: 58 distinct years, vote_intention:
        // 67) are too dense -- many stacked colors over many years -- to
        // share a row with other facets and stay legible. One full-width
        // facet per row instead, so the years stay readable; see rowWidth
        // below for the width side of this.
        ? 1
        : Math.max(1, Math.min(groupsSorted.length, Math.floor(availableWidth / facetMinWidth))))
    : 1;
  // Total width for one row of facets: the full card width for the
  // one-facet-per-row party case (no need to leave room for siblings that
  // don't exist), otherwise facetMinWidth per facet plus the fixed
  // marginLeft/marginRight overhead (paid once per row, not per facet --
  // see the note above these vars' first use for why that distinction
  // matters).
  const rowWidth = chunkLength =>
    isParty ? availableWidth : chunkLength * facetMinWidth + baseOpts.marginLeft + baseOpts.marginRight;

  if (!grouped) {
    // No facets to share the card with -- use the full available width,
    // same as the single-facet-per-row party case above.
    const plotOpts = {
      ...baseOpts,
      width: availableWidth,
      color: { legend: true, domain: orderedCats, range: colorRange },
      marks: buildMarks(segments, avgRows, false),
    };
    container.appendChild(Plot.plot(plotOpts));
  } else if (ncols >= groupsSorted.length) {
    // All facets fit in one row at a legible width -- a single Plot.plot()
    // call, same as before, just with the sort_order-correct facet order.
    const plotOpts = {
      ...baseOpts,
      width: rowWidth(groupsSorted.length),
      color: { legend: true, domain: orderedCats, range: colorRange },
      fx: { domain: groupsSorted, label: null },
      marks: buildMarks(segments, avgRows, true),
    };
    container.appendChild(Plot.plot(plotOpts));
  } else {
    // Too many groups for one legible row -- wrap into multiple rows of
    // `ncols` facets each (its own independent Plot.plot() call per row,
    // since Plot has no built-in facet-wrap), with a single shared legend
    // above them instead of a duplicate legend per row.
    const legend = Plot.legend({ color: { domain: orderedCats, range: colorRange } });
    legend.style.marginBottom = "8px";
    container.appendChild(legend);
    for (let i = 0; i < groupsSorted.length; i += ncols) {
      const chunk = groupsSorted.slice(i, i + ncols);
      const chunkSegments = segments.filter(s => chunk.includes(s.grp));
      const chunkAvgRows = avgRows.filter(s => chunk.includes(s.grp));
      const plotOpts = {
        ...baseOpts,
        width: rowWidth(chunk.length),
        color: { domain: orderedCats, range: colorRange },
        fx: { domain: chunk, label: null },
        marks: buildMarks(chunkSegments, chunkAvgRows, true),
      };
      const row = Plot.plot(plotOpts);
      row.style.marginBottom = "12px";
      container.appendChild(row);
    }
  }
}

// ---------------------------------------------------------------------------
// 5. Year-range slider and party-picker UI.
// ---------------------------------------------------------------------------

function currentYearRange() {
  let lo = +yearMinInput.value, hi = +yearMaxInput.value;
  if (lo > hi) [lo, hi] = [hi, lo];
  return { lo, hi };
}

function updateYearLabel() {
  const { lo, hi } = currentYearRange();
  yearRangeLabel.textContent = `${lo}–${hi}`;
}

// Positions the highlighted "selected range" bar between the two overlaid
// thumbs -- this is what makes two native <input type="range"> elements read
// as a single two-headed slider instead of two stacked ones.
function updateYearSliderVisual() {
  const min = +yearMinInput.min, max = +yearMinInput.max;
  const { lo, hi } = currentYearRange();
  const span = max - min;
  if (!Number.isFinite(span) || span <= 0) {
    yearSliderRangeEl.style.left = "0%";
    yearSliderRangeEl.style.width = "100%";
    return;
  }
  const loPct = ((lo - min) / span) * 100;
  const hiPct = ((hi - min) / span) * 100;
  yearSliderRangeEl.style.left = `${loPct}%`;
  yearSliderRangeEl.style.width = `${hiPct - loPct}%`;
}

async function refreshYearBounds(question) {
  const bounds = await getYearBounds(question);
  yearMinInput.min = yearMaxInput.min = bounds.lo;
  yearMinInput.max = yearMaxInput.max = bounds.hi;
  yearMinInput.value = bounds.lo;
  yearMaxInput.value = bounds.hi;
  updateYearLabel();
  updateYearSliderVisual();
}

function renderPartyChecks(options) {
  partyChecksEl.innerHTML = "";
  for (const opt of options) {
    const wrap = document.createElement("label");
    wrap.className = "party-check";
    const cb = document.createElement("input");
    cb.type = "checkbox";
    cb.value = opt.label;
    cb.checked = partySelection.has(opt.label);
    const span = document.createElement("span");
    span.textContent = opt.label;
    wrap.appendChild(cb);
    wrap.appendChild(span);
    partyChecksEl.appendChild(wrap);

    cb.addEventListener("change", () => {
      if (cb.checked) {
        if (partySelection.size >= MAX_PARTIES) { cb.checked = false; return; }
        partySelection.add(cb.value);
      } else {
        partySelection.delete(cb.value);
      }
      syncPartyCheckboxDisabled();
      updatePartyCountLabel();
      update();
    });
  }
  syncPartyCheckboxDisabled();
  updatePartyCountLabel();
}

function syncPartyCheckboxDisabled() {
  const atLimit = partySelection.size >= MAX_PARTIES;
  for (const cb of partyChecksEl.querySelectorAll('input[type="checkbox"]')) {
    cb.disabled = atLimit && !cb.checked;
  }
}

function updatePartyCountLabel() {
  partyCountEl.textContent = `(${partySelection.size} of ${MAX_PARTIES} — rest folded into "Other")`;
}

async function refreshPartyOptions(question) {
  const { options, defaultSelection } = await getPartyOptions(question);
  partySelection = defaultSelection;
  renderPartyChecks(options);
}

// ---------------------------------------------------------------------------
// 6. Wiring.
// ---------------------------------------------------------------------------

async function update() {
  const question = questionSelect.value;
  if (!question) { cardEl.style.display = "none"; return; }
  const groupVar = groupSelect.value;
  const meta = await getQuestionMeta(question);
  const isParty = meta.type === "party";

  if (question !== currentQuestion) {
    currentQuestion = question;
    await refreshYearBounds(question);
    if (isParty) await refreshPartyOptions(question);
  }
  partyFieldEl.hidden = !isParty;

  const { lo, hi } = currentYearRange();
  const rows = await fetchChartData(
    question, meta.type, groupVar, lo, hi,
    isParty ? partySelection : null
  );
  const cat = catalog.find(c => c.question === question) || {};
  renderChart(question, meta, rows, groupVar, cat.wording, cat.issue_label);
}

async function main() {
  try {
    conn = await initDB();
    setStatus("Loading question catalog…");
    catalog = await loadCatalog();
    populateDomainPicker();
    populateQuestionPicker();
    populateGroupPicker();
    setStatus(`Ready — ${catalog.length} questions loaded.`);

    domainSelect.addEventListener("change", () => { populateQuestionPicker(); update(); });
    questionSelect.addEventListener("change", update);
    groupSelect.addEventListener("change", update);
    yearMinInput.addEventListener("input", () => {
      if (+yearMinInput.value > +yearMaxInput.value) yearMaxInput.value = yearMinInput.value;
      updateYearLabel();
      updateYearSliderVisual();
      update();
    });
    yearMaxInput.addEventListener("input", () => {
      if (+yearMaxInput.value < +yearMinInput.value) yearMinInput.value = yearMaxInput.value;
      updateYearLabel();
      updateYearSliderVisual();
      update();
    });
    // Two native range inputs are overlaid to form one two-headed slider
    // (see .year-slider in style.css). When their values are close together
    // the thumb underneath can get "stuck" and unreachable, so raise
    // whichever thumb the user actually grabs above its sibling for the
    // duration of that interaction.
    for (const input of [yearMinInput, yearMaxInput]) {
      input.addEventListener("pointerdown", () => {
        yearMinInput.classList.remove("active-thumb");
        yearMaxInput.classList.remove("active-thumb");
        input.classList.add("active-thumb");
      });
    }

    await update();
  } catch (err) {
    console.error(err);
    setStatus(`Error: ${err.message}`, true);
  }
}

main();
