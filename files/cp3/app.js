// CP3 — DuckDB-WASM app.
//
// Architecture (see project memory / docs/data-model.md, docs/data_quality_check.md
// for the reasoning behind each of these):
//  - The whole ~1.7M-row dataset ships as one static file, surveydb.duckdb
//    (currently ~35.7MB, unindexed — indexes were benchmarked and dropped;
//    see populate_db.R). DuckDB-WASM loads it fully client-side, no backend.
//  - Label lookups (code_labels) are applied via SQL JOIN at query time, not
//    in JS — per-decision 2026-08-31.
//  - vbl_data.type can vary by survey_obs within one question (data_quality_check.md
//    item 3); every query here resolves the question's *dominant* type first and
//    filters to only the waves matching it, flagging any excluded waves in the UI,
//    exactly like prototypes/plot-diverging-poc-v2.html did.
//  - "year" comes from survey.year, not indiv.year, unlike the current Shiny app
//    (which uses indiv.year) — survey.year is the authoritative source for a
//    survey wave's date (see data_quality_check.md item 2); this is a deliberate,
//    small behavioral improvement, not a straight port.

import * as duckdb from "https://cdn.jsdelivr.net/npm/@duckdb/duckdb-wasm@1.32.0/+esm";
import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm";
import * as Plot from "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6.17/+esm";

const DB_URL = "../surveydb.duckdb";

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
let catalog = []; // [{question, domain, category, title, wording}]
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

  setStatus("Downloading survey database…");
  const res = await fetch(DB_URL);
  if (!res.ok) throw new Error(`Could not fetch ${DB_URL}: HTTP ${res.status}`);
  const buffer = new Uint8Array(await res.arrayBuffer());
  await db.registerFileBuffer("surveydb.duckdb", buffer);

  const connection = await db.connect();
  await connection.query(`ATTACH 'surveydb.duckdb' AS surveydb (READ_ONLY)`);
  await connection.query(`USE surveydb`);
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
      any_value(title)    AS title,
      any_value(wording)  AS wording
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
    // vbl_data.title is currently a placeholder ("<question>_title" - see
    // populate_db.R) for every row, not a real short title, so wording (which
    // is real, human-written text) is what's actually usable here. Swap this
    // back to q.title once real titles exist.
    opt.textContent = q.wording || q.question;
    opt.title = q.wording || "";
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
  const rows = await runQuery(`
    SELECT MIN(s.year) AS lo, MAX(s.year) AS hi
    FROM resp r JOIN survey s ON r.survey_obs = s.survey_obs
    WHERE r.question = '${escapeSql(question)}'
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
    FROM resp r JOIN code_labels cl ON cl.variable = r.question AND cl.code = r.response
    WHERE r.question = '${q}'
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
  const grpJoin = grouped
    ? `JOIN indiv i ON r.obs = i.obs
       JOIN code_labels cl_grp ON cl_grp.variable = '${escapeSql(groupVar)}' AND cl_grp.code = i.${groupVar}`
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

  const sql = `
    SELECT
      s.year AS year,
      ${grpSelect}
      ${respLabelExpr} AS response_fac,
      ${respSortExpr} AS resp_sort,
      SUM(r.weight) AS w,
      SUM(SUM(r.weight)) OVER (PARTITION BY s.year${grpPartition}) AS group_total_w,
      100.0 * SUM(SUM(r.weight * r.response)) OVER (PARTITION BY s.year${grpPartition})
            / SUM(SUM(r.weight)) OVER (PARTITION BY s.year${grpPartition}) AS avg_native
    FROM resp r
    JOIN survey s ON r.survey_obs = s.survey_obs
    JOIN vbl_data v ON v.question = r.question AND v.survey_obs = r.survey_obs
    ${grpJoin}
    JOIN code_labels cl_resp ON cl_resp.variable = '${escapeSql(respVariable)}' AND cl_resp.code = r.response
    WHERE r.question = '${q}' AND v.type = '${escapeSql(type)}'
      AND s.year BETWEEN ${lo} AND ${hi}
    GROUP BY s.year, ${grpGroupBy} response_fac, resp_sort
    ORDER BY s.year, ${grpOrderBy} resp_sort
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

function renderChart(question, meta, rows, groupVar, wording, title) {
  const isParty = meta.type === "party";
  const grouped = groupVar !== "";

  // vbl_data.title is a placeholder today (see populate_db.R note above), so
  // the question code is what actually distinguishes charts for now.
  document.getElementById("title").textContent = wording || question;
  document.getElementById("wording").textContent =
    question + `  ·  type: ${meta.type}` + (grouped ? `  ·  grouped by ${groupVar}` : "");

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
    for (const r of rows) seen.set(r.response_fac, r.resp_sort);
    orderedCats = [...seen.entries()].sort((a, b) => Number(a[1]) - Number(b[1])).map(e => e[0]);
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
    const shareByCat = {};
    for (const r of groupRows) shareByCat[r.response_fac] = (r.w / r.group_total_w) * 100;
    const segs = isParty
      ? stackedSegments(shareByCat, orderedCats)
      : divergingSegments(shareByCat, orderedCats);
    for (const s of segs) segments.push(Object.assign({ year, grp }, s));
    if (!isParty) {
      const avgNative = groupRows[0].avg_native;
      avgRows.push({ year, grp, divAvg: divergingAverage(avgNative, TYPE_CONFIG[meta.type].values), avgNative });
    }
  }

  const container = document.getElementById("chart");
  container.innerHTML = "";

  const nGroups = grouped ? new Set(rows.map(r => r.grp)).size : 1;
  const width = grouped ? Math.max(620, nGroups * 260) : 620;

  const colorRange = isParty
    ? orderedCats.map((cat, i) => PARTY_COLORS[cat] || CATEGORICAL_COLORS[i % CATEGORICAL_COLORS.length])
    : POLE_COLORS[orderedCats.length];

  const plotOpts = {
    width,
    height: 320,
    marginLeft: 52,
    marginBottom: 42,
    marginTop: 28,
    x: { type: "band", label: "Year", tickRotate: -90, padding: 0.35 },
    y: isParty
      ? { domain: [0, 100], tickFormat: v => `${v}%`, label: "Share", grid: true }
      : {
          domain: [-100, 100],
          ticks: [-100, -50, 0, 50, 100],
          tickFormat: v => `${Math.abs(v)}%`,
          label: `← ${orderedCats[0]}    ${orderedCats[orderedCats.length - 1]} →`,
          grid: true,
        },
    color: { legend: true, domain: orderedCats, range: colorRange },
    marks: [
      ...(isParty ? [] : [Plot.ruleY([0], { stroke: "var(--baseline)" })]),
      Plot.rectY(segments, {
        x: "year", y1: "y0", y2: "y1", fill: "category",
        stroke: "var(--surface-1)", strokeWidth: 1.5, rx: 2,
        ...(grouped ? { fx: "grp" } : {}),
        title: d => `${grouped ? d.grp + " · " : ""}${d.year}\n${d.category}: ${d.share.toFixed(1)}%`,
      }),
      // A native SVG <title> tooltip (set via the `title` channel above) is
      // unreliable in practice -- long hover delay, easy to miss, some
      // browsers barely show it at all -- so pair every mark with a real
      // Plot.tip that tracks the pointer and appears immediately.
      Plot.tip(segments, Plot.pointer({
        x: "year", y: d => (d.y0 + d.y1) / 2,
        ...(grouped ? { fx: "grp" } : {}),
        title: d => `${grouped ? d.grp + " · " : ""}${d.year}\n${d.category}: ${d.share.toFixed(1)}%`,
        fill: "var(--surface-1)", stroke: "var(--border)",
      })),
      ...(isParty ? [] : [
        Plot.lineY(avgRows, {
          x: "year", y: "divAvg", stroke: "var(--text-primary)", strokeWidth: 2, curve: "linear",
          ...(grouped ? { fx: "grp" } : {}),
        }),
        Plot.dot(avgRows, {
          x: "year", y: "divAvg", r: 3, fill: "var(--text-primary)",
          stroke: "var(--surface-1)", strokeWidth: 1.5,
          ...(grouped ? { fx: "grp" } : {}),
          title: d => `${grouped ? d.grp + " · " : ""}${d.year}\nAverage: ${d.avgNative.toFixed(1)}%`,
        }),
        // No Plot.tip for the average line: its dots sit close enough to the
        // bar segments that the pointer-proximity tip more often than not
        // grabbed the average instead of whichever segment was actually
        // being hovered. The native <title> on the dot above still covers
        // it as a fallback; the segment tip is the one that matters.
      ]),
    ],
  };
  if (grouped) plotOpts.fx = { label: null };

  container.appendChild(Plot.plot(plotOpts));
  cardEl.style.display = "block";
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
  renderChart(question, meta, rows, groupVar, cat.wording, cat.title);
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
