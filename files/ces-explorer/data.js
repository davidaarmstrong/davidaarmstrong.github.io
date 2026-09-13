// Loads data/ces_data.json (column-oriented, see data-export.R) and zips it
// into row objects, which is what stats.js expects.

export async function loadData() {
  const res = await fetch("data/ces_data.json");
  if (!res.ok) throw new Error(`Failed to load data/ces_data.json (${res.status})`);
  const json = await res.json();
  const cols = Object.keys(json.data);
  const n = json.meta.n;
  const rows = new Array(n);
  for (let i = 0; i < n; i++) {
    const row = {};
    for (const c of cols) row[c] = json.data[c][i];
    rows[i] = row;
  }
  // DISPLAY_LABELS wins over json.labels (the R `label` attribute, when the
  // .rda happens to carry one): ces0421.rda had them, ces0425.rda doesn't
  // (confirmed via str() -- every column's `label` attr is simply absent
  // there), which silently degraded every axis title and table header to
  // raw variable names ("leader_con" instead of "Feelings: Conservative")
  // until this was noticed and fixed. Making our own list authoritative
  // means the app's display text no longer depends on that upstream R
  // metadata detail at all, so this can't recur with some future .rda.
  const labels = { ...json.labels, ...DISPLAY_LABELS };
  return { rows, levels: json.levels, labels, meta: json.meta };
}

// The "Summarise Variable" list for the Descriptives tab (numeric feeling
// thermometers only), matching the old ui.r val_num_vars/val_num_names.
export const NUM_VARS = [
  { value: "leader_bloc", label: "Feeling Thermometer: BQ Leader" },
  { value: "leader_lib", label: "Feeling Thermometer: Liberal Leader" },
  { value: "leader_ndp", label: "Feeling Thermometer: NDP Leader" },
  { value: "leader_con", label: "Feeling Thermometer: Conservative" },
].sort((a, b) => a.label.localeCompare(b.label));

// The "Group By" / "Row Variable" / "Column Variable" list, matching the
// old ui.r val_strat_vars/val_strat_names ("None" always first, then
// alphabetical by label).
export const STRAT_VARS = [
  { value: "gender", label: "Gender" },
  { value: "agegrp", label: "Age Group" },
  { value: "cathol", label: "Catholic" },
  { value: "relig", label: "Religion" },
  { value: "educ", label: "Schooling" },
  { value: "union", label: "Union Household" },
  { value: "year", label: "Year" },
  { value: "province", label: "Region" },
  { value: "pid", label: "Party ID" },
  { value: "retroper", label: "Personal Retrospective Economy" },
  { value: "retrocan", label: "National Retrospective Economy" },
  { value: "sp_defence", label: "Defense Spending" },
  { value: "sp_envir", label: "Environment Spending" },
  { value: "immig", label: "More Immigration?" },
  { value: "vote", label: "Vote" },
  { value: "usties", label: "Ties to USA" },
  { value: "jobspriv", label: "Private Sector Create Jobs" },
  { value: "blame", label: "Blame Self for Failure" },
  { value: "poorgap", label: "Reduce Income Inequality" },
  { value: "dowomen", label: "Done for Women" },
  { value: "incumb_copart", label: "Incumbent Co-partisan" },
].sort((a, b) => a.label.localeCompare(b.label));

// Cross-Tabs "Row Variable" list: a restricted subset of STRAT_VARS. The
// row variable plays the dependent-variable role in this tab, so it's
// limited to actual outcome-shaped questions -- keeps students from doing
// things like predicting age group from vote choice (row=age, col=vote).
// The Column Variable picker still uses the full STRAT_VARS list.
export const ROW_VARS = STRAT_VARS.filter((v) => [
  "blame", "dowomen", "sp_defence", "sp_envir", "immig",
  "retrocan", "retroper", "jobspriv", "poorgap", "vote", "usties",
].includes(v.value));

// Models tab: Dependent Variable list, ported from the old ui.r/server.R
// dv.names. "Vote for Incumbent" (vote_incumbent) is dropped -- that
// column has never existed in the data (ces0421.rda nor ces0425.rda), a
// pre-existing bug in the old app's dropdown (see app-js/README.md).
export const DV_VARS = [
  { value: "vote_lib", label: "Vote Liberal" },
  { value: "vote_con", label: "Vote Conservative" },
  { value: "vote_ndp", label: "Vote NDP" },
  { value: "vote_bloc", label: "Vote BQ" },
  { value: "vote_green", label: "Vote Green" },
  { value: "turnout", label: "Vote Turnout" },
];

// Models tab: Independent Variable catalog, ported from the old ui.r/
// server.R `blocks`/`nblocks`. Grouped into <optgroup>-style blocks in the
// picker; flattened here with a `block` label for that purpose.
export const IV_BLOCKS = [
  {
    block: "Demographics",
    vars: [
      { value: "gender", label: "Gender" },
      { value: "agegrp", label: "Age Group" },
      { value: "cathol", label: "Catholic" },
      { value: "relig", label: "Religion" },
      { value: "educ", label: "Schooling" },
      { value: "union", label: "Union Household" },
      { value: "year_fac", label: "Year" },
      { value: "province", label: "Region" },
    ],
  },
  {
    block: "Party Identification",
    vars: [
      { value: "pid", label: "Party ID" },
      { value: "pidng", label: "Party ID (No Green)" },
      { value: "pidnb", label: "Party ID (No BQ)" },
      { value: "pidnbg", label: "Party ID (No Green or BQ)" },
    ],
  },
  {
    block: "Retrospective Evaluations",
    vars: [
      { value: "retroper", label: "Personal Retrospective" },
      { value: "retrocan", label: "National Retrospective" },
    ],
  },
  {
    block: "Issue Positions",
    vars: [
      { value: "sp_defence", label: "Defense Spend" },
      { value: "sp_envir", label: "Environment Spend" },
      { value: "immig", label: "More Immigration" },
      { value: "usties", label: "Ties to USA" },
      { value: "jobspriv", label: "Private Sector Create Jobs" },
      { value: "blame", label: "Blame Self for Failure" },
      { value: "poorgap", label: "Reduce Income Inequality" },
      { value: "dowomen", label: "Done for Women" },
    ],
  },
  {
    block: "Feelings",
    vars: [
      { value: "leader_con", label: "Feelings: Conservative" },
      { value: "leader_lib", label: "Feelings: Liberal" },
      { value: "leader_ndp", label: "Feelings: NDP" },
      { value: "leader_bloc", label: "Feelings: BQ" },
      { value: "leader_incumbent", label: "Feelings: Incumbent Leader" },
      { value: "incumb_copart", label: "Incumbent Co-partisan" },
    ],
  },
];
export const IV_VARS = IV_BLOCKS.flatMap((b) => b.vars);

// "Vary Effect by" -- adds an interaction between the chosen column and the
// focus variable. `column` is the actual data column (year uses the factor
// year_fac, not numeric year, so each year gets its own dummy/interaction
// term rather than being treated as continuous).
export const VARY_BY_OPTIONS = [
  { value: "none", label: "None", column: null },
  { value: "year", label: "Year", column: "year_fac" },
  { value: "region", label: "Region", column: "province" },
  { value: "incumbent", label: "Incumbent Co-partisan", column: "incumb_copart" },
];

// Consolidated {value: label} lookup for axis titles/table headers/
// coefficient rows across all three tabs, built from the picker lists
// above rather than any per-column metadata in the .rda file (see
// loadData() for why). Earlier list wins where the same variable appears
// in more than one list with different phrasing -- e.g. the four feeling
// thermometers are "Feeling Thermometer: X" in NUM_VARS (Descriptives)
// and the terser "Feelings: X" in IV_BLOCKS (Models); NUM_VARS is listed
// first so its fuller phrasing is what shows up everywhere.
const LABEL_SOURCES = [...NUM_VARS, ...STRAT_VARS, ...DV_VARS, ...IV_VARS];
export const DISPLAY_LABELS = {};
for (const { value, label } of LABEL_SOURCES) {
  if (!(value in DISPLAY_LABELS)) DISPLAY_LABELS[value] = label;
}

// `year` is numeric in the data, not a factor, so it has no entry in
// json.levels -- give it a synthetic ascending level order.
export function levelsFor(levels, varName, rows) {
  if (varName === "year") {
    // Numeric (not string) so it matches r.year's type when used as a
    // grouping key in stats.js.
    return [...new Set(rows.map((r) => r.year).filter((v) => v != null))]
      .sort((a, b) => a - b);
  }
  return levels[varName] || null;
}
