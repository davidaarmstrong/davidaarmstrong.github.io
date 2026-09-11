// Logistic regression (IRLS/Newton-Raphson) + average-case predicted
// probabilities, hand-rolled since there's no off-the-shelf JS survey/GLM
// package. This is unweighted (see data-export.R) and average-case only
// (the old app's average-effect option was already dead code -- see
// app-js/README.md). The average-case simulation here is a direct port of
// the old server.R `probci2` aveCase branch: simulate coefficient draws
// from N(beta, vcov), evaluate predicted probabilities at each draw, take
// quantiles for the median + 95% CI.

import { crossprodWeighted, crossprodWeightedVec, inverse, matVec, cholesky } from "./matrix.js";
import { quantile } from "./stats.js";

// pidng/pidnb/pidnbg are character columns in the exported data (party-ID
// variants), not JSON-typed factors, so they have no entry in `levels` --
// treat them as categorical anyway.
const CHAR_CATEGORICALS = ["pidng", "pidnb", "pidnbg"];

export function isCategorical(varName, levelsLookup) {
  return varName in levelsLookup || CHAR_CATEGORICALS.includes(varName);
}

// Levels of a categorical variable actually observed in `rows`, in their
// declared display order (falls back to sorted-alphabetical for the
// character columns, which have no declared order) -- matches the old
// server.R's `droplevels()` call on the subsetted data.
export function observedLevels(rows, varName, levelsLookup) {
  const observed = new Set(rows.map((r) => r[varName]).filter((v) => v != null));
  const declared = levelsLookup[varName];
  if (declared) return declared.filter((l) => observed.has(l));
  return [...observed].sort();
}

// Rows with no missing values on the DV or any model variable -- the
// listwise deletion glm()/svyglm() do implicitly.
export function analysisRows(rows, dv, requiredVars) {
  return rows.filter((r) => r[dv] != null && requiredVars.every((v) => r[v] != null));
}

// terms: ordered list of main-effect variable names (deduped). interaction:
// [varA, varB] or null. Mirrors the old app's `iv1 <- c(iv1, "year_fac*focus")`
// -- both interaction variables are added as main effects too if not
// already present.
export function buildSpec({ mainVars, varyByColumn, focusVar }) {
  const terms = [];
  const seen = new Set();
  const add = (v) => { if (v && !seen.has(v)) { seen.add(v); terms.push(v); } };
  mainVars.forEach(add);
  let interaction = null;
  if (varyByColumn && focusVar) {
    add(varyByColumn);
    add(focusVar);
    interaction = [varyByColumn, focusVar];
  }
  return { terms, interaction };
}

// A variable's dummy-coded (reference = first observed level) or numeric
// design-matrix parts.
function expandVar(varName, rows, levelsLookup) {
  if (isCategorical(varName, levelsLookup)) {
    const levels = observedLevels(rows, varName, levelsLookup);
    return levels.slice(1).map((level) => ({ var: varName, valueKind: "dummy", level }));
  }
  return [{ var: varName, valueKind: "numeric" }];
}

// Builds the ordered list of design-matrix columns for a spec (intercept,
// main-effect parts, then interaction parts -- interaction columns are the
// cross product of each side's own parts).
export function buildColumns({ terms, interaction }, rows, levelsLookup) {
  const columns = [{ role: "intercept" }];
  for (const t of terms) {
    for (const part of expandVar(t, rows, levelsLookup)) {
      columns.push({ role: "main", var: part.var, valueKind: part.valueKind, level: part.level });
    }
  }
  if (interaction) {
    const partsA = expandVar(interaction[0], rows, levelsLookup);
    const partsB = expandVar(interaction[1], rows, levelsLookup);
    for (const a of partsA) for (const b of partsB) columns.push({ role: "interaction", a, b });
  }
  return columns;
}

function partValue(part, row) {
  return part.valueKind === "numeric" ? Number(row[part.var]) : (row[part.var] === part.level ? 1 : 0);
}

function columnValue(col, row) {
  if (col.role === "intercept") return 1;
  if (col.role === "main") return partValue(col, row);
  return partValue(col.a, row) * partValue(col.b, row);
}

export function partLabel(part, labelsLookup) {
  const base = labelsLookup[part.var] || part.var;
  return part.valueKind === "numeric" ? base : `${base}: ${part.level}`;
}

export function columnLabel(col, labelsLookup) {
  if (col.role === "intercept") return "(Intercept)";
  if (col.role === "main") return partLabel(col, labelsLookup);
  return `${partLabel(col.a, labelsLookup)} × ${partLabel(col.b, labelsLookup)}`;
}

export function buildDesignMatrix(rows, columns) {
  return rows.map((row) => columns.map((col) => columnValue(col, row)));
}

function binomialDeviance(y, mu) {
  let d = 0;
  for (let i = 0; i < y.length; i++) {
    const m = Math.min(Math.max(mu[i], 1e-12), 1 - 1e-12);
    d += y[i] === 1 ? -2 * Math.log(m) : -2 * Math.log(1 - m);
  }
  return d;
}

// IRLS (Newton-Raphson via IRLS weights) for binomial logistic regression.
// Convergence check matches R's glm.fit default (relative change in
// deviance < 1e-8, 25 iterations) rather than a parameter-change
// criterion, so near-separation cases (e.g. a province-only predictor of
// BQ vote, which is ~0 everywhere outside Quebec) converge the same number
// of iterations R does instead of diverging further under a stricter test.
export function fitLogistic(X, y, { maxIter = 25, tol = 1e-8 } = {}) {
  const n = X.length;
  const p = X[0].length;
  let beta = new Array(p).fill(0);
  let dev = binomialDeviance(y, y.map(() => 0.5));
  let converged = false;
  for (let iter = 0; iter < maxIter; iter++) {
    const eta = matVec(X, beta);
    const mu = eta.map((e) => 1 / (1 + Math.exp(-e)));
    const w = mu.map((m) => Math.max(m * (1 - m), 1e-10));
    const z = eta.map((e, i) => e + (y[i] - mu[i]) / w[i]);
    const XtWX = crossprodWeighted(X, w);
    const XtWz = crossprodWeightedVec(X, w, z);
    beta = matVec(inverse(XtWX), XtWz);
    const newEta = matVec(X, beta);
    const newMu = newEta.map((e) => 1 / (1 + Math.exp(-e)));
    const newDev = binomialDeviance(y, newMu);
    if (Math.abs(newDev - dev) / (Math.abs(newDev) + 0.1) < tol) { dev = newDev; converged = true; break; }
    dev = newDev;
  }
  const eta = matVec(X, beta);
  const fitted = eta.map((e) => 1 / (1 + Math.exp(-e)));
  const w = fitted.map((m) => Math.max(m * (1 - m), 1e-10));
  const vcov = inverse(crossprodWeighted(X, w));
  const se = vcov.map((row, i) => Math.sqrt(row[i]));
  // A rough separation flag for the UI: coefficients this large only
  // happen when a predictor (near-)perfectly separates the outcome.
  const separation = beta.some((b) => Math.abs(b) > 15);
  return { coef: beta, vcov, se, fitted, converged, separation, deviance: dev, n, p };
}

// Proportional Reduction in Error diagnostics, port of the old server.R
// `pre()` function (its `mod2`/null-model quantities reduce to closed-form
// expressions of ybar since the null model always predicts p=ybar for
// everyone -- see app-js/README.md for the derivation).
export function computePRE(y, fitted) {
  const n = y.length;
  const ybar = y.reduce((a, b) => a + b, 0) / n;
  const nullPred = ybar >= 0.5 ? 1 : 0;
  let pmcHits = 0, pcpHits = 0, epcpSum = 0;
  for (let i = 0; i < n; i++) {
    if (nullPred === y[i]) pmcHits++;
    if ((fitted[i] >= 0.5 ? 1 : 0) === y[i]) pcpHits++;
    epcpSum += y[i] === 1 ? fitted[i] : 1 - fitted[i];
  }
  const pmc = pmcHits / n;
  const pcp = pcpHits / n;
  const pre = (pcp - pmc) / (1 - pmc);
  const epmc = ybar * ybar + (1 - ybar) * (1 - ybar);
  const epcp = epcpSum / n;
  const epre = (epcp - epmc) / (1 - epmc);
  return { pmc, pcp, pre, epmc, epcp, epre, n };
}

// "Typical" value for a variable not being varied: the modal level for a
// categorical variable or a numeric one with <=10 distinct values, else the
// median -- a direct port of the old server.R `central()`.
export function centralTendency(rows, varName, levelsLookup) {
  const vals = rows.map((r) => r[varName]).filter((v) => v != null);
  const categorical = isCategorical(varName, levelsLookup);
  const numericFewValues = !categorical && new Set(vals).size <= 10;
  if (categorical || numericFewValues) {
    const counts = new Map();
    for (const v of vals) counts.set(v, (counts.get(v) || 0) + 1);
    let best = null, bestN = -1;
    for (const [k, c] of counts) if (c > bestN) { best = k; bestN = c; }
    return best;
  }
  const sorted = [...vals].sort((a, b) => a - b);
  return quantile(sorted, 0.5);
}

// Grid values for a variable being varied in the average-case prediction:
// every observed level for a categorical variable, or [min, max] for a
// numeric one (the old app's `numQuantVals=2` in the Models tab -- a
// straightforward "first difference" from the low to the high end).
function gridValues(rows, varName, levelsLookup) {
  if (isCategorical(varName, levelsLookup)) return observedLevels(rows, varName, levelsLookup);
  const vals = rows.map((r) => r[varName]).filter((v) => v != null);
  const uniq = [...new Set(vals)];
  if (uniq.length <= 2) return uniq.sort((a, b) => a - b);
  return [Math.min(...vals), Math.max(...vals)];
}

function cartesianProduct(arrays) {
  return arrays.reduce((acc, arr) => acc.flatMap((a) => arr.map((v) => [...a, v])), [[]]);
}

// One synthetic row per combination of `changeVars` values, with every
// other model variable held at its central tendency.
export function averageCaseGrid({ rows, columns, changeVars, levelsLookup }) {
  const usedVars = new Set();
  for (const col of columns) {
    if (col.role === "main") usedVars.add(col.var);
    if (col.role === "interaction") { usedVars.add(col.a.var); usedVars.add(col.b.var); }
  }
  const others = [...usedVars].filter((v) => !changeVars.includes(v));
  const centralRow = {};
  for (const v of others) centralRow[v] = centralTendency(rows, v, levelsLookup);

  const valueLists = changeVars.map((v) => gridValues(rows, v, levelsLookup));
  const combos = cartesianProduct(valueLists);
  return combos.map((combo) => {
    const row = { ...centralRow };
    changeVars.forEach((v, i) => { row[v] = combo[i]; });
    return row;
  });
}

function randNormal() {
  let u = 0, v = 0;
  while (u === 0) u = Math.random();
  while (v === 0) v = Math.random();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

// nDraws x p simulated coefficient vectors from N(coef, vcov), via a
// Cholesky factor of vcov (the old app used MASS::mvrnorm's exact-moment
// "empirical=TRUE" correction; this is plain Monte Carlo simulation
// instead -- asymptotically identical, just with ordinary simulation noise
// that 2500 draws makes negligible for a 95% CI).
export function simulateCoefDraws(coef, vcov, nDraws = 2500) {
  const p = coef.length;
  const L = cholesky(vcov);
  const draws = [];
  for (let d = 0; d < nDraws; d++) {
    const z = Array.from({ length: p }, randNormal);
    const draw = new Array(p);
    for (let i = 0; i < p; i++) {
      let s = coef[i];
      for (let j = 0; j <= i; j++) s += L[i][j] * z[j];
      draw[i] = s;
    }
    draws.push(draw);
  }
  return draws;
}

// Median + 95% CI predicted probability for each row of an average-case
// grid, simulated from the model's coefficient distribution.
export function predictedProbabilities({ coef, vcov }, columns, gridRows, nDraws = 2500) {
  const draws = simulateCoefDraws(coef, vcov, nDraws);
  return gridRows.map((row) => {
    const x = columns.map((col) => columnValue(col, row));
    const probs = draws.map((beta) => {
      let eta = 0;
      for (let i = 0; i < x.length; i++) eta += x[i] * beta[i];
      return 1 / (1 + Math.exp(-eta));
    }).sort((a, b) => a - b);
    return {
      row,
      median: quantile(probs, 0.5),
      lower: quantile(probs, 0.025),
      upper: quantile(probs, 0.975),
    };
  });
}
