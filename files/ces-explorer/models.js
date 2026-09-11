import { DV_VARS, IV_BLOCKS, IV_VARS, VARY_BY_OPTIONS } from "./data.js";
import {
  buildSpec, buildColumns, buildDesignMatrix, analysisRows, columnLabel,
  fitLogistic, computePRE, averageCaseGrid, predictedProbabilities,
} from "./model.js";
import { renderPredictedProbabilities } from "./charts.js";

function fmt(x, digits = 3) {
  return x == null || Number.isNaN(x) ? "" : x.toFixed(digits);
}
function fmtN(x) {
  return x == null || Number.isNaN(x) ? "" : x.toLocaleString();
}
function sig(coef, se) {
  return Math.abs(se) > 0 && Math.abs(coef / se) >= 1.96 ? "*" : "";
}

export function initModels({ levels, labels }) {
  let rows = [];

  const dvSel = document.getElementById("dv");
  const mod1Sel = document.getElementById("mod1");
  const cmActive = document.getElementById("cmactive");
  const mod2Field = document.getElementById("mod2-field");
  const mod2Sel = document.getElementById("mod2");
  const varyBySel = document.getElementById("varyby");
  const focusVarSel = document.getElementById("focusvar");
  const estimateBtn = document.getElementById("estimate-btn");
  const statusEl = document.getElementById("model-status");

  const resultsWrap = document.getElementById("model-results");
  const coefWrap = document.getElementById("model-coef-wrap");
  const fitWrap = document.getElementById("model-fit-wrap");
  const plotWrap = document.getElementById("model-plot-wrap");
  const plotEl = document.getElementById("model-plot");
  const ppWrap = document.getElementById("model-pp-wrap");
  const ppTableEl = document.getElementById("model-pp-table");

  for (const { value, label } of DV_VARS) {
    const opt = document.createElement("option");
    opt.value = value; opt.textContent = label;
    dvSel.append(opt);
  }
  for (const sel of [mod1Sel, mod2Sel]) {
    for (const block of IV_BLOCKS) {
      const group = document.createElement("optgroup");
      group.label = block.block;
      for (const { value, label } of block.vars) {
        const opt = document.createElement("option");
        opt.value = value; opt.textContent = label;
        group.append(opt);
      }
      sel.append(group);
    }
  }
  for (const { value, label } of VARY_BY_OPTIONS) {
    const opt = document.createElement("option");
    opt.value = value; opt.textContent = label;
    varyBySel.append(opt);
  }

  function selectedValues(select) {
    return [...select.selectedOptions].map((o) => o.value);
  }

  // The focus-variable list is restricted to variables actually in the
  // model(s) -- if both models are active, only variables common to both,
  // so the "vary effect by" comparison plot is comparing the same
  // variable's effect across models. Live-updates as mod1/mod2 change;
  // does not itself trigger a refit (see models below on why refit is
  // gated behind the Refresh Model Output button only).
  function refreshFocusVarChoices() {
    const mod1Vars = selectedValues(mod1Sel);
    const mod2Vars = cmActive.checked ? selectedValues(mod2Sel) : [];
    const pool = cmActive.checked && mod2Vars.length
      ? mod1Vars.filter((v) => mod2Vars.includes(v))
      : mod1Vars;
    const prev = focusVarSel.value;
    focusVarSel.innerHTML = "";
    const blank = document.createElement("option");
    blank.value = ""; blank.textContent = pool.length ? "(none selected)" : "(no variables in model)";
    focusVarSel.append(blank);
    for (const v of IV_VARS) {
      if (!pool.includes(v.value)) continue;
      const opt = document.createElement("option");
      opt.value = v.value; opt.textContent = v.label;
      focusVarSel.append(opt);
    }
    if (pool.includes(prev)) focusVarSel.value = prev;
  }

  function invalidateResults(message) {
    resultsWrap.hidden = true;
    statusEl.textContent = message || "";
    statusEl.classList.remove("error");
  }

  function fitOne(dv, mainVars, varyByColumn, focusVar) {
    const spec = buildSpec({ mainVars, varyByColumn, focusVar });
    if (spec.terms.length === 0) return null;
    const arows = analysisRows(rows, dv, spec.terms);
    if (arows.length < spec.terms.length * 5) {
      throw new Error(`Not enough observations (n=${arows.length}) to estimate this model in the current subset.`);
    }
    const columns = buildColumns(spec, arows, levels);
    const X = buildDesignMatrix(arows, columns);
    const y = arows.map((r) => r[dv]);
    const fit = fitLogistic(X, y);
    const pre = computePRE(y, fit.fitted);
    return { spec, arows, columns, fit, pre };
  }

  function renderCoefTable(dvLabel, m1, m2) {
    const allCols = m2 ? [...new Map([...m1.columns, ...m2.columns].map((c) => [columnLabel(c, labels), c])).keys()] : m1.columns.map((c) => columnLabel(c, labels));
    const lookup = (m) => {
      const map = new Map();
      m.columns.forEach((c, i) => map.set(columnLabel(c, labels), { coef: m.fit.coef[i], se: m.fit.se[i] }));
      return map;
    };
    const l1 = lookup(m1);
    const l2 = m2 ? lookup(m2) : null;
    coefWrap.innerHTML = `
      <h2>Coefficients &mdash; ${dvLabel}</h2>
      <p class="hint">Logistic regression coefficients (log-odds), unweighted. Standard errors in parentheses. * p &lt; .05 (two-tailed).</p>
      <table class="data-table">
        <thead><tr><th>Term</th><th>Model 1</th>${m2 ? "<th>Model 2</th>" : ""}</tr></thead>
        <tbody>
          ${allCols.map((label) => {
            const e1 = l1.get(label);
            const e2 = l2 ? l2.get(label) : null;
            return `<tr>
              <td>${label}</td>
              <td>${e1 ? `${fmt(e1.coef)}${sig(e1.coef, e1.se)}<br><span style="color:var(--text-muted);font-size:11px">(${fmt(e1.se)})</span>` : ""}</td>
              ${m2 ? `<td>${e2 ? `${fmt(e2.coef)}${sig(e2.coef, e2.se)}<br><span style="color:var(--text-muted);font-size:11px">(${fmt(e2.se)})</span>` : ""}</td>` : ""}
            </tr>`;
          }).join("")}
        </tbody>
      </table>`;
  }

  function renderFitTable(m1, m2) {
    const rowsSpec = [
      ["N", (m) => fmtN(m.fit.n)],
      ["Prop. Correct (Null)", (m) => fmt(m.pre.pmc)],
      ["Prop. Correct (Model)", (m) => fmt(m.pre.pcp)],
      ["PRE", (m) => fmt(m.pre.pre)],
      ["Expected Prop. Correct (Null)", (m) => fmt(m.pre.epmc)],
      ["Expected Prop. Correct (Model)", (m) => fmt(m.pre.epcp)],
      ["Expected PRE", (m) => fmt(m.pre.epre)],
    ];
    const warn1 = m1.fit.separation ? `<p class="chisq-note">Model 1: some coefficients are extremely large/unstable &mdash; a predictor here likely (near-)perfectly separates the outcome (e.g. a region-only predictor of a region-specific party's vote).</p>` : "";
    const warn2 = m2 && m2.fit.separation ? `<p class="chisq-note">Model 2: some coefficients are extremely large/unstable, likely for the same separation reason.</p>` : "";
    fitWrap.innerHTML = `
      <h2>Model Fit Statistics</h2>
      <table class="data-table">
        <thead><tr><th></th><th>Model 1</th>${m2 ? "<th>Model 2</th>" : ""}</tr></thead>
        <tbody>
          ${rowsSpec.map(([label, f]) => `<tr><td>${label}</td><td>${f(m1)}</td>${m2 ? `<td>${f(m2)}</td>` : ""}</tr>`).join("")}
        </tbody>
      </table>
      ${warn1}${warn2}`;
  }

  function renderPredictions(dvLabel, focusVar, varyByColumn, m1, m2) {
    const changeVars = varyByColumn ? [varyByColumn, focusVar] : [focusVar];
    const focusLabel = labels[focusVar] || focusVar;
    const varyLabel = varyByColumn ? (labels[varyByColumn] || varyByColumn) : null;

    function predsFor(m, modelName) {
      const grid = averageCaseGrid({ rows: m.arows, columns: m.columns, changeVars, levelsLookup: levels });
      const preds = predictedProbabilities(m.fit, m.columns, grid, 2500);
      return preds.map((p) => ({
        x: p.row[focusVar],
        facet: varyByColumn ? p.row[varyByColumn] : null,
        model: modelName,
        median: p.median, lower: p.lower, upper: p.upper,
      }));
    }

    const plotData = [...predsFor(m1, "Model 1"), ...(m2 ? predsFor(m2, "Model 2") : [])];
    renderPredictedProbabilities(plotEl, plotData, { xLabel: focusLabel, yLabel: `Pr(${dvLabel})` });

    const separated = m1.fit.separation || (m2 && m2.fit.separation);
    const sepNote = separated
      ? `<p class="chisq-note">One of the estimated models shows signs of separation (see the fit-statistics warning above) &mdash; its predicted probabilities and CIs below may be unstable (near 0/1 with very wide or erratic intervals) rather than a reliable estimate.</p>`
      : "";

    const showFacet = !!varyByColumn;
    ppTableEl.innerHTML = `
      ${sepNote}
      <table class="data-table">
        <thead><tr>
          ${showFacet ? `<th>${varyLabel}</th>` : ""}
          <th>${focusLabel}</th><th>Model</th>
          <th>Predicted Probability</th><th>Lower 95% CI</th><th>Upper 95% CI</th>
        </tr></thead>
        <tbody>
          ${plotData.map((d) => `<tr>
            ${showFacet ? `<td>${d.facet}</td>` : ""}
            <td>${d.x}</td><td>${d.model}</td>
            <td>${fmt(d.median)}</td><td>${fmt(d.lower)}</td><td>${fmt(d.upper)}</td>
          </tr>`).join("")}
        </tbody>
      </table>`;
  }

  function estimate() {
    const dv = dvSel.value;
    const mainVars1 = selectedValues(mod1Sel);
    const useModel2 = cmActive.checked;
    const mainVars2 = useModel2 ? selectedValues(mod2Sel) : [];
    const varyBy = VARY_BY_OPTIONS.find((v) => v.value === varyBySel.value);
    const focusVar = focusVarSel.value || null;
    const varyByColumn = focusVar ? varyBy.column : null;

    if (!dv) { invalidateResults("Choose a dependent variable."); return; }
    if (mainVars1.length === 0) { invalidateResults("Choose at least one Model 1 variable."); return; }
    if (useModel2 && mainVars2.length === 0) { invalidateResults("Choose at least one Model 2 variable, or uncheck “Add Comparison Model.”"); return; }

    statusEl.classList.remove("error");
    statusEl.textContent = "Estimating…";
    resultsWrap.hidden = true;

    setTimeout(() => {
      try {
        const m1 = fitOne(dv, mainVars1, varyByColumn, focusVar);
        const m2 = useModel2 ? fitOne(dv, mainVars2, varyByColumn, focusVar) : null;
        const dvLabel = DV_VARS.find((d) => d.value === dv).label;

        renderCoefTable(dvLabel, m1, m2);
        renderFitTable(m1, m2);

        if (focusVar) {
          plotWrap.hidden = false;
          ppWrap.hidden = false;
          renderPredictions(dvLabel, focusVar, varyByColumn, m1, m2);
        } else {
          plotWrap.hidden = true;
          ppWrap.hidden = true;
        }

        resultsWrap.hidden = false;
        statusEl.textContent = `Estimated on n=${m1.fit.n.toLocaleString()} observations${m2 ? ` (Model 2: n=${m2.fit.n.toLocaleString()})` : ""}.`;
      } catch (err) {
        statusEl.textContent = `Could not estimate: ${err.message}`;
        statusEl.classList.add("error");
        console.error(err);
      }
    }, 0);
  }

  mod1Sel.addEventListener("change", () => { refreshFocusVarChoices(); invalidateResults("Model inputs changed — click “Refresh Model Output” to refresh results."); });
  mod2Sel.addEventListener("change", () => { refreshFocusVarChoices(); invalidateResults("Model inputs changed — click “Refresh Model Output” to refresh results."); });
  cmActive.addEventListener("change", () => {
    mod2Field.hidden = !cmActive.checked;
    refreshFocusVarChoices();
    invalidateResults("Model inputs changed — click “Refresh Model Output” to refresh results.");
  });
  dvSel.addEventListener("change", () => invalidateResults("Model inputs changed — click “Refresh Model Output” to refresh results."));
  varyBySel.addEventListener("change", () => invalidateResults("Model inputs changed — click “Refresh Model Output” to refresh results."));
  focusVarSel.addEventListener("change", () => invalidateResults("Model inputs changed — click “Refresh Model Output” to refresh results."));
  estimateBtn.addEventListener("click", estimate);

  refreshFocusVarChoices();
  invalidateResults("Choose a dependent variable and at least one Model 1 variable, then click “Refresh Model Output.”");

  return {
    setRows(newRows) {
      rows = newRows;
      invalidateResults("Sample subset changed — click “Refresh Model Output” to refresh results.");
    },
  };
}
