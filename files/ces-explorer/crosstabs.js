import { STRAT_VARS, ROW_VARS, levelsFor } from "./data.js";
import { freqTable, contingency, chiSquareTest } from "./stats.js";
import { renderMosaic, renderFreqBar } from "./charts.js";

function fmt(x, digits = 1) {
  return x == null || Number.isNaN(x) ? "" : x.toFixed(digits);
}

export function initCrosstabs({ rows: initialRows, levels, labels }) {
  let rows = initialRows;
  const rowSel = document.getElementById("rowvar");
  const colSel = document.getElementById("colvar");
  const hint = document.getElementById("xtab-hint");
  const tableWrap = document.getElementById("xtab-table-wrap");
  const chisqWrap = document.getElementById("xtab-chisq");
  const chartWrap = document.getElementById("xtab-chart-wrap");
  const chartTitle = document.getElementById("xtab-chart-title");
  const chartEl = document.getElementById("xtab-chart");

  for (const { value, label } of ROW_VARS) {
    const opt = document.createElement("option");
    opt.value = value;
    opt.textContent = label;
    rowSel.append(opt);
  }
  for (const { value, label } of STRAT_VARS) {
    const opt = document.createElement("option");
    opt.value = value;
    opt.textContent = label;
    colSel.append(opt);
  }

  function render() {
    const rowVar = rowSel.value;
    const colVar = colSel.value;

    hint.hidden = !!rowVar;
    tableWrap.hidden = !rowVar;
    chisqWrap.hidden = !(rowVar && colVar);
    chartWrap.hidden = !rowVar;

    if (!rowVar) return;

    const rowLabel = labels[rowVar] || rowVar;

    if (!colVar) {
      const rowLevels = levelsFor(levels, rowVar, rows);
      const ft = freqTable(rows, rowVar, rowLevels);
      tableWrap.innerHTML = `
        <table class="data-table">
          <thead><tr><th>${rowLabel}</th><th>N</th><th>%</th></tr></thead>
          <tbody>
            ${ft.rows.map((r) => `<tr><td>${r.group}</td><td>${r.n}</td><td>${fmt(r.pct)}%</td></tr>`).join("")}
          </tbody>
          <tfoot><tr><td>Total</td><td>${ft.total}</td><td>100.0%</td></tr></tfoot>
        </table>`;
      chartTitle.textContent = "Bar Chart";
      renderFreqBar(chartEl, ft.rows, rowLabel);
      return;
    }

    const colLabel = labels[colVar] || colVar;
    const rowLevels = levelsFor(levels, rowVar, rows);
    const colLevels = levelsFor(levels, colVar, rows);
    const ct = contingency(rows, rowVar, colVar, rowLevels, colLevels);
    const colTotals = ct.colLevels.map((_, j) => ct.matrix.reduce((a, row) => a + row[j], 0));
    const rowTotals = ct.matrix.map((row) => row.reduce((a, b) => a + b, 0));
    const grandTotal = rowTotals.reduce((a, b) => a + b, 0);

    tableWrap.innerHTML = `
      <table class="data-table">
        <thead>
          <tr><th></th><th colspan="${ct.colLevels.length}" style="text-align:center">${colLabel}</th><th></th></tr>
          <tr><th>${rowLabel}</th>${ct.colLevels.map((c) => `<th>${c}</th>`).join("")}<th>Total</th></tr>
        </thead>
        <tbody>
          ${ct.matrix.map((row, i) => `
            <tr>
              <td>${ct.rowLevels[i]}</td>
              ${row.map((n, j) => `<td>${n}<br><span style="color:var(--text-muted);font-size:11px">${fmt((100 * n) / colTotals[j])}%</span></td>`).join("")}
              <td>${rowTotals[i]}</td>
            </tr>`).join("")}
        </tbody>
        <tfoot>
          <tr>
            <td>Total</td>
            ${colTotals.map((n) => `<td>${n}</td>`).join("")}
            <td>${grandTotal}</td>
          </tr>
        </tfoot>
      </table>`;

    const chi2 = chiSquareTest(ct.matrix);
    const explainer = chi2.pValue < 0.05
      ? `A p-value below 0.05 means the pattern of association we observe is systematically different from what we'd expect if ${rowLabel} and ${colLabel} were unrelated in the population.`
      : `A p-value of 0.05 or higher means the pattern of association we observe is consistent with a population where ${rowLabel} and ${colLabel} are unrelated.`;
    chisqWrap.innerHTML = `
      <p class="chisq-note">
        The Pearson Chi-squared statistic is
        &chi;&sup2; = ${chi2.statistic.toFixed(2)} with ${chi2.df} degrees of freedom.<br>
        The p-value is approximately ${chi2.pValue < 0.001 ? "< 0.001" : chi2.pValue.toFixed(3)}.
      </p>
      <p class="chisq-note">${explainer}</p>`;

    chartTitle.textContent = "Mosaic Plot";
    renderMosaic(chartEl, ct, rowLabel, colLabel);
  }

  rowSel.addEventListener("change", render);
  colSel.addEventListener("change", render);
  render();

  return {
    setRows(newRows) {
      rows = newRows;
      render();
    },
  };
}
