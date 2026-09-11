import { NUM_VARS, STRAT_VARS, levelsFor } from "./data.js";
import { summarise, pairwiseComparisons } from "./stats.js";
import { renderHistogram, renderGroupMeans } from "./charts.js";

function fmt(x, digits = 3) {
  return x == null || Number.isNaN(x) ? "" : x.toFixed(digits);
}

export function initDescriptives({ rows: initialRows, levels, labels }) {
  let rows = initialRows;
  const desvarSel = document.getElementById("desvar");
  const stratdesSel = document.getElementById("stratdes");
  const nbinsField = document.getElementById("nbins-field");
  const nbinsInput = document.getElementById("nbins");
  const hint = document.getElementById("des-hint");
  const tableWrap = document.getElementById("des-table-wrap");
  const chartWrap = document.getElementById("des-chart-wrap");
  const facWrap = document.getElementById("des-fac-wrap");
  const chartEl = document.getElementById("chart");
  const pairwiseEl = document.getElementById("pairwise-table");

  for (const { value, label } of NUM_VARS) {
    const opt = document.createElement("option");
    opt.value = value;
    opt.textContent = label;
    desvarSel.append(opt);
  }
  for (const { value, label } of STRAT_VARS) {
    const opt = document.createElement("option");
    opt.value = value;
    opt.textContent = label;
    stratdesSel.append(opt);
  }

  function render() {
    const desvar = desvarSel.value;
    const stratvar = stratdesSel.value;

    nbinsField.hidden = !(desvar && !stratvar);
    hint.hidden = !!desvar;
    tableWrap.hidden = !desvar;
    chartWrap.hidden = !desvar;
    facWrap.hidden = true;

    if (!desvar) return;

    const groupVar = stratvar || null;
    const groupLevels = groupVar ? levelsFor(levels, groupVar, rows) : null;
    const summary = summarise(rows, desvar, groupVar, groupLevels);

    const desLabel = labels[desvar] || desvar;
    const showGroupCol = !!groupVar;
    tableWrap.innerHTML = `
      <table class="data-table">
        <thead><tr>
          ${showGroupCol ? "<th>Group</th>" : ""}
          <th>Mean</th><th>SD</th><th>IQR</th>
          <th>0%</th><th>25%</th><th>50%</th><th>75%</th><th>100%</th>
          <th>N</th><th>NA</th>
        </tr></thead>
        <tbody>
          ${summary.map((r) => `
            <tr>
              ${showGroupCol ? `<td>${r.group}</td>` : ""}
              <td>${fmt(r.mean)}</td><td>${fmt(r.sd)}</td><td>${fmt(r.iqr)}</td>
              <td>${fmt(r.q0)}</td><td>${fmt(r.q25)}</td><td>${fmt(r.q50)}</td><td>${fmt(r.q75)}</td><td>${fmt(r.q100)}</td>
              <td>${r.n}</td><td>${r.na}</td>
            </tr>`).join("")}
        </tbody>
      </table>`;

    if (groupVar) {
      renderGroupMeans(chartEl, summary, `Average ${desLabel}`);
      if (summary.length >= 2) {
        facWrap.hidden = false;
        const comparisons = pairwiseComparisons(summary);
        pairwiseEl.innerHTML = `
          <table class="data-table">
            <thead><tr>
              <th>Group A</th><th>Group B</th><th>Difference</th>
              <th>Analytical p</th><th>Bonferroni p</th>
            </tr></thead>
            <tbody>
              ${comparisons.map((c) => `
                <tr>
                  <td>${c.groupA}</td><td>${c.groupB}</td>
                  <td>${fmt(c.diff)}</td>
                  <td>${fmt(c.pRaw)}</td>
                  <td>${fmt(c.pBonferroni)}${c.pBonferroni < 0.05 ? "*" : ""}</td>
                </tr>`).join("")}
            </tbody>
          </table>
          <p class="legend-note">Difference = Group A mean &minus; Group B mean. Analytical p is the unadjusted two-tailed p-value for that pair; Bonferroni p multiplies it by the number of comparisons (${comparisons.length}) to control the error rate across all of them. * Bonferroni p &lt; .05.</p>`;
      }
    } else {
      const nBins = Number(nbinsInput.value) || 10;
      renderHistogram(chartEl, rows.map((r) => r[desvar]), nBins, desLabel);
    }
  }

  desvarSel.addEventListener("change", render);
  stratdesSel.addEventListener("change", render);
  nbinsInput.addEventListener("input", render);
  render();

  return {
    setRows(newRows) {
      rows = newRows;
      render();
    },
  };
}
