// Global sample-subset control (None / Year / Region), shared across tabs.
// Every tab operates on whatever the current subset is -- see app.js, which
// re-renders both tabs' modules through their setRows() on every change.

export function initSubset({ rows, levels }, onChange) {
  const bySel = document.getElementById("subset-by");
  const valField = document.getElementById("subset-value-field");
  const valSel = document.getElementById("subset-value");
  const info = document.getElementById("subset-info");

  const years = [...new Set(rows.map((r) => r.year).filter((v) => v != null))].sort((a, b) => a - b);
  const regions = levels.province || [];

  function populateValues() {
    const by = bySel.value;
    valSel.innerHTML = "";
    const opts = by === "year" ? years.map(String) : by === "province" ? regions : [];
    for (const v of opts) {
      const opt = document.createElement("option");
      opt.value = v;
      opt.textContent = v;
      valSel.append(opt);
    }
    valField.hidden = !by;
  }

  function apply() {
    const by = bySel.value;
    let subset = rows;
    if (by === "year") {
      const y = Number(valSel.value);
      subset = rows.filter((r) => r.year === y);
    } else if (by === "province") {
      subset = rows.filter((r) => r.province === valSel.value);
    }
    info.textContent = `N = ${subset.length.toLocaleString()} respondents in this subset`;
    onChange(subset);
  }

  bySel.addEventListener("change", () => {
    populateValues();
    apply();
  });
  valSel.addEventListener("change", apply);

  populateValues();
  apply();
}
