import { loadData } from "./data.js";
import { initDescriptives } from "./descriptives.js";
import { initCrosstabs } from "./crosstabs.js";
import { initModels } from "./models.js";
import { initSubset } from "./subset.js";

const status = document.getElementById("status");

function initTabs() {
  const buttons = document.querySelectorAll(".tab-btn");
  const panels = document.querySelectorAll(".tab-panel");
  buttons.forEach((btn) => {
    btn.addEventListener("click", () => {
      if (btn.disabled) return;
      buttons.forEach((b) => b.classList.toggle("active", b === btn));
      panels.forEach((p) => p.classList.toggle("active", p.id === `panel-${btn.dataset.tab}`));
    });
  });
}

async function main() {
  initTabs();
  try {
    const dataset = await loadData();
    status.textContent = `${dataset.meta.n.toLocaleString()} respondents loaded (${dataset.meta.source}, unweighted)`;
    const descriptivesApi = initDescriptives(dataset);
    const crosstabsApi = initCrosstabs(dataset);
    const modelsApi = initModels(dataset);
    initSubset(dataset, (subsetRows) => {
      descriptivesApi.setRows(subsetRows);
      crosstabsApi.setRows(subsetRows);
      modelsApi.setRows(subsetRows);
    });
  } catch (err) {
    status.textContent = `Failed to load data: ${err.message}`;
    status.classList.add("error");
    console.error(err);
  }
}

main();
