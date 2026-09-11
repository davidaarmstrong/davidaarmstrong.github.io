import { loadData } from "./data.js";
import { initDescriptives } from "./descriptives.js";
import { initCrosstabs } from "./crosstabs.js";
import { initModels } from "./models.js";
import { initSubset } from "./subset.js";

const status = document.getElementById("status");

// Mobile off-canvas sidebar. The toggle button opens whichever .sidebar
// belongs to the currently active tab (each tab-panel has its own, except
// Documentation, which has none) -- resolved dynamically rather than
// hardcoding a per-tab id list, so a future tab with a sidebar works for
// free. Wired up unconditionally; the CSS media query is what makes any of
// it visible, so this is harmless to leave active at any viewport width.
function initMobileSidebar() {
  const toggleBtn = document.getElementById("sidebar-toggle");
  const backdrop = document.getElementById("sidebar-backdrop");

  function activeSidebar() {
    return document.querySelector(".tab-panel.active .sidebar");
  }

  function openSidebar() {
    const sidebar = activeSidebar();
    if (!sidebar) return;
    sidebar.classList.add("open");
    backdrop.classList.add("open");
    document.body.classList.add("sidebar-open");
    toggleBtn.setAttribute("aria-expanded", "true");
  }

  function closeSidebar() {
    document.querySelectorAll(".sidebar.open").forEach((s) => s.classList.remove("open"));
    backdrop.classList.remove("open");
    document.body.classList.remove("sidebar-open");
    toggleBtn.setAttribute("aria-expanded", "false");
  }

  function updateToggleVisibility() {
    toggleBtn.hidden = !activeSidebar();
  }

  toggleBtn.addEventListener("click", openSidebar);
  backdrop.addEventListener("click", closeSidebar);
  document.querySelectorAll(".sidebar-close").forEach((btn) => btn.addEventListener("click", closeSidebar));
  document.addEventListener("keydown", (e) => { if (e.key === "Escape") closeSidebar(); });

  updateToggleVisibility();
  return { closeSidebar, updateToggleVisibility };
}

function initTabs(mobileSidebar) {
  const buttons = document.querySelectorAll(".tab-btn");
  const panels = document.querySelectorAll(".tab-panel");
  buttons.forEach((btn) => {
    btn.addEventListener("click", () => {
      if (btn.disabled) return;
      buttons.forEach((b) => b.classList.toggle("active", b === btn));
      panels.forEach((p) => p.classList.toggle("active", p.id === `panel-${btn.dataset.tab}`));
      mobileSidebar.closeSidebar();
      mobileSidebar.updateToggleVisibility();
    });
  });
}

async function main() {
  const mobileSidebar = initMobileSidebar();
  initTabs(mobileSidebar);
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
