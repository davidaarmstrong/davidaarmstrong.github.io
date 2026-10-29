(function () {
  "use strict";
  function initialize() {
    const widget = document.getElementById("activity-table-widget");
    if (!widget || widget.dataset.ready) return;
    widget.dataset.ready = "true";
    const table = widget.querySelector("table");
    const body = table.tBodies[0];
    const rows = Array.from(body.rows);
    const direction = widget.querySelector("#activity-direction");
    const index = widget.querySelector("#activity-index");
    const count = widget.querySelector("#activity-count");
    const empty = widget.querySelector("#activity-empty");
    const buttons = Array.from(widget.querySelectorAll(".activity-sort"));
    let sortKey = null;
    let ascending = true;
    function update() {
      const ordered = rows.slice().sort(function (a, b) {
        let result = 0;
        if (sortKey === "action") {
          result = a.querySelector(".activity-action").textContent.localeCompare(b.querySelector(".activity-action").textContent, "en", { sensitivity: "base" });
        } else if (sortKey === "percent") {
          result = Number(a.dataset.percent) - Number(b.dataset.percent);
        }
        return (ascending ? result : -result) || Number(a.dataset.order) - Number(b.dataset.order);
      });
      let shown = 0;
      ordered.forEach(function (row) {
        row.hidden = Boolean((direction.value && row.dataset.direction !== direction.value) || (index.value && row.dataset.index !== index.value));
        row.classList.toggle("activity-even", !row.hidden && shown % 2 === 1);
        if (!row.hidden) shown++;
        body.appendChild(row);
      });
      buttons.forEach(function (button) {
        const active = button.dataset.sort === sortKey;
        button.parentElement.setAttribute("aria-sort", active ? (ascending ? "ascending" : "descending") : "none");
        button.querySelector("span").textContent = active ? (ascending ? "↑" : "↓") : "↕";
        button.setAttribute("aria-label", "Sort by " + button.dataset.sort + (active ? (ascending ? ", currently ascending" : ", currently descending") : ""));
      });
      count.textContent = "Showing " + shown + " of " + rows.length + " activities";
      empty.hidden = shown !== 0;
    }
    direction.addEventListener("change", update);
    index.addEventListener("change", update);
    buttons.forEach(function (button) {
      button.addEventListener("click", function () {
        const key = button.dataset.sort;
        ascending = key === sortKey ? !ascending : key === "action";
        sortKey = key;
        update();
      });
    });
    widget.querySelector("#activity-reset").addEventListener("click", function () {
      direction.value = "";
      index.value = "";
      sortKey = null;
      ascending = true;
      update();
    });
    update();
  }
  function ready() {
    initialize();
    if (typeof window.require === "function") {
      window.require(["gitbook"], function (gitbook) {
        gitbook.events.bind("page.change", initialize);
      });
    }
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", ready);
  else ready();
})();
