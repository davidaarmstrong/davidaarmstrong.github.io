import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7.9.0/+esm";
import * as Plot from "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6.17/+esm";
import { histogram } from "./stats.js";

function clear(container) {
  container.innerHTML = "";
}

const catColor = (i) => `var(--cat-${(i % 13) + 1})`;

export function renderHistogram(container, values, nBins, xLabel) {
  clear(container);
  const h = histogram(values, nBins);
  const data = h.mids.map((mid, i) => ({ mid, prop: h.counts[i] / h.n }));
  const plot = Plot.plot({
    width: 420,
    height: 320,
    marginLeft: 55,
    x: { label: xLabel },
    y: { label: "Proportion", grid: true, percent: false },
    marks: [
      Plot.rectY(data, {
        x: (d) => d.mid - h.width / 2,
        x2: (d) => d.mid + h.width / 2,
        y: "prop",
        fill: "var(--accent)",
      }),
      Plot.ruleY([0]),
    ],
  });
  container.append(plot);
}

// ft.rows: [{group, n, pct}] from stats.freqTable(), in display order.
// Horizontal bars -- category labels run along the y-axis, so there's no
// risk of them overplotting each other regardless of label length or
// category count.
export function renderFreqBar(container, ftRows, yLabel) {
  clear(container);
  const data = ftRows.map((r) => ({ group: String(r.group), pct: r.pct }));
  const longestLabel = Math.max(...data.map((d) => d.group.length));
  const marginLeft = Math.min(220, Math.max(70, longestLabel * 6.5));
  const plot = Plot.plot({
    width: 480,
    height: Math.max(220, data.length * 34 + 40),
    marginLeft,
    marginRight: 20,
    x: { label: "Percent", grid: true },
    y: { label: yLabel, domain: data.map((d) => d.group) },
    marks: [
      Plot.barX(data, { y: "group", x: "pct", fill: "var(--accent)" }),
      Plot.ruleX([0]),
    ],
  });
  container.append(plot);
}

// summaryRows: [{group, mean, sd, n, ...}]. Draws mean +/- 95% CI by group.
export function renderGroupMeans(container, summaryRows, yLabel) {
  clear(container);
  const data = summaryRows.map((r) => {
    const err = 1.96 * (r.sd / Math.sqrt(r.n));
    return { group: String(r.group), mean: r.mean, lower: r.mean - err, upper: r.mean + err };
  });
  const plot = Plot.plot({
    width: 420,
    height: 320,
    marginLeft: 55,
    marginBottom: 60,
    x: { label: null, domain: data.map((d) => d.group), tickRotate: data.length > 6 ? -35 : 0 },
    y: { label: yLabel, grid: true },
    marks: [
      Plot.ruleX(data, { x: "group", y1: "lower", y2: "upper", stroke: "var(--accent)", strokeWidth: 1.5 }),
      Plot.dot(data, { x: "group", y: "mean", fill: "var(--accent)", r: 4 }),
    ],
  });
  container.append(plot);
}

// Average-case predicted probability plot. data: array of
// {x, facet, model, median, lower, upper}. `facet` is null when there's no
// "vary effect by" grouping. Two models get a small pixel offset (dx) so
// their point+CI pairs at the same x don't overlap.
export function renderPredictedProbabilities(container, data, { xLabel, yLabel }) {
  clear(container);
  const models = [...new Set(data.map((d) => d.model))];
  const hasFacet = data.some((d) => d.facet != null);
  const xDomain = [...new Set(data.map((d) => String(d.x)))];
  const colorRange = { "Model 1": "var(--accent)", "Model 2": "var(--accent-2)" };
  const dxFor = (i) => (models.length > 1 ? (i === 0 ? -6 : 6) : 0);

  const marks = [];
  models.forEach((m, i) => {
    const sub = data.filter((d) => d.model === m).map((d) => ({ ...d, x: String(d.x) }));
    marks.push(
      Plot.ruleY(sub, { x: "x", y1: "lower", y2: "upper", stroke: "model", strokeWidth: 1.5, dx: dxFor(i), fx: hasFacet ? "facet" : undefined }),
      Plot.dot(sub, { x: "x", y: "median", fill: "model", r: 4, dx: dxFor(i), fx: hasFacet ? "facet" : undefined }),
    );
  });

  const plot = Plot.plot({
    width: hasFacet ? Math.max(560, [...new Set(data.map((d) => d.facet))].length * 160) : 480,
    height: 340,
    marginLeft: 55,
    marginBottom: 55,
    x: { label: xLabel, domain: xDomain, tickRotate: xDomain.length > 5 ? -30 : 0 },
    y: { label: yLabel, domain: [0, 1], grid: true, percent: false },
    fx: hasFacet ? { label: null } : undefined,
    color: { domain: models, range: models.map((m) => colorRange[m]), legend: models.length > 1 },
    marks,
  });
  container.append(plot);
}

// Mosaic plot from a contingency() result: column widths proportional to
// column totals, row segments within each column proportional to that
// column's row shares (i.e. matches the table's column percentages).
export function renderMosaic(container, ct, rowLabel, colLabel) {
  clear(container);
  const { rowLevels, colLevels, matrix } = ct;
  const colTotals = colLevels.map((_, j) => matrix.reduce((a, row) => a + row[j], 0));
  const grandTotal = colTotals.reduce((a, b) => a + b, 0);

  const width = 560;
  const height = 480;
  const margin = { top: 10, right: 140, bottom: 70, left: 10 };
  const plotW = width - margin.left - margin.right;
  const plotH = height - margin.top - margin.bottom;

  const svg = d3.create("svg").attr("viewBox", [0, 0, width, height]).attr("width", width).attr("height", height);
  const g = svg.append("g").attr("transform", `translate(${margin.left},${margin.top})`);

  let xCursor = 0;
  const colorScale = d3.scaleOrdinal().domain(rowLevels).range(rowLevels.map((_, i) => catColor(i)));

  colLevels.forEach((col, j) => {
    const colW = (colTotals[j] / grandTotal) * plotW;
    let yCursor = 0;
    rowLevels.forEach((row, i) => {
      const n = matrix[i][j];
      const share = colTotals[j] > 0 ? n / colTotals[j] : 0;
      const segH = share * plotH;
      if (segH > 0) {
        g.append("rect")
          .attr("x", xCursor)
          .attr("y", yCursor)
          .attr("width", Math.max(colW - 1, 0))
          .attr("height", Math.max(segH - 1, 0))
          .attr("fill", colorScale(row))
          .append("title")
          .text(`${row} × ${col}: ${n} (${(share * 100).toFixed(1)}% of ${col})`);
      }
      yCursor += segH;
    });
    g.append("text")
      .attr("x", xCursor + colW / 2)
      .attr("y", plotH + 16)
      .attr("text-anchor", "end")
      .attr("transform", `rotate(-35 ${xCursor + colW / 2} ${plotH + 16})`)
      .attr("font-size", 11)
      .attr("fill", "var(--text-secondary)")
      .text(col);
    xCursor += colW;
  });

  g.append("text")
    .attr("x", plotW / 2)
    .attr("y", plotH + 55)
    .attr("text-anchor", "middle")
    .attr("font-size", 12)
    .attr("fill", "var(--text-secondary)")
    .text(colLabel);

  const legend = svg.append("g").attr("transform", `translate(${width - margin.right + 12},${margin.top})`);
  rowLevels.forEach((row, i) => {
    const ly = i * 18;
    legend.append("rect").attr("x", 0).attr("y", ly).attr("width", 11).attr("height", 11).attr("fill", colorScale(row));
    legend.append("text").attr("x", 16).attr("y", ly + 9.5).attr("font-size", 11).attr("fill", "var(--text-secondary)").text(row);
  });
  legend.append("text").attr("x", 0).attr("y", -8).attr("font-size", 11).attr("font-weight", 600).attr("fill", "var(--text-secondary)").text(rowLabel);

  container.append(svg.node());
}
