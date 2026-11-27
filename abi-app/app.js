/* Anti-Black Index map: data in data/abi.json (R/28_app_data.R) and
   data/us.topo.json (2010 counties, pre-projected Albers with Alaska and Hawaii insets). */
(function () {
  "use strict";

  const W = 960, H = 600;               // map viewBox
  const NCLASS = 7;
  const DIMS = { h: "Hostility", t: "Treatment" };
  const LEVELS = { county: "county", state: "state" };
  const st = { level: "county", dim: "h", t: 11, within: false, sel: null };

  let D, NT, cIdx = new Map(), sIdx = new Map(), cState = [];
  let cFeat = [], sFeat = [], mesh;
  let svg, gRoot, gC, gS, meshPath, hoverPath, selHalo, selPath, zoom, pathGen;
  let gLab, countyMesh, places, cityXY = [], countyXY = [], stateXY = [], curT = d3.zoomIdentity, labelFrame = 0;
  let scale, playTimer = null;
  const pooled = {};

  const fmt = d3.format(".1f"), fmt1 = d3.format(".1f");
  const tip = document.getElementById("tooltip");
  const css = (v) => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
  const isDark = () => {
    const a = document.documentElement.getAttribute("data-theme");
    return a ? a === "dark" : matchMedia("(prefers-color-scheme: dark)").matches;
  };

  // ---------- data helpers ----------
  const L = (level = st.level) => (level === "county" ? D.counties : D.states);
  const nUnits = (level = st.level) => L(level).name.length;
  const val = (key, i, t, level = st.level) => L(level)[key][i * NT + t];
  const unitName = (i, level = st.level) =>
    level === "county" ? `${D.counties.name[i]}, ${D.counties.state[i]}` : D.states.name[i];
  const decade = (t = st.t) => D.decades[t];

  // University of Michigan palette: hostility runs pale maize -> Maize ->
  // Ross Orange -> dark orange; treatment pale blue -> Arboretum Blue ->
  // Michigan Blue. Dark mode runs from the surface up to Maize / light blue.
  const RAMPS = {
    light: { h: ["#fff4cc", "#FFCB05", "#D86018", "#6b2a06"], t: ["#e3ebf5", "#8fb0d9", "#2F65A7", "#00274C"] },
    dark:  { h: ["#2a2f3a", "#8a5a12", "#D86018", "#FFCB05"], t: ["#1d2a3c", "#2F65A7", "#7ea4d6", "#d6e3f3"] },
  };
  function ramp(dim) {
    const stops = RAMPS[isDark() ? "dark" : "light"][dim];
    return d3.quantize(d3.piecewise(d3.interpolateLab, stops), NCLASS);
  }

  function buildScale() {
    const arr = L()[st.dim];
    let vals;
    if (st.within) {
      vals = [];
      for (let i = 0, n = nUnits(); i < n; i++) { const v = arr[i * NT + st.t]; if (v != null) vals.push(v); }
    } else {
      const k = st.level + st.dim;
      if (!pooled[k]) pooled[k] = arr.filter((v) => v != null);
      vals = pooled[k];
    }
    scale = d3.scaleQuantile().domain(vals).range(ramp(st.dim));
  }

  // ---------- map ----------
  function drawMap(topo, placeData) {
    const host = document.getElementById("map");
    host.innerHTML = "";
    svg = d3.select(host).append("svg").attr("viewBox", `0 0 ${W} ${H}`).attr("role", "img")
      .attr("aria-label", "Map of the United States");
    const counties = topojson.feature(topo, topo.objects.counties);
    const states = topojson.feature(topo, topo.objects.states);
    mesh = topojson.mesh(topo, topo.objects.states, (a, b) => a !== b);
    pathGen = d3.geoPath(d3.geoIdentity().reflectY(true).fitExtent([[6, 6], [W - 6, H - 6]], counties));

    cFeat = new Array(nUnits("county"));
    counties.features.forEach((f) => { const i = cIdx.get(f.properties.fips); if (i != null) cFeat[i] = f; });
    sFeat = new Array(nUnits("state"));
    states.features.forEach((f) => { const i = sIdx.get(f.properties.state); if (i != null) sFeat[i] = f; });

    // diagonal hatching for units without an estimate
    svg.append("defs").append("pattern").attr("id", "hatch").attr("patternUnits", "userSpaceOnUse")
      .attr("width", 4).attr("height", 4).attr("patternTransform", "rotate(45)")
      .call((p) => { p.append("rect").attr("class", "hatch-bg").attr("width", 4).attr("height", 4);
                     p.append("line").attr("class", "hatch-line").attr("x1", 0).attr("y1", 0).attr("x2", 0).attr("y2", 4); });
    gRoot = svg.append("g");
    // base: land under the semi-transparent fills
    gRoot.append("g").attr("class", "base").selectAll("path").data(states.features).join("path")
      .attr("class", "land").attr("d", pathGen);
    gC = gRoot.append("g").attr("class", "counties-level");
    gS = gRoot.append("g").attr("class", "states-level");
    const mk = (g, feats, level) => g.selectAll("path").data(d3.range(feats.length).filter((i) => feats[i]))
      .join("path").attr("class", "unit").attr("d", (i) => pathGen(feats[i]))
      .on("mousemove", (e, i) => { showTip(e, i, level); hoverPath.attr("d", pathGen(feats[i])); })
      .on("mouseleave", () => { hideTip(); hoverPath.attr("d", null); })
      .on("click", (e, i) => select(i));
    mk(gC, cFeat, "county");
    mk(gS, sFeat, "state");
    countyMesh = gRoot.append("path").attr("class", "county-mesh")
      .attr("d", pathGen(topojson.mesh(topo, topo.objects.counties, (a, b) => a !== b)));
    meshPath = gRoot.append("path").attr("class", "state-mesh").attr("d", pathGen(mesh));
    hoverPath = gRoot.append("path").attr("class", "hover-outline");
    selHalo = gRoot.append("path").attr("class", "sel-halo");
    selPath = gRoot.append("path").attr("class", "sel-outline");

    zoom = d3.zoom().scaleExtent([1, 14]).translateExtent([[0, 0], [W, H]])
      .on("start", clearHover)
      .on("zoom", (e) => { gRoot.attr("transform", e.transform); curT = e.transform; showCountyMesh(); scheduleLabels(); });
    svg.call(zoom);

    // reference labels (cities, states, counties) drawn above the fills, in
    // screen space so their size stays constant while zooming
    gLab = svg.append("g").attr("class", "labels").attr("aria-hidden", "true");
    places = placeData;
    const proj = pathGen.projection();
    cityXY = places.x.map((x, i) => proj([x, places.y[i]]));
    countyXY = cFeat.map((f) => (f ? pathGen.centroid(f) : null));
    stateXY = sFeat.map((f) => (f ? pathGen.centroid(f) : null));
    document.getElementById("reset").onclick = () => svg.transition().duration(400).call(zoom.transform, d3.zoomIdentity);
  }

  function recolor() {
    buildScale();
    const g = st.level === "county" ? gC : gS;
    g.selectAll("path").each(function (i) {
      const v = val(st.dim, i, st.t);
      this.setAttribute("fill", v == null ? "" : scale(v));
      this.classList.toggle("missing", v == null);
    });
    drawLegend();
  }

  // ---------- basemap labels ----------
  function showCountyMesh() {
    countyMesh.style("display", st.level === "state" && curT.k >= 3 ? null : "none");
  }
  function scheduleLabels() {
    if (labelFrame) return;
    labelFrame = requestAnimationFrame(() => { labelFrame = 0; placeLabels(); });
  }
  // cities by population as you zoom in, state names at middle zooms, county
  // names when close; each label is kept only if it doesn't overlap one
  // already placed (largest cities first)
  function placeLabels() {
    const k = curT.k, rect = svg.node().getBoundingClientRect();
    if (!rect.width) return;
    const u = W / rect.width;                       // viewBox units per screen pixel
    const boxes = [], out = [];
    const free = (b) => !boxes.some((o) => b[0] < o[2] && b[2] > o[0] && b[1] < o[3] && b[3] > o[1]);
    const inView = (p) => p[0] >= 0 && p[0] <= W && p[1] >= 0 && p[1] <= H;
    const add = (cls, text, p, fs, dot) => {
      const w = text.length * fs * 0.56, pad = 2 * u;
      const x = dot ? p[0] + 4 * u : p[0] - w / 2, y = dot ? p[1] + fs * 0.35 : p[1] + fs * 0.35;
      const b = [x - pad - (dot ? 4 * u : 0), y - fs - pad, x + w + pad, y + pad];
      if (!free(b)) return false;
      boxes.push(b); out.push({ cls, text, x, y, fs, dot: dot ? p : null });
      return true;
    };
    const thr = k < 1.4 ? Infinity : k < 2.2 ? 600000 : k < 3.5 ? 200000 : k < 5.5 ? 75000 : k < 8.5 ? 30000 : 10000;
    for (let i = 0; i < cityXY.length && places.pop[i] >= thr; i++) {
      const p = curT.apply(cityXY[i]);
      if (inView(p)) add("city", places.name[i], p, (places.pop[i] >= 1e6 ? 12 : 11) * u, true);
    }
    if (k >= 1.6 && k < 6)
      stateXY.forEach((c, i) => { if (c) { const p = curT.apply(c); if (inView(p)) add("state-label", D.states.name[i].toUpperCase(), p, 10 * u, false); } });
    if (st.level === "county" && k >= 5)
      countyXY.forEach((c, i) => { if (c) { const p = curT.apply(c); if (inView(p)) add("county-label", D.counties.name[i], p, 9.5 * u, false); } });

    gLab.selectAll("g").data(out).join("g").attr("class", (d) => d.cls).each(function (d) {
      const g = d3.select(this);
      g.selectAll("*").remove();
      if (d.dot) g.append("circle").attr("cx", d.dot[0]).attr("cy", d.dot[1]).attr("r", 2.2 * u).attr("stroke-width", u);
      g.append("text").attr("x", d.x).attr("y", d.y).attr("font-size", d.fs).attr("stroke-width", 3 * u).text(d.text);
    });
  }

  function showLevel() {
    gC.style("display", st.level === "county" ? null : "none");
    gS.style("display", st.level === "state" ? null : "none");
    meshPath.style("display", st.level === "county" ? null : "none");
    showCountyMesh(); scheduleLabels();
    document.getElementById("map-hint").textContent =
      `Hover for values; click a ${st.level} for its trajectory. Scroll or pinch to zoom.`;
  }

  function drawLegend() {
    const el = document.getElementById("legend");
    const cols = scale.range(), q = scale.quantiles();
    const scope = st.within ? `quantiles within ${decade()}` : "quantiles over all decades";
    el.innerHTML = `<div class="legend-title">${DIMS[st.dim]}, ${decade()} <span class="muted" style="font-weight:400">(0–100 scale; ${scope}; higher = more anti-Black)</span></div>`;
    const row = document.createElement("div");
    row.style.display = "flex"; row.style.flexWrap = "wrap"; row.style.alignItems = "flex-start"; row.style.rowGap = "4px";
    const r = document.createElement("div"); r.className = "ramp";
    cols.forEach((c, k) => {
      const cell = document.createElement("div"); cell.className = "cell";
      cell.innerHTML = `<div class="sw" style="background:${c}"></div><div class="lab">${k < q.length ? fmt1(q[k]) : ""}</div>`;
      r.appendChild(cell);
    });
    row.appendChild(r);
    const na = document.createElement("span"); na.className = "na";
    na.innerHTML = `<span class="sw"></span>no estimate`;
    row.appendChild(na);
    el.appendChild(row);
  }

  // ---------- tooltip ----------
  function placeTip(e) {
    const pad = 14, r = tip.getBoundingClientRect();
    let x = e.clientX + pad, y = e.clientY + pad;
    if (x + r.width > innerWidth - 4) x = e.clientX - r.width - pad;
    if (y + r.height > innerHeight - 4) y = e.clientY - r.height - pad;
    tip.style.left = `${Math.max(4, x)}px`; tip.style.top = `${Math.max(4, y)}px`;
  }
  function tipHTML(title, rows) {
    return `<div class="tt-title">${title}</div>` +
      rows.map(([k, v]) => `<div class="tt-row"><span>${k}</span><b>${v}</b></div>`).join("");
  }
  const interval = (key, i, t, level) => {
    const v = val(key, i, t, level);
    return v == null ? "no estimate" : `${fmt(v)} [${fmt(val(key + "_lo", i, t, level))}, ${fmt(val(key + "_hi", i, t, level))}]`;
  };
  function showTip(e, i, level) {
    const pb = val("pb", i, st.t, level);
    const rows = [[DIMS[st.dim], interval(st.dim, i, st.t, level)], ["Percent Black", pb == null ? "no data" : `${fmt1(pb)}%`]];
    if (level === "county") rows.push(["County observations", val("n", i, st.t, level) ?? 0]);
    tip.innerHTML = tipHTML(`${unitName(i, level)} &middot; ${decade()}`, rows);
    tip.hidden = false; placeTip(e);
  }
  function hideTip() { tip.hidden = true; }
  // On touch screens a tap fires mousemove but never mouseleave, so the
  // tooltip and hover outline would stay up: clear them on scroll, on zoom
  // or pan, and on a tap anywhere that isn't a map unit or chart.
  function clearHover() { hideTip(); if (hoverPath) hoverPath.attr("d", null); }

  // ---------- selection ----------
  function select(i) {
    st.sel = i;
    const f = (st.level === "county" ? cFeat : sFeat)[i];
    selPath.attr("d", f ? pathGen(f) : null);
    selHalo.attr("d", f ? pathGen(f) : null);
    renderSide(); writeHash();
  }

  function renderSide() {
    const lvl = st.level, i = st.sel, t = st.t;
    const nameEl = document.getElementById("sel-name"), subEl = document.getElementById("sel-sub");
    if (i == null) {
      nameEl.textContent = `No ${lvl} selected`;
      subEl.innerHTML = `Click a ${lvl} on the map to see its trajectory on both dimensions.`;
    } else {
      nameEl.textContent = unitName(i);
      const pb = val("pb", i, t);
      let html = lvl === "county" ? `County in ${D.states.name[cState[i]]}` : "State";
      html += `<div class="vals"><span>Hostility ${decade()}: <b>${val("h", i, t) == null ? "–" : fmt(val("h", i, t))}</b></span>` +
        `<span>Treatment: <b>${val("t", i, t) == null ? "–" : fmt(val("t", i, t))}</b></span>` +
        `<span>Percent Black: <b>${pb == null ? "–" : fmt1(pb) + "%"}</b></span>` +
        (lvl === "county" ? `<span>County observations: <b>${val("n", i, t) ?? 0}</b></span>` : "") + `</div>`;
      subEl.innerHTML = html;
    }
    drawTraj("h"); drawTraj("t"); drawTrajLegend(); drawTable(); drawHist();
  }

  // ---------- trajectory charts ----------
  function drawTraj(dim) {
    const host = document.getElementById(`traj-${dim}`);
    host.innerHTML = "";
    const w = Math.max(260, host.clientWidth), h = 150, m = { l: 40, r: 10, t: 8, b: 22 };
    const svgT = d3.select(host).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("height", h);
    const x = d3.scaleLinear().domain([D.decades[0], D.decades[NT - 1]]).range([m.l, w - m.r]);
    const i = st.sel, lvl = st.level, col = css(dim === "h" ? "--c-h" : "--c-t");
    if (i == null) {
      svgT.append("text").attr("class", "empty").attr("x", w / 2).attr("y", h / 2).attr("text-anchor", "middle")
        .text(`Select a ${lvl} on the map`);
      return;
    }
    const pts = D.decades.map((d, t) => ({
      d, t, v: val(dim, i, t), lo: val(dim + "_lo", i, t), hi: val(dim + "_hi", i, t),
      n: lvl === "county" ? (val("n", i, t) ?? 0) : null,
      s: lvl === "county" ? val(dim, cState[i], t, "state") : null,
    }));
    const ext = d3.extent(pts.flatMap((p) => [p.lo, p.hi, p.s]).filter((v) => v != null));
    if (ext[0] == null) {
      svgT.append("text").attr("class", "empty").attr("x", w / 2).attr("y", h / 2).attr("text-anchor", "middle").text("No estimates");
      return;
    }
    const y = d3.scaleLinear().domain(ext).nice(4).range([h - m.b, m.t]);
    svgT.append("g").attr("class", "grid").attr("transform", `translate(${m.l},0)`)
      .call(d3.axisLeft(y).ticks(4).tickSize(-(w - m.l - m.r)).tickFormat(d3.format("d")))
      .call((g) => g.selectAll(".tick text").attr("x", -6));
    svgT.append("g").attr("class", "axis").attr("transform", `translate(0,${h - m.b})`)
      .call(d3.axisBottom(x).tickValues(d3.range(D.decades[0], D.decades[NT - 1] + 1, 40)).tickFormat(d3.format("d")).tickSize(3));
    svgT.append("line").attr("class", "now").attr("x1", x(decade())).attr("x2", x(decade())).attr("y1", m.t).attr("y2", h - m.b);
    const def = (p) => p.v != null;
    svgT.append("path").datum(pts).attr("fill", col).attr("fill-opacity", 0.18)
      .attr("d", d3.area().defined(def).x((p) => x(p.d)).y0((p) => y(p.lo)).y1((p) => y(p.hi)));
    if (lvl === "county")
      svgT.append("path").datum(pts).attr("class", "state-line")
        .attr("d", d3.line().defined((p) => p.s != null).x((p) => x(p.d)).y((p) => y(p.s)));
    svgT.append("path").datum(pts).attr("fill", "none").attr("stroke", col).attr("stroke-width", 2)
      .attr("d", d3.line().defined(def).x((p) => x(p.d)).y((p) => y(p.v)));
    svgT.append("g").selectAll("circle").data(pts.filter(def)).join("circle")
      .attr("cx", (p) => x(p.d)).attr("cy", (p) => y(p.v)).attr("r", (p) => (p.t === st.t ? 5 : 3.5))
      .attr("fill", (p) => (p.n === 0 ? css("--surface") : col)).attr("stroke", (p) => (p.n === 0 ? col : css("--surface")))
      .attr("stroke-width", (p) => (p.n === 0 ? 1.5 : 1));

    // hover crosshair and click to pick the decade
    const xh = svgT.append("line").attr("class", "xhair").attr("y1", m.t).attr("y2", h - m.b).style("display", "none");
    svgT.append("rect").attr("x", m.l).attr("y", m.t).attr("width", w - m.l - m.r).attr("height", h - m.t - m.b)
      .attr("fill", "transparent").style("cursor", "pointer")
      .on("mousemove", (e) => {
        const [mx] = d3.pointer(e), p = pts[Math.max(0, Math.min(NT - 1, Math.round((x.invert(mx) - D.decades[0]) / 10)))];
        xh.style("display", null).attr("x1", x(p.d)).attr("x2", x(p.d));
        const rows = [[DIMS[dim], interval(dim, i, p.t)]];
        if (lvl === "county") {
          rows.push([`State (${D.counties.state[i]})`, p.s == null ? "–" : fmt(p.s)]);
          rows.push(["County observations", p.n]);
        }
        tip.innerHTML = tipHTML(`${unitName(i)} &middot; ${p.d}`, rows); tip.hidden = false; placeTip(e);
      })
      .on("mouseleave", () => { xh.style("display", "none"); hideTip(); })
      .on("click", (e) => {
        const [mx] = d3.pointer(e);
        setDecade(Math.max(0, Math.min(NT - 1, Math.round((x.invert(mx) - D.decades[0]) / 10))));
      });
  }

  function drawTrajLegend() {
    const el = document.getElementById("traj-legend");
    if (st.sel == null) { el.innerHTML = ""; return; }
    const ink = css("--ink-2");
    let html = `<span><svg viewBox="0 0 22 10"><rect x="0" y="1" width="22" height="8" fill="${ink}" fill-opacity="0.18"/><line x1="0" y1="5" x2="22" y2="5" stroke="${ink}" stroke-width="2"/></svg>${st.level === "county" ? "County" : "State"}, 95% interval</span>`;
    if (st.level === "county") {
      html += `<span><svg viewBox="0 0 22 10"><line x1="0" y1="5" x2="22" y2="5" stroke="${css("--state-line")}" stroke-width="1.5" stroke-dasharray="4 3"/></svg>State</span>`;
      html += `<span><svg viewBox="0 0 22 10"><circle cx="11" cy="5" r="3.5" fill="${css("--surface")}" stroke="${ink}" stroke-width="1.5"/></svg>No county-level data</span>`;
    }
    html += `<span class="muted">Click a chart to change decade</span>`;
    el.innerHTML = html;
  }

  function drawTable() {
    const el = document.getElementById("table");
    if (st.sel == null) { el.innerHTML = `<p class="muted">Select a ${st.level} first.</p>`; return; }
    const i = st.sel, c = st.level === "county";
    let html = `<table><thead><tr><th>Decade</th><th>Hostility [95%]</th><th>Treatment [95%]</th><th>% Black</th>${c ? "<th>Obs.</th>" : ""}</tr></thead><tbody>`;
    D.decades.forEach((d, t) => {
      const pb = val("pb", i, t);
      html += `<tr${t === st.t ? ' style="font-weight:600"' : ""}><td>${d}</td><td>${interval("h", i, t)}</td><td>${interval("t", i, t)}</td>` +
        `<td>${pb == null ? "–" : fmt1(pb)}</td>${c ? `<td>${val("n", i, t) ?? 0}</td>` : ""}</tr>`;
    });
    el.innerHTML = html + "</tbody></table>";
  }

  // ---------- histogram of percent Black ----------
  function drawHist() {
    const host = document.getElementById("hist");
    host.innerHTML = "";
    const c = st.level === "county", plural = c ? "counties" : "states";
    document.getElementById("hist-title").textContent = `Percent Black, all ${plural}, ${decade()}`;
    const w = Math.max(260, host.clientWidth), h = 150, m = { l: 40, r: 12, t: 16, b: 24 };
    const svgH = d3.select(host).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("height", h);
    const vals = [];
    for (let i = 0, n = nUnits(); i < n; i++) { const v = val("pb", i, st.t); if (v != null) vals.push(v); }
    if (!vals.length) {
      svgH.append("text").attr("class", "empty").attr("x", w / 2).attr("y", h / 2).attr("text-anchor", "middle").text("No census data");
      return;
    }
    const step = c ? 2.5 : 5;
    const bins = d3.bin().domain([0, 100]).thresholds(d3.range(step, 100, step))(vals);
    const x = d3.scaleLinear().domain([0, 100]).range([m.l, w - m.r]);
    const y = (c ? d3.scaleSqrt() : d3.scaleLinear()).domain([0, d3.max(bins, (b) => b.length)]).nice().range([h - m.b, m.t]);
    svgH.append("g").attr("class", "grid").attr("transform", `translate(${m.l},0)`)
      .call(d3.axisLeft(y).ticks(c ? 3 : 4, "d").tickSize(-(w - m.l - m.r)))
      .call((g) => g.selectAll(".tick text").attr("x", -6));
    svgH.append("g").attr("class", "axis").attr("transform", `translate(0,${h - m.b})`)
      .call(d3.axisBottom(x).tickValues([0, 20, 40, 60, 80, 100]).tickFormat((d) => `${d}%`).tickSize(3));
    svgH.append("text").attr("x", m.l).attr("y", 9).attr("font-size", 10)
      .text(c ? `${vals.length} counties (square-root scale)` : `${vals.length} states`);
    const pb = st.sel == null ? null : val("pb", st.sel, st.t);
    svgH.append("g").selectAll("rect").data(bins).join("rect")
      .attr("class", (b) => (pb != null && pb >= b.x0 && (pb < b.x1 || (b.x1 === 100 && pb <= 100)) ? "bar hit" : "bar"))
      .attr("x", (b) => x(b.x0) + 0.5).attr("width", (b) => Math.max(0, x(b.x1) - x(b.x0) - 1))
      .attr("y", (b) => y(b.length)).attr("height", (b) => y(0) - y(b.length)).attr("rx", 1)
      .on("mousemove", (e, b) => {
        tip.innerHTML = tipHTML(`${b.x0}–${b.x1}% Black`, [[plural[0].toUpperCase() + plural.slice(1), b.length]]);
        tip.hidden = false; placeTip(e);
      })
      .on("mouseleave", hideTip);
    if (st.sel != null && pb != null) {
      const px = x(pb);
      svgH.append("line").attr("class", "marker-line").attr("x1", px).attr("x2", px).attr("y1", m.t - 2).attr("y2", h - m.b);
      const right = px < w * 0.65;
      svgH.append("text").attr("class", "marker-label").attr("x", px + (right ? 5 : -5)).attr("y", m.t + 8)
        .attr("text-anchor", right ? "start" : "end").text(`${c ? D.counties.name[st.sel] : D.states.id[st.sel]}: ${fmt1(pb)}%`);
    }
  }

  // ---------- controls ----------
  function setDecade(t) {
    st.t = t;
    document.getElementById("decade").value = t;
    document.getElementById("decade-value").textContent = decade();
    recolor(); renderSide(); writeHash();
  }

  function setLevel(level) {
    if (level === st.level) return;
    const prev = st.level, prevSel = st.sel;
    st.level = level;
    st.sel = prev === "county" && prevSel != null ? cState[prevSel] : null;
    pressed("[data-level]", "level", level);
    showLevel(); recolor(); hoverPath.attr("d", null);
    select(st.sel);
  }

  function setDim(dim) {
    st.dim = dim; pressed("[data-dim]", "dim", dim);
    recolor(); writeHash();
  }

  function pressed(selector, key, v) {
    document.querySelectorAll(selector).forEach((b) => b.setAttribute("aria-pressed", String(b.dataset[key] === v)));
  }

  function togglePlay() {
    const icon = document.getElementById("play-icon"), btn = document.getElementById("play");
    if (playTimer) {
      clearInterval(playTimer); playTimer = null;
      icon.setAttribute("d", "M4 2.5v11l9-5.5z"); btn.setAttribute("aria-label", "Play through the decades");
      return;
    }
    if (st.t >= NT - 1) setDecade(0);
    icon.setAttribute("d", "M4 2.5h3v11H4zM9 2.5h3v11H9z"); btn.setAttribute("aria-label", "Pause");
    playTimer = setInterval(() => {
      if (st.t >= NT - 1) { togglePlay(); return; }
      setDecade(st.t + 1);
    }, 1100);
  }

  function writeHash() {
    const p = new URLSearchParams({ level: st.level, dim: st.dim, decade: decade() });
    if (st.within) p.set("within", "1");
    if (st.sel != null) p.set("id", st.level === "county" ? D.counties.fips[st.sel] : D.states.id[st.sel]);
    history.replaceState(null, "", `#${p}`);
  }
  function readHash() {
    const p = new URLSearchParams(location.hash.slice(1));
    if (LEVELS[p.get("level")]) st.level = p.get("level");
    if (DIMS[p.get("dim")]) st.dim = p.get("dim");
    const t = D.decades.indexOf(Number(p.get("decade")));
    if (t >= 0) st.t = t;
    st.within = p.get("within") === "1";
    const id = p.get("id");
    if (id != null) { const i = (st.level === "county" ? cIdx : sIdx).get(id); if (i != null) st.sel = i; }
  }

  // ---------- start ----------
  Promise.all([d3.json("data/abi.json"), d3.json("data/us.topo.json"), d3.json("data/places.json")]).then(([abi, topo, placeData]) => {
    D = abi; NT = D.decades.length;
    D.counties.fips.forEach((f, i) => cIdx.set(f, i));
    D.states.id.forEach((s, i) => sIdx.set(s, i));
    cState = D.counties.state.map((s) => sIdx.get(s));
    readHash();

    drawMap(topo, placeData);
    const slider = document.getElementById("decade");
    slider.max = NT - 1; slider.value = st.t;
    document.getElementById("decade-value").textContent = decade();
    pressed("[data-level]", "level", st.level); pressed("[data-dim]", "dim", st.dim);
    pressed("[data-scale]", "scale", st.within ? "decade" : "all");
    showLevel(); recolor(); select(st.sel);

    slider.addEventListener("input", () => setDecade(+slider.value));
    document.querySelectorAll("[data-level]").forEach((b) => b.addEventListener("click", () => setLevel(b.dataset.level)));
    document.querySelectorAll("[data-dim]").forEach((b) => b.addEventListener("click", () => setDim(b.dataset.dim)));
    document.querySelectorAll("[data-scale]").forEach((b) => b.addEventListener("click", () => {
      st.within = b.dataset.scale === "decade"; pressed("[data-scale]", "scale", b.dataset.scale); recolor(); writeHash();
    }));
    document.getElementById("play").addEventListener("click", togglePlay);
    let rt; addEventListener("resize", () => { clearTimeout(rt); rt = setTimeout(() => { renderSide(); scheduleLabels(); }, 150); });
    addEventListener("scroll", clearHover, { passive: true });
    document.addEventListener("touchstart", (e) => {
      if (!e.target.closest(".unit, .chart svg")) clearHover();
    }, { passive: true });
    document.addEventListener("themechange", () => { recolor(); renderSide(); });   // theme.js
  }).catch((err) => {
    document.getElementById("map").innerHTML = `<div class="loading">Could not load the data (${err.message}). Serve the folder over http rather than opening the file directly.</div>`;
  });
})();
