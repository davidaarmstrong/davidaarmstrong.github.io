/* Anti-Black Index map: data in data/abi.json (R/28_app_data.R) and
   data/us.topo.json (2010 counties, pre-projected Albers with Alaska and Hawaii insets).
   The data files keep the model's names for the dimensions: h (hostility) is
   shown as personal integrity violations, t (treatment) as civil liberties
   violations. */
(function () {
  "use strict";

  const W = 960, H = 600;               // map viewBox
  const NCLASS = 7;
  const DIMS = { h: "Personal integrity violations", t: "Civil liberties violations" };
  const SHORT = { h: "Personal integrity", t: "Civil liberties" };
  const LEVELS = { county: "county", state: "state" };
  const VIEWS = { time: 1, joint: 1, dist: 1 };
  // sel: the selected unit; cmp: an optional second unit drawn alongside it
  const st = { level: "county", dim: "h", t: 11, within: false, sel: null, cmp: null, view: "time", pal: "michigan" };

  let D, NT, cIdx = new Map(), sIdx = new Map(), cState = [];
  let cFeat = [], sFeat = [], mesh;
  let svg, gRoot, gC, gS, meshPath, hoverPath, selHalo, selPath, cmpHalo, cmpPath, zoom, pathGen;
  let gLab, countyMesh, places, cityXY = [], countyXY = [], stateXY = [], curT = d3.zoomIdentity, labelFrame = 0;
  let scale, playTimer = null, compareMode = false, lastCtx = 0, lastRender = null;
  const pooled = {};

  const fmt = d3.format(".1f"), fmt1 = d3.format(".1f");
  const tip = document.getElementById("tooltip");
  const css = (v) => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
  const isDark = () => {
    const a = document.documentElement.getAttribute("data-theme");
    return a ? a === "dark" : matchMedia("(prefers-color-scheme: dark)").matches;
  };
  const store = {
    get: (k) => { try { return localStorage.getItem(k); } catch (e) { return null; } },
    set: (k, v) => { try { localStorage.setItem(k, v); } catch (e) { /* not kept */ } },
  };

  // ---------- data helpers ----------
  const L = (level = st.level) => (level === "county" ? D.counties : D.states);
  const nUnits = (level = st.level) => L(level).name.length;
  const val = (key, i, t, level = st.level) => L(level)[key][i * NT + t];
  const unitName = (i, level = st.level) =>
    level === "county" ? `${D.counties.name[i]}, ${D.counties.state[i]}` : D.states.name[i];
  const shortName = (i, level = st.level) => (level === "county" ? D.counties.name[i] : D.states.id[i]);
  const decade = (t = st.t) => D.decades[t];
  const plural = () => (st.level === "county" ? "counties" : "states");
  // the units on the charts: the selection, then the comparator
  const chosen = () => [st.sel, st.cmp].filter((i) => i != null).map((i, k) => ({ i, k }));

  // series colours: the selection in the dimension's colour, the comparator in
  // a darker (light mode) or lighter (dark mode) shade of it, always dashed
  const dimCol = (dim) => css(dim === "h" ? "--c-h" : "--c-t");
  const shade = (c) => d3.interpolateLab(c, isDark() ? "#ffffff" : "#000000")(isDark() ? 0.45 : 0.4);
  const seriesCol = (dim, k) => (k === 0 ? dimCol(dim) : shade(dimCol(dim)));
  const DASH = "5 3";

  // ---------- palettes ----------
  // University of Michigan palette: personal integrity runs pale maize -> Maize ->
  // Ross Orange -> dark orange; civil liberties pale blue -> Arboretum Blue ->
  // Michigan Blue. Dark mode runs from the surface up to Maize / light blue.
  const RAMPS = {
    light: { h: ["#fff4cc", "#FFCB05", "#D86018", "#6b2a06"], t: ["#e3ebf5", "#8fb0d9", "#2F65A7", "#00274C"] },
    dark:  { h: ["#2a2f3a", "#8a5a12", "#D86018", "#FFCB05"], t: ["#1d2a3c", "#2F65A7", "#7ea4d6", "#d6e3f3"] },
  };
  // ColorBrewer sequential schemes (d3-scale-chromatic), one ramp for both dimensions
  const BREWER = {
    "Single hue": ["Blues", "Greens", "Greys", "Oranges", "Purples", "Reds"],
    "Multi-hue": ["BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"],
  };
  const isBrewer = (p) => Object.values(BREWER).some((l) => l.includes(p));
  function ramp(dim) {
    if (st.pal !== "michigan") {
      // light to dark; in dark mode reversed, so higher values are brighter on the dark page (as in the Michigan ramps)
      const c = d3[`scheme${st.pal}`][NCLASS].slice();
      return isDark() ? c.reverse() : c;
    }
    const stops = RAMPS[isDark() ? "dark" : "light"][dim];
    return d3.quantize(d3.piecewise(d3.interpolateLab, stops), NCLASS);
  }
  function initPalette() {
    const sel = document.getElementById("palette");
    let html = `<option value="michigan">Michigan</option>`;
    for (const [grp, names] of Object.entries(BREWER))
      html += `<optgroup label="ColorBrewer: ${grp.toLowerCase()}">${names.map((n) => `<option value="${n}">${n}</option>`).join("")}</optgroup>`;
    sel.innerHTML = html;
    sel.value = st.pal;
    sel.addEventListener("change", () => { st.pal = sel.value; store.set("abi-palette", st.pal); recolor(); writeHash(); });
  }

  // the values behind the colour classes (and the distribution charts):
  // every unit-decade, or the selected decade only
  function scopeVals(dim) {
    const arr = L()[dim];
    if (st.within) {
      const vals = [];
      for (let i = 0, n = nUnits(); i < n; i++) { const v = arr[i * NT + st.t]; if (v != null) vals.push(v); }
      return vals;
    }
    const k = st.level + dim;
    if (!pooled[k]) pooled[k] = arr.filter((v) => v != null);
    return pooled[k];
  }
  function buildScale() { scale = d3.scaleQuantile().domain(scopeVals(st.dim)).range(ramp(st.dim)); }

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
    // click selects; Ctrl-, Cmd- or Shift-click adds a comparator. On a Mac,
    // Ctrl-click fires contextmenu instead of click, so that is caught too.
    const mk = (g, feats, level) => g.selectAll("path").data(d3.range(feats.length).filter((i) => feats[i]))
      .join("path").attr("class", "unit").attr("d", (i) => pathGen(feats[i]))
      .on("mousemove", (e, i) => { showTip(e, i, level); hoverPath.attr("d", pathGen(feats[i])); })
      .on("mouseleave", () => { hideTip(); hoverPath.attr("d", null); })
      .on("click", (e, i) => { if (Date.now() - lastCtx > 400) pick(i, e.ctrlKey || e.metaKey || e.shiftKey); })
      .on("contextmenu", (e, i) => { if (e.ctrlKey) { e.preventDefault(); lastCtx = Date.now(); pick(i, true); } });
    mk(gC, cFeat, "county");
    mk(gS, sFeat, "state");
    countyMesh = gRoot.append("path").attr("class", "county-mesh")
      .attr("d", pathGen(topojson.mesh(topo, topo.objects.counties, (a, b) => a !== b)));
    meshPath = gRoot.append("path").attr("class", "state-mesh").attr("d", pathGen(mesh));
    hoverPath = gRoot.append("path").attr("class", "hover-outline");
    cmpHalo = gRoot.append("path").attr("class", "sel-halo");
    cmpPath = gRoot.append("path").attr("class", "sel-outline cmp-outline");
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
      `Hover for values; click a ${st.level} for its charts, and Ctrl-, ⌘- or Shift-click another to compare. Scroll or pinch to zoom.`;
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
  function showTipHTML(e, html) { tip.innerHTML = html; tip.hidden = false; placeTip(e); }
  const interval = (key, i, t, level) => {
    const v = val(key, i, t, level);
    return v == null ? "no estimate" : `${fmt(v)} [${fmt(val(key + "_lo", i, t, level))}, ${fmt(val(key + "_hi", i, t, level))}]`;
  };
  const pbText = (i, t) => { const pb = val("pb", i, t); return pb == null ? "no data" : `${fmt1(pb)}%`; };
  function showTip(e, i, level) {
    const pb = val("pb", i, st.t, level);
    const rows = [[SHORT[st.dim], interval(st.dim, i, st.t, level)], ["Percent Black", pb == null ? "no data" : `${fmt1(pb)}%`]];
    if (level === "county") rows.push(["County observations", val("n", i, st.t, level) ?? 0]);
    showTipHTML(e, tipHTML(`${unitName(i, level)} &middot; ${decade()}`, rows));
  }
  function hideTip() { tip.hidden = true; }
  // On touch screens a tap fires mousemove but never mouseleave, so the
  // tooltip and hover outline would stay up: clear them on scroll, on zoom
  // or pan, and on a tap anywhere that isn't a map unit or chart.
  function clearHover() { hideTip(); if (hoverPath) hoverPath.attr("d", null); }

  // ---------- selection ----------
  // A plain click selects; a compare click (or a click after "Compare") sets
  // the second unit, or removes it if it is already the second unit.
  function pick(i, compare) {
    if ((compare || compareMode) && st.sel != null) {
      if (i !== st.sel) st.cmp = st.cmp === i ? null : i;
    } else {
      st.sel = i;
      if (st.cmp === i) st.cmp = null;
    }
    compareMode = false;
    updateSelection();
  }
  function updateSelection() {
    const feats = st.level === "county" ? cFeat : sFeat;
    const d = (i) => (i != null && feats[i] ? pathGen(feats[i]) : null);
    selPath.attr("d", d(st.sel)); selHalo.attr("d", d(st.sel));
    cmpPath.attr("d", d(st.cmp)); cmpHalo.attr("d", d(st.cmp));
    renderSide(); writeHash();
  }

  const lineKey = (k, col) => `<svg class="key" viewBox="0 0 22 10" aria-hidden="true"><line x1="0" y1="5" x2="22" y2="5" stroke="${col}" stroke-width="2.2"${k ? ` stroke-dasharray="${DASH}"` : ""}/></svg>`;
  function unitVals(i, t) {
    const lvl = st.level, v = (k) => (val(k, i, t) == null ? "–" : fmt(val(k, i, t)));
    return `<div class="vals"><span>${SHORT.h} ${decade()}: <b>${v("h")}</b></span><span>${SHORT.t}: <b>${v("t")}</b></span>` +
      `<span>Percent Black: <b>${val("pb", i, t) == null ? "–" : fmt1(val("pb", i, t)) + "%"}</b></span>` +
      (lvl === "county" ? `<span>County observations: <b>${val("n", i, t) ?? 0}</b></span>` : "") + `</div>`;
  }

  function renderSide() {
    const lvl = st.level, i = st.sel, t = st.t, ink = css("--ink-2");
    const nameEl = document.getElementById("sel-name"), subEl = document.getElementById("sel-sub"), cmpEl = document.getElementById("cmp");
    if (i == null) {
      nameEl.textContent = `No ${lvl} selected`;
      subEl.innerHTML = `Click a ${lvl} on the map to see its charts on both dimensions.`;
      cmpEl.innerHTML = "";
    } else {
      nameEl.innerHTML = (st.cmp != null ? lineKey(0, ink) : "") + unitName(i);
      subEl.innerHTML = (lvl === "county" ? `County in ${D.states.name[cState[i]]}` : "State") + unitVals(i, t);
      if (st.cmp != null) {
        cmpEl.innerHTML = `<div class="cmp-head">${lineKey(1, ink)}<span>Compared with <b>${unitName(st.cmp)}</b></span>` +
          `<button type="button" class="x" id="cmp-clear" aria-label="Remove ${unitName(st.cmp)} from the charts" title="Remove from the charts">×</button></div>` +
          `<div class="muted">${unitVals(st.cmp, t)}</div>`;
        document.getElementById("cmp-clear").onclick = () => { st.cmp = null; updateSelection(); };
      } else {
        cmpEl.innerHTML = `<button type="button" class="small${compareMode ? " on" : ""}" id="cmp-mode" aria-pressed="${compareMode}">` +
          `${compareMode ? `Now pick a ${lvl} on the map` : "+ Compare"}</button>` +
          `<span class="muted">${compareMode ? "" : `or Ctrl-, ⌘- or Shift-click another ${lvl}`}</span>`;
        document.getElementById("cmp-mode").onclick = () => { compareMode = !compareMode; renderSide(); };
      }
    }
    // the newest segment of the path against percent Black is animated when
    // the decade has just moved forward one step on the same units
    const key = [st.view, lvl, st.sel, st.cmp].join("|");
    const animate = st.view === "joint" && lastRender && lastRender.key === key && t === lastRender.t + 1;
    lastRender = { key, t };
    for (const dim of ["h", "t"]) {
      const host = document.getElementById(`chart-${dim}`);
      host.innerHTML = "";
      if (st.view === "time") drawTraj(dim, host);
      else if (st.view === "joint") drawJoint(dim, host, animate);
      else drawDist(dim, host);
    }
    document.getElementById("replay").hidden = st.view !== "joint";
    drawChartLegend(); drawTable(); drawHist();
  }

  function emptyMsg(svgX, w, h, text) {
    svgX.append("text").attr("class", "empty").attr("x", w / 2).attr("y", h / 2).attr("text-anchor", "middle").text(text);
  }
  const decadeAt = (x, mx) => Math.max(0, Math.min(NT - 1, Math.round((x.invert(mx) - D.decades[0]) / 10)));

  // ---------- over time ----------
  function drawTraj(dim, host) {
    const w = Math.max(260, host.clientWidth), h = 150, m = { l: 40, r: 10, t: 8, b: 22 };
    const svgT = d3.select(host).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("height", h);
    const x = d3.scaleLinear().domain([D.decades[0], D.decades[NT - 1]]).range([m.l, w - m.r]);
    const lvl = st.level, units = chosen();
    if (!units.length) return emptyMsg(svgT, w, h, `Select a ${lvl} on the map`);
    const showState = lvl === "county" && units.length === 1;   // the state line only without a comparator
    const series = units.map(({ i, k }) => ({
      i, k, col: seriesCol(dim, k),
      pts: D.decades.map((d, t) => ({
        d, t, v: val(dim, i, t), lo: val(dim + "_lo", i, t), hi: val(dim + "_hi", i, t),
        n: lvl === "county" ? (val("n", i, t) ?? 0) : null,
        s: showState ? val(dim, cState[i], t, "state") : null,
      })),
    }));
    const ext = d3.extent(series.flatMap((s) => s.pts.flatMap((p) => [p.lo, p.hi, p.s])).filter((v) => v != null));
    if (ext[0] == null) return emptyMsg(svgT, w, h, "No estimates");
    const y = d3.scaleLinear().domain(ext).nice(4).range([h - m.b, m.t]);
    svgT.append("g").attr("class", "grid").attr("transform", `translate(${m.l},0)`)
      .call(d3.axisLeft(y).ticks(4).tickSize(-(w - m.l - m.r)).tickFormat(d3.format("d")))
      .call((g) => g.selectAll(".tick text").attr("x", -6));
    svgT.append("g").attr("class", "axis").attr("transform", `translate(0,${h - m.b})`)
      .call(d3.axisBottom(x).tickValues(d3.range(D.decades[0], D.decades[NT - 1] + 1, 40)).tickFormat(d3.format("d")).tickSize(3));
    svgT.append("line").attr("class", "now").attr("x1", x(decade())).attr("x2", x(decade())).attr("y1", m.t).attr("y2", h - m.b);
    const def = (p) => p.v != null;
    // bands first, then lines, so neither unit's band covers the other's line
    series.slice().reverse().forEach((s) => svgT.append("path").datum(s.pts).attr("fill", s.col).attr("fill-opacity", s.k ? 0.12 : 0.18)
      .attr("d", d3.area().defined(def).x((p) => x(p.d)).y0((p) => y(p.lo)).y1((p) => y(p.hi))));
    if (showState)
      svgT.append("path").datum(series[0].pts).attr("class", "state-line")
        .attr("d", d3.line().defined((p) => p.s != null).x((p) => x(p.d)).y((p) => y(p.s)));
    const surf = css("--surface");
    series.slice().reverse().forEach((s) => {
      svgT.append("path").datum(s.pts).attr("fill", "none").attr("stroke", s.col).attr("stroke-width", 2)
        .attr("stroke-dasharray", s.k ? DASH : null)
        .attr("d", d3.line().defined(def).x((p) => x(p.d)).y((p) => y(p.v)));
      svgT.append("g").selectAll("circle").data(s.pts.filter(def)).join("circle")
        .attr("cx", (p) => x(p.d)).attr("cy", (p) => y(p.v)).attr("r", (p) => (p.t === st.t ? 5 : s.k ? 3 : 3.5))
        .attr("fill", (p) => (p.n === 0 ? surf : s.col)).attr("stroke", (p) => (p.n === 0 ? s.col : surf))
        .attr("stroke-width", (p) => (p.n === 0 ? 1.5 : 1));
    });

    // hover crosshair and click to pick the decade
    const xh = svgT.append("line").attr("class", "xhair").attr("y1", m.t).attr("y2", h - m.b).style("display", "none");
    svgT.append("rect").attr("x", m.l).attr("y", m.t).attr("width", w - m.l - m.r).attr("height", h - m.t - m.b)
      .attr("fill", "transparent").style("cursor", "pointer")
      .on("mousemove", (e) => {
        const t = decadeAt(x, d3.pointer(e)[0]);
        xh.style("display", null).attr("x1", x(D.decades[t])).attr("x2", x(D.decades[t]));
        const rows = [];
        series.forEach((s) => {
          rows.push([units.length > 1 ? shortName(s.i) : SHORT[dim], interval(dim, s.i, t)]);
          if (lvl === "county" && units.length === 1) {
            rows.push([`State (${D.counties.state[s.i]})`, s.pts[t].s == null ? "–" : fmt(s.pts[t].s)]);
            rows.push(["County observations", s.pts[t].n]);
          }
        });
        const title = units.length > 1 ? `${SHORT[dim]} &middot; ${D.decades[t]}` : `${unitName(units[0].i)} &middot; ${D.decades[t]}`;
        showTipHTML(e, tipHTML(title, rows));
      })
      .on("mouseleave", () => { xh.style("display", "none"); hideTip(); })
      .on("click", (e) => setDecade(decadeAt(x, d3.pointer(e)[0])));
  }

  // ---------- against percent Black ----------
  // Each unit's path through (percent Black, dimension) from 1850 up to the
  // selected decade; older decades fade. Playing the decades draws it out.
  function drawJoint(dim, host, animate) {
    const w = Math.max(260, host.clientWidth), h = 210, m = { l: 40, r: 14, t: 10, b: 34 };
    const svgJ = d3.select(host).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("height", h);
    const units = chosen();
    if (!units.length) return emptyMsg(svgJ, w, h, `Select a ${st.level} on the map`);
    const series = units.map(({ i, k }) => ({
      i, k, col: seriesCol(dim, k),
      pts: D.decades.map((d, t) => ({ d, t, x: val("pb", i, t), y: val(dim, i, t) })).filter((p) => p.x != null && p.y != null),
    }));
    const all = series.flatMap((s) => s.pts);
    if (!all.length) return emptyMsg(svgJ, w, h, "No estimates");
    // axes cover every decade, so they stay put while the path is drawn out
    const x = d3.scaleLinear().domain([0, Math.max(1, d3.max(all, (p) => p.x))]).nice(5).range([m.l, w - m.r]);
    let ey = d3.extent(all, (p) => p.y);
    if (ey[1] - ey[0] < 4) ey = [ey[0] - 2, ey[1] + 2];
    const y = d3.scaleLinear().domain(ey).nice(4).range([h - m.b, m.t]);
    svgJ.append("g").attr("class", "grid").attr("transform", `translate(${m.l},0)`)
      .call(d3.axisLeft(y).ticks(4).tickSize(-(w - m.l - m.r)).tickFormat(d3.format("d")))
      .call((g) => g.selectAll(".tick text").attr("x", -6));
    svgJ.append("g").attr("class", "axis").attr("transform", `translate(0,${h - m.b})`)
      .call(d3.axisBottom(x).ticks(5).tickFormat((d) => `${d}%`).tickSize(3));
    svgJ.append("text").attr("class", "axis-title").attr("x", w - m.r).attr("y", h - 4).attr("text-anchor", "end").text("Percent Black");

    const surf = css("--surface");
    const op = (t) => Math.max(0.28, 1 - (st.t - t) * 0.07);
    const hits = [];
    series.slice().reverse().forEach((s) => {
      const shown = s.pts.filter((p) => p.t <= st.t);
      const g = svgJ.append("g");
      for (let j = 1; j < shown.length; j++) {
        const a = shown[j - 1], b = shown[j];
        const seg = g.append("line").attr("x1", x(a.x)).attr("y1", y(a.y)).attr("x2", x(b.x)).attr("y2", y(b.y))
          .attr("stroke", s.col).attr("stroke-width", 2).attr("stroke-linecap", "round").attr("opacity", op(b.t))
          .attr("stroke-dasharray", s.k ? DASH : null);
        if (animate && b.t === st.t && a.t === st.t - 1) {
          // grow the newest segment from the previous decade's point
          seg.attr("x2", x(a.x)).attr("y2", y(a.y)).transition().duration(650).ease(d3.easeCubicOut)
            .attr("x2", x(b.x)).attr("y2", y(b.y));
        }
      }
      shown.forEach((p) => {
        const cur = p.t === st.t;
        const c = g.append("circle").attr("cx", x(p.x)).attr("cy", y(p.y)).attr("r", cur ? 5 : 2.8)
          .attr("fill", s.k ? surf : s.col).attr("stroke", s.k ? s.col : surf).attr("stroke-width", s.k ? 1.6 : 1)
          .attr("opacity", op(p.t));
        if (cur && animate) c.attr("r", 0).transition().delay(450).duration(250).attr("r", 5);
        hits.push({ s, p });
      });
      // decade labels at the start and at the newest point
      const lab = (p, anchorEnd) => g.append("text").attr("class", "pt-label").attr("x", x(p.x) + (anchorEnd ? -7 : 7)).attr("y", y(p.y) - 6)
        .attr("text-anchor", anchorEnd ? "end" : "start").text(p.d);
      if (shown.length) {
        const first = shown[0], last = shown[shown.length - 1];
        lab(first, x(first.x) > w * 0.7);
        if (last !== first) lab(last, x(last.x) > w * 0.7);
      }
    });
    // hover and click targets, larger than the points
    svgJ.append("g").selectAll("circle").data(hits).join("circle")
      .attr("cx", (d) => x(d.p.x)).attr("cy", (d) => y(d.p.y)).attr("r", 9).attr("fill", "transparent").style("cursor", "pointer")
      .on("mousemove", (e, d) => showTipHTML(e, tipHTML(`${unitName(d.s.i)} &middot; ${d.p.d}`,
        [[SHORT[dim], interval(dim, d.s.i, d.p.t)], ["Percent Black", `${fmt1(d.p.x)}%`]])))
      .on("mouseleave", hideTip)
      .on("click", (e, d) => setDecade(d.p.t));
  }

  // ---------- distributions ----------
  // Every county's (or state's) value in the selected decade, or every
  // unit-decade, following the Quantiles switch above the map; the selected
  // units' values in the selected decade are marked.
  function drawDist(dim, host) {
    const c = st.level === "county", units = chosen().filter(({ i }) => val(dim, i, st.t) != null);
    const w = Math.max(260, host.clientWidth), m = { l: 40, r: 12, t: 16 + 13 * units.length, b: 24 }, h = 130 + 13 * units.length;
    const svgD = d3.select(host).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("height", h);
    const vals = scopeVals(dim).map((v) => Math.min(100, Math.max(0, v)));
    if (!vals.length) return emptyMsg(svgD, w, h, "No estimates");
    const step = c ? 2.5 : 5;
    const bins = d3.bin().domain([0, 100]).thresholds(d3.range(step, 100, step))(vals);
    const x = d3.scaleLinear().domain([0, 100]).range([m.l, w - m.r]);
    const y = (c || !st.within ? d3.scaleSqrt() : d3.scaleLinear()).domain([0, d3.max(bins, (b) => b.length)]).nice().range([h - m.b, m.t]);
    svgD.append("g").attr("class", "grid").attr("transform", `translate(${m.l},0)`)
      .call(d3.axisLeft(y).ticks(3, "d").tickSize(-(w - m.l - m.r)))
      .call((g) => g.selectAll(".tick text").attr("x", -6));
    svgD.append("g").attr("class", "axis").attr("transform", `translate(0,${h - m.b})`)
      .call(d3.axisBottom(x).tickValues([0, 20, 40, 60, 80, 100]).tickSize(3));
    const what = st.within ? `${vals.length} ${plural()}, ${decade()}` : `${d3.format(",")(vals.length)} ${st.level}-decades, ${D.decades[0]}–${D.decades[NT - 1]}`;
    svgD.append("text").attr("x", m.l).attr("y", 9).attr("font-size", 10).text(`${what}${y.exponent ? " (square-root scale)" : ""}`);
    const v0 = st.sel == null ? null : val(dim, st.sel, st.t);
    svgD.append("g").selectAll("rect").data(bins).join("rect")
      .attr("class", (b) => (v0 != null && v0 >= b.x0 && (v0 < b.x1 || b.x1 === 100) ? "bar hit" : "bar"))
      .attr("x", (b) => x(b.x0) + 0.5).attr("width", (b) => Math.max(0, x(b.x1) - x(b.x0) - 1))
      .attr("y", (b) => y(b.length)).attr("height", (b) => y(0) - y(b.length)).attr("rx", 1)
      .on("mousemove", (e, b) => showTipHTML(e, tipHTML(`${SHORT[dim]} ${b.x0}–${b.x1}`,
        [[st.within ? plural()[0].toUpperCase() + plural().slice(1) : `${st.level[0].toUpperCase()}${st.level.slice(1)}-decades`, d3.format(",")(b.length)]])))
      .on("mouseleave", hideTip);
    markers(svgD, units.map(({ i, k }) => ({ v: val(dim, i, st.t), k, col: seriesCol(dim, k), label: `${shortName(i)}: ${fmt(val(dim, i, st.t))}` })), x, m, w, h);
  }

  // marker lines for the selected units in a histogram, labels stacked above
  function markers(svgX, list, x, m, w, h) {
    list.forEach((d, j) => {
      const px = x(d.v), ly = 22 + 13 * j;
      svgX.append("line").attr("class", "marker-line").attr("x1", px).attr("x2", px).attr("y1", ly + 2).attr("y2", h - m.b)
        .style("stroke", d.col).attr("stroke-dasharray", d.k ? "4 3" : null);
      const right = px < w * 0.62;
      svgX.append("text").attr("class", "marker-label").attr("x", px + (right ? 5 : -5)).attr("y", ly)
        .attr("text-anchor", right ? "start" : "end").text(d.label);
    });
  }

  function drawChartLegend() {
    const el = document.getElementById("chart-legend"), units = chosen(), ink = css("--ink-2");
    const lvl = st.level, scopeTxt = st.within ? `all ${plural()} in ${decade()}` : `every ${lvl} in every decade`;
    document.getElementById("title-h").textContent = DIMS.h;
    document.getElementById("title-t").textContent = DIMS.t;
    let html = "";
    if (st.view === "dist") {
      html = `<span>Bars: ${scopeTxt} (set by the Quantiles switch above the map)</span>`;
      units.forEach(({ i, k }) => { html += `<span>${lineKey(k, ink)}${unitName(i)}, ${decade()}</span>`; });
      el.innerHTML = html; return;
    }
    if (!units.length) { el.innerHTML = ""; return; }
    if (st.view === "time") {
      units.forEach(({ i, k }) => {
        html += `<span><svg viewBox="0 0 22 10"><rect x="0" y="1" width="22" height="8" fill="${ink}" fill-opacity="${k ? 0.12 : 0.18}"/><line x1="0" y1="5" x2="22" y2="5" stroke="${ink}" stroke-width="2"${k ? ` stroke-dasharray="${DASH}"` : ""}/></svg>${units.length > 1 ? unitName(i) : lvl === "county" ? "County" : "State"}, 95% interval</span>`;
      });
      if (lvl === "county" && units.length === 1)
        html += `<span><svg viewBox="0 0 22 10"><line x1="0" y1="5" x2="22" y2="5" stroke="${css("--state-line")}" stroke-width="1.5" stroke-dasharray="4 3"/></svg>State</span>`;
      if (lvl === "county")
        html += `<span><svg viewBox="0 0 22 10"><circle cx="11" cy="5" r="3.5" fill="${css("--surface")}" stroke="${ink}" stroke-width="1.5"/></svg>No county-level data</span>`;
      html += `<span class="muted">Click a chart to change decade</span>`;
    } else {
      if (units.length > 1) units.forEach(({ i, k }) => { html += `<span>${lineKey(k, ink)}${unitName(i)}</span>`; });
      html += `<span class="muted">Each point is a decade, from 1850 to ${decade()}; older decades fade. Play to draw the path; click a point to change decade.</span>`;
    }
    el.innerHTML = html;
  }

  function drawTable() {
    const el = document.getElementById("table");
    if (st.sel == null) { el.innerHTML = `<p class="muted">Select a ${st.level} first.</p>`; return; }
    const c = st.level === "county";
    const one = (i) => {
      let html = `<table><thead><tr><th>Decade</th><th>${SHORT.h} [95%]</th><th>${SHORT.t} [95%]</th><th>% Black</th>${c ? "<th>Obs.</th>" : ""}</tr></thead><tbody>`;
      D.decades.forEach((d, t) => {
        const pb = val("pb", i, t);
        html += `<tr${t === st.t ? ' style="font-weight:600"' : ""}><td>${d}</td><td>${interval("h", i, t)}</td><td>${interval("t", i, t)}</td>` +
          `<td>${pb == null ? "–" : fmt1(pb)}</td>${c ? `<td>${val("n", i, t) ?? 0}</td>` : ""}</tr>`;
      });
      return html + "</tbody></table>";
    };
    el.innerHTML = st.cmp == null ? one(st.sel)
      : `<h4>${unitName(st.sel)}</h4>${one(st.sel)}<h4>${unitName(st.cmp)}</h4>${one(st.cmp)}`;
  }

  // ---------- histogram of percent Black ----------
  function drawHist() {
    const host = document.getElementById("hist");
    host.innerHTML = "";
    const c = st.level === "county";
    const units = chosen().filter(({ i }) => val("pb", i, st.t) != null);
    document.getElementById("hist-title").textContent = `Percent Black, all ${plural()}, ${decade()}`;
    const w = Math.max(260, host.clientWidth), h = 140 + 13 * units.length, m = { l: 40, r: 12, t: 16 + 13 * units.length, b: 24 };
    const svgH = d3.select(host).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("height", h);
    const vals = [];
    for (let i = 0, n = nUnits(); i < n; i++) { const v = val("pb", i, st.t); if (v != null) vals.push(v); }
    if (!vals.length) return emptyMsg(svgH, w, h, "No census data");
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
        const p = plural();
        showTipHTML(e, tipHTML(`${b.x0}–${b.x1}% Black`, [[p[0].toUpperCase() + p.slice(1), b.length]]));
      })
      .on("mouseleave", hideTip);
    const selCol = css("--sel");
    markers(svgH, units.map(({ i, k }) => ({ v: val("pb", i, st.t), k, col: selCol, label: `${shortName(i)}: ${fmt1(val("pb", i, st.t))}%` })), x, m, w, h);
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
    const prev = st.level, toState = (i) => (prev === "county" && i != null ? cState[i] : null);
    st.level = level;
    st.sel = toState(st.sel);
    st.cmp = toState(st.cmp);
    if (st.cmp === st.sel) st.cmp = null;
    compareMode = false;
    pressed("[data-level]", "level", level);
    showLevel(); recolor(); hoverPath.attr("d", null);
    updateSelection();
  }

  function setDim(dim) {
    st.dim = dim; pressed("[data-dim]", "dim", dim);
    recolor(); writeHash();
  }

  function setView(v) {
    st.view = v; pressed("[data-view]", "view", v);
    renderSide(); writeHash();
  }

  function pressed(selector, key, v) {
    document.querySelectorAll(selector).forEach((b) => b.setAttribute("aria-pressed", String(b.dataset[key] === v)));
  }

  function syncPlay() {
    const on = !!playTimer;
    document.getElementById("play-icon").setAttribute("d", on ? "M4 2.5h3v11H4zM9 2.5h3v11H9z" : "M4 2.5v11l9-5.5z");
    document.getElementById("play").setAttribute("aria-label", on ? "Pause" : "Play through the decades");
    document.getElementById("replay").textContent = on ? "Pause" : "Play from 1850";
  }
  function togglePlay(fromStart) {
    if (playTimer) { clearInterval(playTimer); playTimer = null; syncPlay(); return; }
    if (fromStart === true || st.t >= NT - 1) setDecade(0);
    playTimer = setInterval(() => {
      if (st.t >= NT - 1) { togglePlay(); return; }
      setDecade(st.t + 1);
    }, 1100);
    syncPlay();
  }

  function writeHash() {
    const p = new URLSearchParams({ level: st.level, dim: st.dim, decade: decade() });
    if (st.within) p.set("within", "1");
    const id = (i) => (st.level === "county" ? D.counties.fips[i] : D.states.id[i]);
    if (st.sel != null) p.set("id", id(st.sel));
    if (st.cmp != null) p.set("id2", id(st.cmp));
    if (st.view !== "time") p.set("view", st.view);
    if (st.pal !== "michigan") p.set("pal", st.pal);
    history.replaceState(null, "", `#${p}`);
  }
  function readHash() {
    const p = new URLSearchParams(location.hash.slice(1));
    if (LEVELS[p.get("level")]) st.level = p.get("level");
    if (DIMS[p.get("dim")]) st.dim = p.get("dim");
    const t = D.decades.indexOf(Number(p.get("decade")));
    if (t >= 0) st.t = t;
    st.within = p.get("within") === "1";
    if (VIEWS[p.get("view")]) st.view = p.get("view");
    const saved = store.get("abi-palette");
    if (isBrewer(saved)) st.pal = saved;
    if (isBrewer(p.get("pal")) || p.get("pal") === "michigan") st.pal = p.get("pal");
    const idx = st.level === "county" ? cIdx : sIdx;
    const id = idx.get(p.get("id")), id2 = idx.get(p.get("id2"));
    if (id != null) { st.sel = id; if (id2 != null && id2 !== id) st.cmp = id2; }
  }

  // ---------- start ----------
  Promise.all([d3.json("data/abi.json"), d3.json("data/us.topo.json"), d3.json("data/places.json")]).then(([abi, topo, placeData]) => {
    D = abi; NT = D.decades.length;
    D.counties.fips.forEach((f, i) => cIdx.set(f, i));
    D.states.id.forEach((s, i) => sIdx.set(s, i));
    cState = D.counties.state.map((s) => sIdx.get(s));
    readHash();

    drawMap(topo, placeData);
    initPalette();
    const slider = document.getElementById("decade");
    slider.max = NT - 1; slider.value = st.t;
    document.getElementById("decade-value").textContent = decade();
    pressed("[data-level]", "level", st.level); pressed("[data-dim]", "dim", st.dim);
    pressed("[data-scale]", "scale", st.within ? "decade" : "all"); pressed("[data-view]", "view", st.view);
    showLevel(); recolor(); updateSelection();

    slider.addEventListener("input", () => setDecade(+slider.value));
    document.querySelectorAll("[data-level]").forEach((b) => b.addEventListener("click", () => setLevel(b.dataset.level)));
    document.querySelectorAll("[data-dim]").forEach((b) => b.addEventListener("click", () => setDim(b.dataset.dim)));
    document.querySelectorAll("[data-view]").forEach((b) => b.addEventListener("click", () => setView(b.dataset.view)));
    document.querySelectorAll("[data-scale]").forEach((b) => b.addEventListener("click", () => {
      st.within = b.dataset.scale === "decade"; pressed("[data-scale]", "scale", b.dataset.scale); recolor(); renderSide(); writeHash();
    }));
    document.getElementById("play").addEventListener("click", () => togglePlay());
    document.getElementById("replay").addEventListener("click", () => togglePlay(true));
    let rt; addEventListener("resize", () => { clearTimeout(rt); rt = setTimeout(() => { renderSide(); scheduleLabels(); }, 150); });
    addEventListener("scroll", clearHover, { passive: true });
    document.addEventListener("touchstart", (e) => {
      if (!e.target.closest(".unit, .chart svg")) clearHover();
    }, { passive: true });
    document.addEventListener("keydown", (e) => { if (e.key === "Escape" && compareMode) { compareMode = false; renderSide(); } });
    document.addEventListener("themechange", () => { recolor(); renderSide(); });   // theme.js
  }).catch((err) => {
    document.getElementById("map").innerHTML = `<div class="loading">Could not load the data (${err.message}). Serve the folder over http rather than opening the file directly.</div>`;
  });
})();
