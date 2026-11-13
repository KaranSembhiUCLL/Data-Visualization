// ---------------------------------------------------------------------------
// charts.js - two D3 charts registered as Shiny output bindings
//   .d3-hist : Q6 histogram of best-match scores, draggable threshold line
//   .d3-bar  : Q4 backlog per category, click a bar to filter Q1
// ---------------------------------------------------------------------------
(function () {
  const arr = x => Array.isArray(x) ? x : (x == null ? [] : [x]);
  const css = name => getComputedStyle(document.documentElement).getPropertyValue(name).trim();

  // one shared tooltip
  let tip = null;
  function tooltip() {
    if (!tip) tip = d3.select("body").append("div").attr("class", "viz-tip").style("opacity", 0);
    return tip;
  }
  function showTip(event, html) {
    tooltip().html(html).style("opacity", 1)
      .style("left", (event.pageX + 14) + "px").style("top", (event.pageY - 12) + "px");
  }
  function hideTip() { tooltip().style("opacity", 0); }

  // ------------------------------------------------------------------ Q6
  function drawHist(el, data) {
    el.innerHTML = "";
    if (!data) return;
    const scores = arr(data.scores), known = arr(data.known);
    const W = el.clientWidth || 600, H = el.clientHeight || 320;
    const m = { t: 34, r: 18, b: known.length ? 80 : 42, l: 42 };
    const lo = data.min_x != null ? data.min_x : 0;
    const x = d3.scaleLinear().domain([lo, 1]).range([m.l, W - m.r]);
    const bins = d3.bin().domain([lo, 1]).thresholds(d3.range(lo, 1.0001, 0.025))(scores);
    const y = d3.scaleLinear().domain([0, d3.max(bins, b => b.length) || 1]).nice().range([H - m.b, m.t]);
    let thr = +data.threshold;

    const svg = d3.select(el).append("svg").attr("width", W).attr("height", H)
      .attr("role", "img").attr("aria-label", "Histogram of best-match similarity scores");

    svg.append("g").attr("class", "grid").attr("transform", `translate(${m.l},0)`)
      .call(d3.axisLeft(y).ticks(4).tickSize(-(W - m.l - m.r)).tickFormat(d3.format("d")))
      .call(g => g.select(".domain").remove());
    svg.append("g").attr("class", "axis").attr("transform", `translate(0,${H - m.b})`)
      .call(d3.axisBottom(x).ticks(Math.min(10, Math.floor(W / 70))).tickFormat(d3.format(".1f")));
    svg.append("text").attr("class", "axis-title").attr("x", m.l).attr("y", 16)
      .text("Pending products per score bin");
    svg.append("text").attr("class", "axis-title").attr("text-anchor", "end")
      .attr("x", W - m.r).attr("y", H - m.b + 32).text("Similarity to closest active product →");

    const bars = svg.append("g").selectAll("rect").data(bins).join("rect")
      .attr("x", b => x(b.x0) + 1).attr("width", b => Math.max(0, x(b.x1) - x(b.x0) - 2))
      .attr("y", b => y(b.length)).attr("height", b => y(0) - y(b.length))
      .attr("rx", 3)
      .on("mousemove", (e, b) => showTip(e,
        `<b>${b.x0.toFixed(3)} – ${b.x1.toFixed(3)}</b><br>${b.length} pending product${b.length === 1 ? "" : "s"}<br>` +
        `<span class="muted">${b.x0 >= thr ? "possible duplicate" : "treated as new"}</span>`))
      .on("mouseleave", hideTip);
    const paint = () => bars.attr("class", b => b.x0 >= thr - 1e-9 ? "bar dup" : "bar new");
    paint();

    // known duplicates (same barcode among active products) as a rug
    if (known.length) {
      const ry = H - m.b + 42;
      svg.append("g").selectAll("line").data(known).join("line").attr("class", "rug")
        .attr("x1", d => x(Math.max(lo, d))).attr("x2", d => x(Math.max(lo, d)))
        .attr("y1", ry - 7).attr("y2", ry + 7);
      svg.append("text").attr("class", "axis-title").attr("x", m.l).attr("y", ry + 24)
        .text(`Rug: ${known.length} known duplicates (active products sharing a barcode)`);
    }

    // draggable threshold line
    const g = svg.append("g").attr("class", "thr").style("cursor", "ew-resize");
    const line = g.append("line").attr("y1", m.t - 6).attr("y2", H - m.b);
    const hit = g.append("rect").attr("y", m.t - 22).attr("height", H - m.b - m.t + 22).attr("width", 20).attr("fill", "transparent");
    const lab = g.append("text").attr("y", m.t - 12);
    const place = () => {
      const px = x(thr);
      line.attr("x1", px).attr("x2", px); hit.attr("x", px - 10);
      lab.attr("x", px).attr("text-anchor", px > W - 150 ? "end" : "start").attr("dx", px > W - 150 ? -6 : 6)
        .text(`threshold ${thr.toFixed(2)} · drag me`);
    };
    place();
    g.call(d3.drag()
      .on("drag", e => { thr = Math.round(Math.min(1, Math.max(lo, x.invert(e.x))) * 100) / 100; place(); paint(); })
      .on("end", () => { if (window.Shiny) Shiny.setInputValue(el.id + "_threshold", thr, { priority: "event" }); }));
  }

  // ------------------------------------------------------------------ Q4
  function drawBar(el, data) {
    el.innerHTML = "";
    if (!data) return;
    const cats = arr(data.cat), pend = arr(data.pending), act = arr(data.active), prop = arr(data.prop);
    const rows = cats.map((c, i) => ({ cat: c, pending: +pend[i], active: +act[i], prop: +prop[i] }));
    const byProp = data.sort_by === "proportion";
    const val = d => byProp ? d.prop : d.pending;
    const rowH = 24, m = { t: 6, r: 96, b: 6, l: 182 };
    const W = el.clientWidth || 400, H = m.t + m.b + rows.length * rowH;
    el.style.height = H + "px";
    const x = d3.scaleLinear().domain([0, d3.max(rows, val) || 1]).range([m.l, W - m.r]);
    const y = d3.scaleBand().domain(rows.map(d => d.cat)).range([m.t, H - m.b]).padding(0.28);
    const sel = data.selected || null;

    const svg = d3.select(el).append("svg").attr("width", W).attr("height", H)
      .attr("role", "img").attr("aria-label", "Unvalidated products per category");
    const row = svg.selectAll("g.catrow").data(rows).join("g")
      .attr("class", d => "catrow" + (sel && d.cat !== sel ? " faded" : "") + (d.cat === sel ? " on" : ""))
      .style("cursor", "pointer")
      .on("click", (e, d) => { hideTip(); if (window.Shiny) Shiny.setInputValue(el.id + "_click", d.cat, { priority: "event" }); })
      .on("mousemove", (e, d) => showTip(e,
        `<b>${d.cat}</b><br>${d.pending} unvalidated · ${d.active} validated<br>` +
        `${(100 * d.prop).toFixed(1)}% of this category is unvalidated<br><span class="muted">click to filter the pending list</span>`))
      .on("mouseleave", hideTip);
    row.append("rect").attr("class", "hitrow").attr("x", 0).attr("width", W)
      .attr("y", d => y(d.cat) - 4).attr("height", y.bandwidth() + 8);
    row.append("rect").attr("class", d => "bar " + (d.cat === "Uncategorised" ? "uncat" : "dup"))
      .attr("x", m.l).attr("y", d => y(d.cat)).attr("height", y.bandwidth())
      .attr("width", d => Math.max(2, x(val(d)) - m.l)).attr("rx", 3);
    row.append("text").attr("class", "cat-label").attr("x", m.l - 8).attr("y", d => y(d.cat) + y.bandwidth() / 2)
      .attr("dy", "0.35em").attr("text-anchor", "end")
      .text(d => d.cat.length > 26 ? d.cat.slice(0, 25) + "…" : d.cat);
    row.append("text").attr("class", "val-label").attr("x", d => x(val(d)) + 6).attr("y", d => y(d.cat) + y.bandwidth() / 2)
      .attr("dy", "0.35em")
      .text(d => byProp ? `${(100 * d.prop).toFixed(1)}%  (${d.pending})` : `${d.pending}  · ${(100 * d.prop).toFixed(1)}%`);
  }

  // ------------------------------------------------- Shiny output bindings
  function makeBinding(cls, draw) {
    const b = new Shiny.OutputBinding();
    b.find = scope => $(scope).find("." + cls);
    b.renderValue = (el, data) => { el._data = data; draw(el, data); };
    b.resize = el => { if (el._data) draw(el, el._data); };
    Shiny.outputBindings.register(b, "foodcheck." + cls);
  }
  makeBinding("d3-hist", drawHist);
  makeBinding("d3-bar", drawBar);
})();
