/* Light/dark switch. With no saved choice the page follows the system
   setting (prefers-color-scheme); the button saves an explicit choice in
   localStorage and sets data-theme on <html>, which style.css reads. Loaded in
   <head> so a saved theme applies before the first paint. app.js listens for
   the "themechange" event to recolour the map and charts. */
(function () {
  "use strict";
  const KEY = "abi-theme";
  const root = document.documentElement;
  const media = matchMedia("(prefers-color-scheme: dark)");
  const saved = () => { try { return localStorage.getItem(KEY); } catch (e) { return null; } };
  const save = (v) => { try { localStorage.setItem(KEY, v); } catch (e) { /* private mode: not kept */ } };
  const current = () => root.getAttribute("data-theme") || (media.matches ? "dark" : "light");

  const s = saved();
  if (s === "light" || s === "dark") root.setAttribute("data-theme", s);

  const SUN = '<svg viewBox="0 0 16 16" aria-hidden="true"><circle cx="8" cy="8" r="3.2"/><g stroke-width="1.5" stroke-linecap="round"><path d="M8 1v1.6M8 13.4V15M1 8h1.6M13.4 8H15M3.05 3.05l1.13 1.13M11.82 11.82l1.13 1.13M3.05 12.95l1.13-1.13M11.82 4.18l1.13-1.13"/></g></svg>';
  const MOON = '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M13.5 10.2A6 6 0 0 1 5.8 2.5a6 6 0 1 0 7.7 7.7z"/></svg>';

  function paint(btn) {
    const dark = current() === "dark";
    btn.innerHTML = dark ? SUN : MOON;
    btn.setAttribute("aria-label", dark ? "Switch to light mode" : "Switch to dark mode");
    btn.title = btn.getAttribute("aria-label");
  }

  document.addEventListener("DOMContentLoaded", () => {
    const btn = document.getElementById("theme");
    if (!btn) return;
    paint(btn);
    btn.addEventListener("click", () => {
      const next = current() === "dark" ? "light" : "dark";
      root.setAttribute("data-theme", next);
      save(next);
      paint(btn);
      document.dispatchEvent(new Event("themechange"));
    });
    // follow system changes while no explicit choice is saved
    media.addEventListener("change", () => {
      if (!root.getAttribute("data-theme")) { paint(btn); document.dispatchEvent(new Event("themechange")); }
    });
  });
})();
