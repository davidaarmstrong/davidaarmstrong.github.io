document.addEventListener("click", function(e) {
  const img = e.target.closest("img.zoomable");
  if (!img) return;

  const rect = img.getBoundingClientRect();

  const x = ((e.clientX - rect.left) / rect.width) * 100;
  const y = ((e.clientY - rect.top) / rect.height) * 100;

  img.style.transformOrigin = `${x}% ${y}%`;

  if (img.classList.contains("zoomed")) {
    img.classList.remove("zoomed");
    img.style.transform = "";
    img.style.cursor = "zoom-in";
  } else {
    const scale = img.dataset.scale || 2;
    img.classList.add("zoomed");
    img.style.transform = `scale(${scale})`;
    img.style.cursor = "zoom-out";
  }
});