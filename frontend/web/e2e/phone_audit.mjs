// What is wrong on a phone's screen now, as stable lines ("ok": nothing):
// whether the page overflows sideways, and any control that is off screen,
// covered by another, smaller than a thumb (44px on its shorter side, 40px
// in the rows in [dense]) or an input iOS would zoom into (under 16px).
// Shared by mobile.mjs and smoke.mjs.
// Rows a phone keeps on screen with more in them than 44px targets leave
// room for, whose targets are 40px: the composer's (+, the model, the host,
// the status, send).
const dense = ".composer";
export const audit = page => page.evaluate(dense => {
  const out = [];
  const W = innerWidth, H = innerHeight;
  const sw = document.scrollingElement.scrollWidth;
  if (sw > W + 1) out.push(`overflows sideways: ${sw} > ${W}`);
  // Nor do the transcript and the dialogs' bodies (a long line in them
  // scrolls in its own box, a code block's, or wraps).
  for (const box of document.querySelectorAll("#chat, .modal-body, .settings-panel"))
    if (box.scrollWidth > box.clientWidth + 1) out.push(`${box.id ? "#" + box.id : box.className.split(" ")[0]} scrolls sideways`);
  const name = e => (e.getAttribute("aria-label") || e.innerText || e.getAttribute("title") || e.className || e.tagName)
    .toString().trim().replace(/\s+/g, " ").slice(0, 32);
  const visible = e => {
    const r = e.getBoundingClientRect();
    if (r.width < 1 || r.height < 1) return false;
    for (let a = e; a; a = a.parentElement) {
      const s = getComputedStyle(a);
      if (s.visibility === "hidden" || s.display === "none" || s.opacity === "0") return false;
    }
    return !e.closest("[inert], .sr-only");
  };
  const top = [...document.querySelectorAll(".modal, .chrome-menu, .chip-popover, .popup, .sidebar, .files-panel, .review-page")]
    .filter(visible).pop();
  const scope = top ?? document;
  const controls = [...scope.querySelectorAll("button, a[href], input:not([type=hidden]), textarea, select, [role=tab]")].filter(visible);
  const off = [], covered = [], small = [], zoom = [];
  for (const e of controls) {
    const r = e.getBoundingClientRect();
    // Inside a scroller, what is past its edge is scrolled to, not lost.
    let scroller = e.parentElement;
    while (scroller && !/(auto|scroll)/.test(getComputedStyle(scroller).overflowY + getComputedStyle(scroller).overflowX)) scroller = scroller.parentElement;
    const box = scroller ? scroller.getBoundingClientRect() : { left: 0, top: 0, right: W, bottom: H };
    const inBox = r.left >= Math.max(0, box.left) - 1 && r.right <= Math.min(W, box.right) + 1
      && r.top >= Math.max(0, box.top) - 1 && r.bottom <= Math.min(H, box.bottom) + 1;
    if (r.right > W + 1 || r.left < -1) { off.push(name(e)); continue; }
    if (!inBox) continue;
    const cx = Math.min(Math.max(r.left + r.width / 2, 0), W - 1), cy = Math.min(Math.max(r.top + r.height / 2, 0), H - 1);
    const hit = document.elementFromPoint(cx, cy);
    if (hit && !e.contains(hit) && !hit.contains(e) && !e.closest("label")?.contains(hit)) covered.push(`${name(e)} under ${name(hit)}`);
    const isText = e.matches("textarea, input:not([type=checkbox]):not([type=radio]):not([type=color]):not([type=range]), select");
    const min = e.closest(dense) ? 40 : 44;
    if (!isText && !e.matches("a") && Math.min(r.width, r.height) < min - 0.5 && !e.closest(".message, .md, .tool-body, .status-details"))
      small.push(`${name(e)} ${Math.round(r.width)}x${Math.round(r.height)}`);
    if (isText && parseFloat(getComputedStyle(e).fontSize) < 16) zoom.push(`${name(e)} ${getComputedStyle(e).fontSize}`);
  }
  if (off.length) out.push(`off screen: ${off.join(" | ")}`);
  if (covered.length) out.push(`covered: ${covered.join(" | ")}`);
  if (small.length) out.push(`small: ${small.join(" | ")}`);
  if (zoom.length) out.push(`zooms (under 16px): ${zoom.join(" | ")}`);
  return out.length ? out.join("\n") : "ok";
}, dense);

