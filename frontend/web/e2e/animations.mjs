// Waits until the page's animations are over (those of selector's element
// and its subtree, if given; it waits for the element too). Endless ones
// (spinners) never are, and do not count. A finished animation that fills
// nothing yet is still listed is one WebKit has not taken out of the style:
// on a busy machine, a frame or more after it finished, it still draws its
// last step (a sheet 2-3px short of the bottom).
export const animationsOver = (target, selector) => target.waitForFunction(selector => {
  const root = selector ? document.querySelector(selector) : document;
  if (!root) return false;
  return root.getAnimations(selector ? { subtree: true } : undefined).every(a => {
    const timing = a.effect?.getComputedTiming();
    if (timing?.iterations === Infinity) return true;
    if (a.playState === "finished") return timing?.fill === "forwards" || timing?.fill === "both";
    return a.playState !== "running";
  });
}, selector);
