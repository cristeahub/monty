const frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

export function showNavigationSpinner(ctx, message, timers = globalThis) {
  let current = ctx;
  let index = 0;
  let stopped = false;
  const draw = () => {
    if (stopped) return;
    try {
      if (!current.hasUI) return;
      const frame = frames[index++ % frames.length];
      current.ui.setStatus("monty-navigation",
        current.ui.theme.fg("accent", `${frame} ${message}`));
    } catch {
      // Every property on the source context is stale while Pi replaces its runtime.
    }
  };
  draw();
  const timer = timers.setInterval(draw, 80);
  timer.unref?.();
  return {
    bind(next) { current = next; draw(); },
    stop() {
      if (stopped) return;
      stopped = true;
      timers.clearInterval(timer);
      try { current.ui.setStatus("monty-navigation", undefined); }
      catch {
        // A successful switch clears this status through the replacement context.
      }
    },
  };
}
