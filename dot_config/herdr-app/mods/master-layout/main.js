import { View, div } from "gpui-kit";
import { panes, focused, run, nextCommand } from "herdr";

// Master on the left, everything else stacked on the right.
// Each new pane splits the last stack pane downward (sizes halve: herdr has no pane resize).
export default class MasterLayout extends View {
  init(_props, cx) {
    cx.spawn(async (cx) => {
      for (;;) {
        const c = await nextCommand();
        if (c === "new") this.newPane();
        else if (c === "next") this.cycle(1);
        else if (c === "prev") this.cycle(-1);
        cx.notify();
      }
    });
  }

  newPane() {
    const f = focused();
    if (!f || !f.pane) return void run("pane.split", { direction: "right" });
    const tabPanes = panes().filter((p) => p.tab === f.tab);
    if (tabPanes.length <= 1) return void run("pane.split", { direction: "right", pane: f.pane });
    const last = tabPanes[tabPanes.length - 1];
    run("pane.split", { direction: "down", pane: last.pane });
  }

  // Focus the next/previous pane of the shown tab in model order (master first), wrapping around.
  // Like tmux's `select-pane -Z`, a zoomed pane stays zoomed on the new one.
  cycle(step) {
    const f = focused();
    if (!f || !f.pane) return;
    const list = panes().filter((p) => p.tab === f.tab);
    const i = list.findIndex((p) => p.pane === f.pane);
    if (list.length < 2 || i < 0) return;
    const cur = list[i];
    const next = list[(i + step + list.length) % list.length];
    if (cur.zoomed) run("pane.zoom", { pane: cur.pane });
    run("pane.focus", { pane: next.pane });
    if (cur.zoomed) run("pane.zoom", { pane: next.pane });
  }

  render() {
    return div().size(0);
  }
}
