import { View, div } from "gpui-kit";
import { exec, focused, openOverlay, closeOverlay, run, nextCommand, notify } from "herdr";

// Toggle a big popup running jjui in jj repos, else lazygit in the focused folder.
export default class GitPopup extends View {
  init(_props, cx) {
    this.open = null; // { overlay, terminal }
    cx.spawn(async (cx) => {
      for (;;) {
        const c = await nextCommand();
        if (c === "toggle") await this.toggle();
        cx.notify();
      }
    });
  }

  async toggle() {
    if (this.open) {
      const { overlay, terminal } = this.open;
      this.open = null;
      // The tool may have quit by itself (q): then the terminal and dialog are already gone and
      // this press should open a fresh popup. Ask the core, not Herdr's cached terminals(), which
      // can still report the terminal live for a moment after it exits.
      const live = await exec("hc", ["ls", "--json"]).then(
        (r) => {
          const all = JSON.parse(r.stdout);
          return (Array.isArray(all) ? all : all.terminals ?? []).some((t) => t.address === terminal && t.state === "live");
        },
        () => false,
      );
      closeOverlay(overlay);
      if (live) {
        run("terminal.kill", { terminal });
        return;
      }
    }
    const cwd = focused()?.cwd ?? undefined;
    try {
      // jjui inside a jj repo, lazygit everywhere else (jj missing counts as "not a jj repo").
      const inJj = await exec("jj", ["root"], { cwd }).then((r) => r.code === 0, () => false);
      const tool = inJj ? "jjui" : "lazygit";
      const r = await exec("hc", ["run", "--bg", "--json", "--no-parent", "--rm", ...(cwd ? ["--cwd", cwd] : []), "--", tool]);
      const terminal = JSON.parse(r.stdout).address;
      const overlay = openOverlay({
        kind: "dialog",
        modal: true,
        width: 0.9,
        height: 0.9,
        layout: { type: "column", children: [{ id: "git-term", type: "block", content: { terminal } }] },
      });
      this.open = { overlay, terminal };
    } catch (e) {
      notify({ title: "git-popup failed", body: String(e), level: "error" });
    }
  }

  render() {
    return div().size(0);
  }
}
