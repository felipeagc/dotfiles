// jj workspace dialog: { space, action: "new" | "open" | "remove" } or
// { folder, machine } for a repo without a space; optional { tab, argv }, and
// { name } to create at once, { confirmed: true } to remove at once.
//
// Layout on disk: ~/.herdr/jj-worktrees/<repo>/<name>/ holds the root repo's
// workspace, and <name>/<sub>/ the workspace of each sub-repo (a direct child
// folder of the root repo that is a jj repo itself, typically gitignored).
// The jj workspace name is always the folder name (<name>), in every repo.

import { View, div } from "gpui-kit";
import { v_flex, h_flex, InputState, Input, Scrollbar } from "gpui-base";
import { run, props, closeOverlay, theme, spaces, panes, exec, home } from "herdr";

// Where a space is now: its first terminal's live folder, else where it was made.
function place(s) {
  if (!s) return null;
  const tabs = new Set((s.tabs ?? []).map((c) => c.id));
  return panes().find((p) => tabs.has(p.tab) && p.cwd)?.cwd || s.folder || null;
}

const ROOT = (userHome) => `${userHome}/.herdr/jj-worktrees`;
const tilde = (path, userHome) => {
  const [p, h] = [path, userHome].map((s) => s.replace(/\\/g, "/"));
  return p.startsWith(h) ? `~${p.slice(h.length)}` : path;
};
const slug = (name) => name.replace(/[^A-Za-z0-9._-]+/g, "-").replace(/^-+|-+$/g, "");
const dirname = (p) => p.replace(/\/+$/, "").replace(/\/[^/]*$/, "") || "/";
const basename = (p) => p.replace(/\/+$/, "").split("/").pop();
const ago = (s) => {
  const d = Date.now() / 1000 - s;
  return d < 3600 ? `${Math.max(1, Math.round(d / 60))}m` : d < 86400 ? `${Math.round(d / 3600)}h` : d < 86400 * 60 ? `${Math.round(d / 86400)}d` : `${Math.round(d / 86400 / 30)}mo`;
};

// Reads pass --ignore-working-copy so polling a repo never snapshots it.
async function jj(cwd, args, on) {
  const r = await exec("jj", ["--no-pager", "--color=never", ...args], { cwd, on });
  return { ok: r.code === 0, out: (r.stdout ?? "").trim(), err: (r.stderr || r.stdout || "").trim() };
}
async function sh(script, args, on) {
  const r = await exec("sh", ["-c", script, "sh", ...args], { on });
  return { ok: r.code === 0, out: (r.stdout ?? "").trim(), err: (r.stderr || r.stdout || "").trim() };
}

// The main workspace folder of the repo a workspace folder belongs to
// (a secondary workspace's .jj/repo is a file pointing at the main one).
async function mainRoot(root, on) {
  const r = await sh('if [ -f "$1/.jj/repo" ]; then cd "$1/.jj" && cd "$(cat repo)/../.." && pwd -P; else cd "$1" && pwd -P; fi', [root], on);
  return r.ok && r.out ? r.out : root;
}

// Direct child folders of `dir` that are jj repos, by name.
async function subRepos(dir, on) {
  const r = await sh('for d in "$1"/*/; do [ -d "${d}.jj" ] && basename "$d"; done; true', [dir], on);
  return r.out.split("\n").filter(Boolean);
}

// The repo a folder belongs to: the root repo (the folder's own, or its
// direct parent when the folder is itself a sub-repo), its sub-repos, and
// whether the folder is one of our workspaces. null outside jj.
async function repoOf(folder, userHome, on) {
  const cur = await jj(folder, ["workspace", "root", "--ignore-working-copy"], on);
  if (!cur.ok) return null;
  const main = await mainRoot(cur.out, on);
  const parent = dirname(main);
  let root = main;
  if (parent !== userHome && parent !== "/" && (await sh('[ -d "$1/.jj" ]', [parent], on)).ok) root = await mainRoot(parent, on);
  return { root, name: basename(root), subs: await subRepos(root, on), cur: cur.out };
}

// Every workspace of the root repo: { name, path, main, at }, main first, then newest.
async function workspaces(repo, on) {
  const list = await jj(repo.root, ["workspace", "list", "--ignore-working-copy", "-T", 'name ++ "\\t" ++ target.committer().timestamp().format("%s") ++ "\\n"'], on);
  if (!list.ok) return [];
  const out = [];
  for (const line of list.out.split("\n").filter(Boolean)) {
    const [name, at] = line.split("\t");
    const path = await jj(repo.root, ["workspace", "root", "--name", name, "--ignore-working-copy"], on);
    if (!path.ok || !(await sh('[ -d "$1" ]', [path.out], on)).ok) continue; // forgotten folder
    out.push({ name, path: path.out, main: path.out === repo.root, at: Number(at) || 0 });
  }
  return out.sort((a, b) => b.main - a.main || b.at - a.at);
}

// The space showing a workspace: its first terminal is in it (the deepest one wins).
function spaceOf(path, all, on) {
  const inside = (p, dir) => p === dir || p.startsWith(dir + "/");
  return spaces().find((s) => (s.machine ?? null) === (on ?? null) && (() => {
    const at = place(s)?.replace(/\\/g, "/");
    if (!at || !inside(at, path)) return false;
    return !all.some((c) => c.path !== path && c.path.length > path.length && inside(at, c.path));
  })());
}

// <ROOT>/<repo>/<name> for a folder inside it, else null.
function workspaceDir(folder, userHome) {
  const root = ROOT(userHome) + "/";
  const f = (folder ?? "").replace(/\\/g, "/");
  if (!f.startsWith(root)) return null;
  const parts = f.slice(root.length).split("/").filter(Boolean);
  if (parts.length < 2 || parts.some((p) => p === "..")) return null;
  return { repo: parts[0], name: parts[1], dir: `${root}${parts[0]}/${parts[1]}` };
}

export default class JjWorkspace extends View {
  init(_props, cx) {
    this.on = this.space()?.machine;
    this.userHome = this.on ? null : home();
    this.action = props()?.action ?? "new";
    this.opening = this.action === "open";
    this.text = props()?.name ?? "";
    this.list = undefined; // open: the workspaces, once read
    this.at = 0;
    this.input = InputState.new({ value: this.text, placeholder: this.opening ? "filter workspaces" : "workspace name" });
    this.input.on("change", (_e, cx) => { this.text = this.input.value(); this.error = ""; this.at = 0; cx.notify(); });
    this.input.on("submit", (_e, cx) => this.go(cx));
    this.inputFocused = true;
    this.input.on("focus", (_e, cx) => { this.inputFocused = true; cx.notify(); });
    this.input.on("blur", (_e, cx) => { this.inputFocused = false; cx.notify(); });
    if (this.action !== "remove") {
      this.input.focus();
      cx.timer.after(600, () => this.input.focus());
    }
    this.busy = false;
    this.progress = "";
    this.error = "";
    this.repo = undefined; // undefined: looking, null: not in jj
    this.target = null; // remove: the workspace folder { repo, name, dir }
    this.dirty = []; // remove: the repos with changes in their working copy
    this.removeSubs = [];
    this.skip = new Set(); // new: sub-repos left out
    this.handle = cx.focus_handle();
    if (this.action === "remove") this.handle.focus();
    cx.spawn(async (cx) => {
      try {
        if (this.on) {
          const r = await sh('printf %s "$HOME"', [], this.on);
          if (!r.ok || !r.out) throw new Error("Could not find the remote home folder.");
          this.userHome = r.out;
        }
        this.folder = props()?.folder || place(this.space()) || this.userHome;
        if (this.action === "remove") {
          this.target = workspaceDir(this.folder, this.userHome);
          if (this.target) {
            this.removeSubs = await subRepos(this.target.dir, this.on);
            this.dirty = await this.dirtyRepos();
            this.repo = { name: this.target.repo };
          } else this.repo = null;
          cx.notify();
          if (this.repo && props()?.confirmed) await this.work(cx);
          return;
        }
        const repo = await repoOf(this.folder, this.userHome, this.on);
        if (repo && this.opening) this.list = await workspaces(repo, this.on);
        this.repo = repo;
        cx.notify();
        if (props()?.name && this.action === "new") await this.work(cx);
      } catch (error) { this.repo = null; this.error = String(error); cx.notify(); }
    });
  }

  space() {
    const p = props() ?? {};
    return spaces().find((s) => s.id === p.space) ?? (p.folder ? { folder: p.folder, machine: p.machine ?? null, name: "" } : undefined);
  }

  close() {
    closeOverlay(props().overlay);
  }

  go(cx) {
    if (this.busy || !this.repo || this.opening) return;
    cx.spawn(async (cx) => this.work(cx));
  }

  async work(cx) {
    if (this.busy || !this.repo) return;
    this.busy = true;
    this.error = "";
    cx.notify();
    try { this.error = this.action === "remove" ? await this.remove(cx) : await this.create(cx); }
    catch (error) { this.error = String(error); }
    this.busy = false;
    this.progress = "";
    if (!this.error) this.close();
    cx.notify();
  }

  async create(cx) {
    const name = slug(this.text.trim());
    if (!name) return "Type a workspace name.";
    const repo = this.repo;
    const wt = `${ROOT(this.userHome)}/${repo.name}/${name}`;
    if ((await sh('[ -e "$1" ]', [wt], this.on)).ok) return `${tilde(wt, this.userHome)} already exists.`;
    const subs = repo.subs.filter((s) => !this.skip.has(s));
    const steps = [{ label: repo.name, cwd: repo.root, dest: wt }, ...subs.map((s) => ({ label: s, cwd: `${repo.root}/${s}`, dest: `${wt}/${s}` }))];
    const made = [];
    for (const [i, step] of steps.entries()) {
      this.progress = `creating ${step.label}\u2026 (${i + 1}/${steps.length})`;
      cx.notify();
      // jj makes the workspace folder itself, not the folders above it.
      await sh('mkdir -p "$(dirname "$1")"', [step.dest], this.on);
      const r = await jj(step.cwd, ["workspace", "add", "--name", name, step.dest], this.on);
      if (!r.ok) return `${step.label}: ${r.err || "jj workspace add failed."}${made.length ? ` Already created and left in place: ${made.join(", ")}.` : ""}`;
      made.push(step.label);
    }
    const p = props();
    // Leave the space unnamed so it follows the folder.
    run("space.new", { folder: wt, machine: this.on, tab: p.tab, argv: p.argv });
    return "";
  }

  // The root workspace and each sub-repo workspace with changes in its working copy.
  async dirtyRepos() {
    const t = this.target;
    const out = [];
    for (const [label, dir] of [[t.repo, t.dir], ...this.removeSubs.map((s) => [s, `${t.dir}/${s}`])]) {
      const r = await jj(dir, ["diff", "--summary"], this.on);
      if (r.ok && r.out) out.push(label);
    }
    return out;
  }

  async remove() {
    const t = this.target;
    const s = this.space();
    // Only ever delete <ROOT>/<repo>/<name>.
    if (!t || t.dir.includes("..") || !new RegExp(`^${ROOT(this.userHome).replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}/[^/]+/[^/]+$`).test(t.dir)) return "Not one of this mod's workspaces.";
    const now = await this.dirtyRepos();
    const changed = now.some((r) => !this.dirty.includes(r));
    this.dirty = [...new Set([...this.dirty, ...now])];
    if (changed) return "It changed since this opened. Enter again removes it anyway.";
    // Forget the sub-repos first, then the root repo, so a failure leaves the folder alone.
    for (const [label, dir] of [...this.removeSubs.map((x) => [x, `${t.dir}/${x}`]), [t.repo, t.dir]]) {
      const main = await mainRoot(dir, this.on);
      const r = await jj(main, ["workspace", "forget", t.name], this.on);
      if (!r.ok && !/No such workspace/i.test(r.err)) return `${label}: ${r.err || "jj workspace forget failed."}`;
    }
    if (s?.id) run("space.close", { id: s.id, confirmed: true });
    const rm = await exec("rm", ["-rf", t.dir], { on: this.on });
    return rm.code === 0 ? "" : (rm.stderr || "rm failed.").trim();
  }

  shown() {
    const f = this.text.trim().toLowerCase();
    return (this.list ?? []).filter((c) => !f || `${c.name} ${c.path}`.toLowerCase().includes(f));
  }

  open(c) {
    if (!c) return;
    this.close();
    const s = spaceOf(c.path, this.list, this.on);
    if (s) run("space.select", { id: s.id });
    else run("space.new", { folder: c.path, machine: this.on });
  }

  key(e, cx) {
    if (e.key === "escape") return this.close();
    if (this.opening) {
      const n = this.shown().length;
      if (e.key === "enter") this.open(this.shown()[this.at]);
      else if ((e.key === "down" || e.key === "up") && n) this.at = (this.at + (e.key === "down" ? 1 : n - 1)) % n;
      else return;
      cx.stop_propagation();
      return cx.notify();
    }
    if (e.key === "enter" && this.action === "remove") return this.go(cx);
  }

  renderOpen(t) {
    const r = this.repo;
    const rows = this.shown();
    const all = this.list ?? [];
    const row = (c, i) => {
      const on = i === this.at;
      const there = spaceOf(c.path, all, this.on);
      return v_flex()
        .id(`jw-${i}`)
        .flex_none()
        .h(44)
        .px(10)
        .justify_center()
        .rounded(7)
        .cursor_pointer()
        .when(on, (el) => el.bg(t.selected))
        .when(!on, (el) => el.hover((s) => s.bg(t.hover)))
        .on_click(() => this.open(c))
        .child(
          h_flex()
            .gap(8)
            .items_center()
            .child(div().flex_1().min_w(0).truncate().text_color(t.foreground).child(c.name))
            .when(c.main, (el) => el.child(div().flex_none().text_size(11).text_color(t.dim).child("main workspace")))
            .when(Boolean(there), (el) => el.child(div().flex_none().text_size(11).text_color(t.accent).child("open")))
            .when(!there && c.at > 0, (el) => el.child(div().flex_none().text_size(11).text_color(t.dim).child(ago(c.at)))),
        )
        .child(div().truncate().text_size(11.5).text_color(t.dim).child(tilde(c.path, this.userHome ?? "")));
    };
    const count = rows.length === all.length ? `${all.length} workspaces` : `${rows.length}/${all.length} workspaces`;
    return v_flex()
      .id("jj-workspace")
      .track_focus(this.handle)
      .on_key_down((e, cx) => this.key(e, cx))
      .size_full()
      .p(16)
      .gap(10)
      .bg(t.popover)
      .text_size(13)
      .child(h_flex().child(div().flex_1().font_semibold().text_color(t.foreground).child(`Open jj workspace${r ? ` of ${r.name}` : ""}`)).child(div().text_size(11).text_color(t.dim).child(r ? count : "")))
      .when(r === undefined || (r && this.list === undefined), (el) => el.child(div().text_color(t.muted).child("\u2026")))
      .when(r === null, (el) => el.child(div().text_color(t.muted).child(`${this.folder ? tilde(this.folder, this.userHome ?? "") : "This folder"} is not in a jj repository.`)))
      .when(Boolean(r && this.list), (el) =>
        el
          .child(Input.new(this.input).h(32).px(10).items_center().rounded(6).border(1).border_color(this.inputFocused ? t.accent : t.input).bg(t.background).text_color(t.foreground))
          .child(
            div()
              .relative()
              .flex_1()
              .min_h(0)
              .child(
                v_flex()
                  .id("jw-list")
                  .size_full()
                  .overflow_y_scroll()
                  .children(rows.map(row))
                  .when(rows.length === 0, (el) => el.child(div().p(10).text_color(t.dim).child("No matching workspaces."))),
              )
              .child(Scrollbar.vertical("jw-list").absolute().inset_0()),
          ),
      )
      .child(h_flex().justify_end().text_size(11).text_color(t.dim).child("\u2191\u2193 choose \u00b7 \u21b5 open \u00b7 esc close"));
  }

  render(cx) {
    const t = theme();
    if (this.opening) return this.renderOpen(t);
    const remove = this.action === "remove";
    const r = this.repo;
    const tg = this.target;
    const home = this.userHome ?? "";
    const title = remove ? `Remove jj workspace ${tg?.name ?? ""}?` : `New jj workspace${r ? ` of ${r.name}` : ""}`;
    const detail =
      r === undefined
        ? "\u2026"
        : r === null
          ? remove
            ? `${this.folder ? tilde(this.folder, home) : "This space"} is not a workspace made here (they live in ~/.herdr/jj-worktrees).`
            : `${this.folder ? tilde(this.folder, home) : "This folder"} is not in a jj repository.`
          : remove
            ? `Closes the space and deletes ${tilde(tg.dir, home)}${this.removeSubs.length ? ` and its ${this.removeSubs.length} sub-repo workspace${this.removeSubs.length > 1 ? "s" : ""} (${this.removeSubs.join(", ")})` : ""}. The changes stay in the repo's history.`
            : `A new space on a workspace in ${tilde(ROOT(home), home)}/${r.name}/${slug(this.text.trim()) || "<name>"}${r.subs.length ? `, with a workspace of the same name in each sub-repo you leave ticked.` : "."}`;
    const blocked = !r;
    return v_flex()
      .id("jj-workspace")
      .track_focus(this.handle)
      .on_key_down((e, cx) => this.key(e, cx))
      .size_full()
      .p(16)
      .gap(10)
      .bg(t.popover)
      .text_size(13)
      .child(div().font_semibold().text_color(t.foreground).child(title))
      .child(div().text_color(t.muted).child(detail))
      .when(remove && this.dirty.length > 0, (el) => el.child(div().text_color(t.blocked).child(`${this.dirty.join(", ")} ${this.dirty.length > 1 ? "have" : "has"} changes in the working copy: the workspace goes, the changes stay in the repo's history (jj op log).`)))
      .when(!remove && !blocked, (el) =>
        el.child(Input.new(this.input).h(32).px(10).items_center().rounded(6).border(1).border_color(this.inputFocused ? t.accent : t.input).bg(t.background).text_color(t.foreground)),
      )
      .when(!remove && !blocked && r.subs.length > 0, (el) =>
        el.child(
          h_flex().gap(14).flex_wrap().children(
            r.subs.map((s) => {
              const on = !this.skip.has(s);
              return h_flex()
                .id(`sub-${s}`)
                .gap(6)
                .items_center()
                .cursor_pointer()
                .on_click((_e, cx) => { on ? this.skip.add(s) : this.skip.delete(s); cx.notify(); })
                .child(div().text_color(on ? t.accent : t.dim).child(on ? "\u2611" : "\u2610"))
                .child(div().text_color(on ? t.foreground : t.muted).child(s));
            }),
          ),
        ),
      )
      .when(Boolean(this.error), (el) => el.child(div().text_color(t.blocked).child(this.error)))
      .child(div().flex_1())
      .child(
        h_flex()
          .justify_end()
          .text_size(11)
          .text_color(t.dim)
          .child(this.busy ? this.progress || "working\u2026" : blocked ? "esc close" : remove ? "\u21b5 remove \u00b7 esc cancel" : "\u21b5 create \u00b7 esc cancel"),
      );
  }
}
