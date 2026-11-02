import { View, div } from "gpui-kit";
import { v_flex, h_flex, Scrollbar, v_virtual_list, Editor, EditorState } from "gpui-base";
import { spaces, panes, focused, exec, watchFolder, theme, settings } from "herdr";

function location() {
  const space = spaces().find((s) => s.active), f = focused();
  const pane = f?.pane ? panes().find(p => p.pane === f.pane) : null;
  const on = f?.pane === null ? (f.terminal?.includes("/") ? f.terminal.split("/")[0] : null) : space?.machine ?? null;
  return { cwd: pane?.cwd || f?.cwd || space?.folder, on };
}
const key = () => JSON.stringify(location());
// The diff is shown in a read-only editor, so its text can be selected and copied.
// false: the original row-by-row view (colored lines, file line numbers, not selectable).
const EDITOR = true;
const MAX_PREDECESSORS = 32; // commits listed below the working copy
const SIDE = 164, SIDE_MIN = 110; // default and smallest width of the commit list (drag its right edge)
const ago = (s) => {
  const d = Date.now() / 1000 - s;
  return d < 60 ? "now" : d < 3600 ? `${Math.round(d / 60)}m` : d < 86400 ? `${Math.round(d / 3600)}h` : d < 86400 * 60 ? `${Math.round(d / 86400)}d` : `${Math.round(d / 86400 / 30)}mo`;
};
// One commit per line: id, short change id, working copy?, empty?, time, first line of the description.
const COMMIT_T = 'commit_id ++ "\\t" ++ change_id.shortest(8) ++ "\\t" ++ if(current_working_copy, "1", "0") ++ "\\t" ++ if(empty, "1", "0") ++ "\\t" ++ committer.timestamp().format("%s") ++ "\\t" ++ description.first_line() ++ "\\n"';
const LH = 18, PAD = 0, GUT = 48; // row height and editor top padding (px), gutter width

// Every call asks for jj's own diff formats (--summary, -T, --git): the user's
// ui.diff-formatter (e.g. difftastic) only applies to a bare `jj diff`, never used here.
const jj = (args, cwd, on) => exec("jj", ["--no-pager", "--color=never", ...args], { cwd, on });

// Numbers each diff line once, so a frame only draws the rows on screen.
function diffRows(lines) {
  let old = 0, next = 0;
  return lines.map((line) => {
    const match = /^@@ -(\d+)(?:,\d+)? \+(\d+)/.exec(line);
    if (match) { old = +match[1]; next = +match[2]; return { kind: "@", text: line }; }
    const kind = line[0];
    if (kind === "-") return { kind, number: old++, text: line.slice(1) || " " };
    if (kind === "+") return { kind, number: next++, text: line.slice(1) || " " };
    if (kind === " ") { old++; return { kind, number: next++, text: line.slice(1) || " " }; }
    return { kind: "", text: line };
  });
}

export default class Changes extends View {
  init(_props, cx) {
    this.files = []; this.rows = []; this.diffWidth = 0; this.handle = cx.focus_handle();
    const t = theme();
    this.sideW = Math.max(SIDE_MIN, Number(localStorage.getItem("sideW")) || SIDE); this.sideDrag = null; this.panelW = 0;
    this.commits = []; this.rev = null; this.pending = false; // rev: a commit id, null = the working copy
    this.codeText = "";
    this.scrollY = 0; this.viewH = 0; this.drag = null; this.selecting = false; // manual scrolling (see diffPane)
    this.code = EditorState.new({ language: "diff", readonly: true, line_numbers: false, folding: false, soft_wrap: false, value: "",
      font_family: settings().font_family, font_size: 11,
      // The diff grammar only colors the @@ and +/- marker characters, all with the same
      // pair of names (the rest of a line stays the default text color): no red/green lines.
      syntax: { punctuation: t.accent, constructor: t.accent, link_text: t.accent, "string.special": t.accent, "diff.minus": t.accent, "diff.plus": t.accent } });
    this.refresh(cx);
    cx.timer.every(10000, (cx) => this.refresh(cx));
  }

  watch(root, cx) {
    if (this.on || this.watched === root) return;
    this.watched = root;
    cx.spawn(async (cx) => {
      try {
        while (this.watched === root) {
          const changed = await watchFolder(root);
          // jj snapshots write under .jj (and the colocated .git): not a reason to refresh again.
          const mine = (p) => p.startsWith(`${root}/.jj/`) || p.startsWith(`${root}/.git/`) || p === `${root}/.jj` || p === `${root}/.git`;
          if (this.watched === root && !changed.every(mine)) this.refresh(cx);
        }
      } catch { this.watched = null; }
    });
  }

  refresh(cx) {
    if (this.loading) return;
    const { cwd, on } = location(), context = key();
    if (context !== this.context) {
      this.context = context; this.root = null; this.files = []; this.cache = {}; this.open = null; this.scrollY = 0; this.drag = null; this.commits = []; this.rev = null;
      this.setLines([]); this.error = null; this.note = null; this.watched = null; this.closed = false; this.limited = false;
    }
    if (!cwd) return;
    this.loading = true;
    cx.spawn(async (cx) => {
      try {
        const top = await jj(["workspace", "root", "--ignore-working-copy"], cwd, on);
        if (key() !== context) return;
        if (top.code !== 0) { this.root = null; this.files = []; this.cache = {}; this.setLines([]); this.open = null; this.error = "Not a jj repository"; return; }
        const root = top.stdout.trim();
        // The log call snapshots the working copy; everything after reads that snapshot.
        const log = await jj(["log", "--no-graph", "-r", `ancestors(@, ${MAX_PREDECESSORS + 1}) ~ root()`, "--limit", String(MAX_PREDECESSORS + 1), "-T", COMMIT_T], root, on);
        if (log.code !== 0) throw new Error(log.stderr);
        const commits = log.stdout.split("\n").filter(Boolean).map((line) => {
          const [id, short, wc, empty, at, ...desc] = line.split("\t");
          return { id, short, current: wc === "1", empty: empty === "1", at: Number(at) || 0, desc: desc.join(" ").trim() };
        });
        // A commit that left the list (rewritten, or too old now) falls back to the working copy.
        if (this.rev && !commits.some((c) => c.id === this.rev)) this.rev = null;
        const revArgs = this.rev ? ["-r", this.rev] : [];
        const list = await jj(["diff", ...revArgs, "--ignore-working-copy", "-T", 'path ++ "\t" ++ status_char ++ "\n"'], root, on);
        if (list.code !== 0) throw new Error(list.stderr);
        const diff = await jj(["diff", ...revArgs, "--git", "--ignore-working-copy"], root, on);
        if (diff.code !== 0) throw new Error(diff.stderr);
        const chunks = diff.stdout.split(/^(?=diff --git )/m).filter((c) => c.startsWith("diff --git "));
        const entries = list.stdout.split("\n").filter(Boolean);
        const files = [], cache = {};
        for (const [i, entry] of entries.slice(0, 1000).entries()) {
          const tab = entry.lastIndexOf("\t"), path = entry.slice(0, tab), status = entry.slice(tab + 1);
          // Chunks follow the file order; fall back to matching the header's new path.
          const chunk = chunks[i]?.split("\n", 1)[0].endsWith(` b/${path}`) ? chunks[i] : chunks.find((c) => c.split("\n", 1)[0].endsWith(` b/${path}`));
          const lines = (chunk ?? "").replace(/\n$/, "").split("\n"), first = lines.findIndex((l) => l.startsWith("@@"));
          const body = first < 0 ? [] : lines.slice(first);
          let add = 0, del = 0;
          for (const l of body) { if (l[0] === "+") add++; else if (l[0] === "-") del++; }
          cache[path] = body.length ? body : [status === "R" ? "Renamed · no content change" : chunk ? "Binary file or mode change · no text diff" : "No text diff available"];
          files.push({ path, status: status === "C" ? "A" : status, add, del });
        }
        if (key() !== context) return;
        this.root = root; this.on = on; this.commits = commits; this.files = files; this.cache = cache; this.limited = entries.length > 1000; this.error = null;
        this.watch(root, cx);
        if (this.open && !files.some((f) => f.path === this.open)) { this.open = null; this.setLines([]); }
        // The first file's diff shows until you pick another.
        if (!this.open && !this.closed && files.length) this.open = files[0].path;
        if (this.open) this.setLines(this.shownLines(this.open));
      } catch (e) { if (key() === context) this.error = String(e); }
      finally { this.loading = false; if (this.pending) { this.pending = false; this.refresh(cx); } cx.notify(); }
    });
  }

  selectRev(id, cx) {
    this.rev = id; this.open = null; this.closed = false; this.scrollY = 0; this.setLines([]); this.note = null;
    if (this.loading) this.pending = true; else this.refresh(cx);
    cx.notify();
  }

  shownLines(path) {
    return (this.cache?.[path] ?? []).slice(0, 1500).map((line) => line.length > 4000 ? line.slice(0, 4000) + "… (line truncated)" : line);
  }

  setLines(lines) {
    // Only on a real change: set_value would reset the scroll and drop a selection on every poll.
    const text = lines.join("\n");
    if (text !== this.codeText) { this.codeText = text; this.code.set_value(text); }
    this.rows = diffRows(lines);
    this.diffWidth = Math.max(300, ...lines.map(line => line.length * 7 + 50));
  }

  pick(path, cx) {
    this.open = this.open === path ? null : path; this.note = null; this.scrollY = 0;
    this.closed = !this.open;
    this.setLines(this.open ? this.shownLines(path) : []);
    cx.notify();
  }

  diffRow({ kind, number, text }, t) {
    if (kind === "@") return div().w_full().px(12).h(20).flex_none().truncate().bg(t.raised).text_color(t.dim).child(text);
    return h_flex().w_full().h(20).flex_none().gap(10).when(kind === "+", (e) => e.bg(t.ansi2 + "18")).when(kind === "-", (e) => e.bg(t.ansi1 + "18"))
      .child(div().w(30).flex_none().text_right().text_color(t.dim).child(number === undefined ? "" : String(number)))
      .child(div().flex_1().min_w(0).truncate().child(text));
  }

  revLabel() {
    const c = this.rev && this.commits.find((x) => x.id === this.rev);
    return c ? `${c.short} ${c.desc || "(no description)"}` : "";
  }

  // The working copy and its predecessors, newest first.
  sidebar(t) {
    const list = v_flex().id("changes-commits").size_full().overflow_y_scrollbar().p(6).gap(2).border_r(1).border_color(t.border)
      .children(this.commits.map((c) => {
        const sel = this.rev === null ? c.current : this.rev === c.id;
        return v_flex().id(`commit-${c.id}`).flex_none().px(8).py(5).gap(1).rounded(7).cursor_pointer()
          .when(sel, (e) => e.bg(t.selected)).hover((s) => s.bg(t.hover))
          .on_click((_e, cx) => this.selectRev(c.current ? null : c.id, cx))
          .child(div().text_size(12).truncate().text_color(c.desc ? t.foreground : t.dim).child(c.current ? (c.desc ? `@ ${c.desc}` : "@ Working copy") : (c.desc || "(no description)")))
          .child(h_flex().gap(6).text_size(10.5).text_color(t.dim)
            .child(div().text_color(sel ? t.accent : t.dim).child(c.short))
            .child(div().child(ago(c.at)))
            .when(c.empty, (e) => e.child(div().child("empty"))));
      }))
      .when(this.commits.length === 0, (e) => e.child(div().p(6).text_size(11).text_color(t.dim).child(this.loading ? "Loading…" : "")));
    // The right edge is a handle: drag it to resize (the drag layer in render follows the pointer).
    return div().relative().w(this.sideW).flex_none().h_full()
      .child(list)
      .child(div().id("changes-grip").absolute().top(0).bottom(0).right(-3).w(7).cursor_col_resize()
        .hover((x) => x.bg(t.accent + "44"))
        .on_mouse_down("left", (e, cx) => { cx.stop_propagation(); this.sideDrag = { x0: e.position.x, w0: this.sideW }; cx.notify(); })
        .when(Boolean(this.sideDrag), (x) => x.bg(t.accent + "66")));
  }

  // The diff: a read-only editor (selectable text) with one row per line behind it carrying
  // the gutter number and the green/red tint. The editor is as tall as its text and never
  // scrolls itself: a layer above it takes the wheel (the editor would otherwise eat it and
  // scroll only its text) and moves everything together; the scrollbar is drawn here too.
  scrollTo(y, cx) {
    const max = Math.max(0, this.rows.length * LH - this.viewH);
    const next = Math.min(Math.max(y, 0), max);
    if (next !== this.scrollY) { this.scrollY = next; cx.notify(); }
  }

  diffPane(t) {
    const n = this.rows.length, H = Math.max(n, 1) * LH, viewH = this.viewH;
    const max = Math.max(0, H - viewH), sy = Math.min(Math.max(this.scrollY, 0), max);
    const thumbH = max > 0 ? Math.max(28, viewH * viewH / H) : 0, room = Math.max(1, viewH - thumbH);
    const thumbTop = max > 0 ? sy / max * room : 0;
    // Only rows near the viewport are built; the editor itself is one element.
    const first = Math.max(0, Math.floor(sy / LH) - 8), last = Math.min(n, Math.ceil((sy + (viewH || 1200)) / LH) + 8);
    const rows = [];
    for (let i = first; i < last; i++) {
      const r = this.rows[i];
      rows.push(div().absolute().top(i * LH).left(0).w_full().h(LH).flex().items_center()
        .when(r.kind === "+", (e) => e.bg(t.ansi2 + "22"))
        .when(r.kind === "-", (e) => e.bg(t.ansi1 + "22"))
        .when(r.kind === "@", (e) => e.bg(t.raised + "88"))
        .child(div().w(GUT).pr(8).text_right().text_size(11).text_color(t.dim).child(r.number === undefined ? "" : String(r.number))));
    }
    return div().id("changes-diff").relative().size_full().overflow_hidden()
      .on_size_change((size, cx) => { if (Math.abs(size.height - this.viewH) > 0.5) { this.viewH = size.height; cx.notify(); } })
      // Selecting text: the editor never scrolls (it is as tall as its text), so dragging a
      // selection toward the top or bottom edge scrolls everything together, faster the closer to the edge.
      .on_mouse_down("left", () => { this.selecting = true; })
      .on_mouse_up("left", () => { this.selecting = false; })
      .on_mouse_move((e, cx) => {
        if (!this.selecting || this.drag) return;
        const y = e.local_position?.y ?? 0, EDGE = 36;
        if (y < EDGE) this.scrollTo(this.scrollY - Math.ceil((EDGE - y) / 3), cx);
        else if (y > viewH - EDGE) this.scrollTo(this.scrollY + Math.ceil((y - (viewH - EDGE)) / 3), cx);
      })
      .child(div().absolute().top(-sy).left(0).right(0).h(H)
        .children(rows)
        .child(Editor.new(this.code).absolute().top(0).left(GUT).right(0).h(H)))
      // Takes vertical wheel/trackpad scrolling before the editor sees it; sideways scrolling still reaches the editor.
      .child(div().id("changes-wheel").absolute().inset_0()
        .on_scroll_wheel((e, cx) => {
          if (Math.abs(e.delta.x) > Math.abs(e.delta.y)) return;
          cx.stop_propagation();
          this.scrollTo(this.scrollY - e.delta.y, cx);
        }))
      .when(max > 0, (el) => el.child(div().id("changes-track").absolute().top(0).right(0).bottom(0).w(12)
        .on_mouse_down("left", (e, cx) => {
          cx.stop_propagation();
          const y = e.local_position?.y ?? 0;
          // A press on the track (not the thumb) jumps there; either way the drag starts.
          if (y < thumbTop || y > thumbTop + thumbH) this.scrollTo((y - thumbH / 2) / room * max, cx);
          this.drag = { y0: e.position.y, s0: this.scrollY };
          cx.notify();
        })
        .child(div().absolute().top(thumbTop).right(2).w(7).h(thumbH).rounded(4).bg(t.muted + (this.drag ? "aa" : "55")))))
      // While dragging the thumb, this layer follows the pointer anywhere over the diff.
      .when(Boolean(this.drag), (el) => el.child(div().id("changes-drag").absolute().inset_0()
        .on_mouse_move((e, cx) => { if (this.drag) this.scrollTo(this.drag.s0 + (e.position.y - this.drag.y0) * max / room, cx); })
        .on_mouse_up("left", (_e, cx) => { this.drag = null; cx.notify(); })));
  }

  render(cx) {
    if (key() !== this.context) this.refresh(cx);
    const t = theme(), add = this.files.reduce((n, f) => n + f.add, 0), del = this.files.reduce((n, f) => n + f.del, 0);
    return v_flex().id("changes").relative().size_full().min_h(0).on_size_change((size, cx) => { if (Math.abs(size.width - this.panelW) > 0.5) { this.panelW = size.width; cx.notify(); } }).rounded(10).overflow_hidden().bg(t.background).border_1().border_color(t.border).text_color(t.foreground).text_size(13).track_focus(this.handle)
      .child(h_flex().h(42).flex_none().px(14).gap(8).items_center().border_b(1).border_color(t.border)
        .child(div().font_semibold().child("Changes"))
        .child(div().px(5).py(1).rounded(4).border_1().border_color(t.border).text_size(10).text_color(t.muted).child(`${this.files.length} files`))
        .child(div().flex_1().min_w(0).truncate().text_size(11).text_color(t.muted).child(this.revLabel())).child(div().text_size(11).text_color(t.ansi2).child(`+${add}`)).child(div().text_size(11).text_color(t.ansi1).child(`−${del}`)))
      .child(h_flex().flex_1().min_h(0).child(this.sidebar(t)).child(v_flex().flex_1().min_w(0).h_full().min_h(0)
      .child(v_flex().id("changes-list").flex_none().h(Math.min(200, Math.max(44, this.files.length * 28 + 12))).overflow_y_scrollbar().p(6).border_b(1).border_color(t.border).children(this.files.map((f) => {
        const parts = f.path.split("/"), name = parts.pop();
        return h_flex().id(`change-${f.path}`).h(28).flex_none().px(8).gap(8).items_center().rounded(7).cursor_pointer().when(this.open === f.path, (e) => e.bg(t.selected)).hover((s) => s.bg(t.hover)).on_click((_e, cx) => this.pick(f.path, cx))
          .child(div().w(14).text_size(11).text_color(f.status === "A" ? t.ansi2 : f.status === "D" ? t.ansi1 : f.status === "U" ? t.ansi1 : t.blocked).child(f.status))
          .child(h_flex().flex_1().min_w(0).child(div().text_color(t.dim).truncate().child(parts.length ? parts.join("/") + "/" : "")).child(div().max_w(260).min_w(0).truncate().child(name)))
          .when(f.add > 0, (e) => e.child(div().text_size(11).text_color(t.ansi2).child(`+${f.add}`))).when(f.del > 0, (e) => e.child(div().text_size(11).text_color(t.ansi1).child(`−${f.del}`)));
      })).when(!this.files.length, (e) => e.child(div().p(8).text_color(t.dim).child(this.error || (this.loading ? "Loading changes…" : this.root ? (this.rev ? "No changes in this commit" : "No changes in the working copy") : "No folder")))))
      .when(this.limited, el => el.child(div().px(12).py(4).text_size(11).text_color(t.dim).child("Showing the first 1,000 changed files")))
      .child(div().flex_1().min_h(0).relative().font_family(settings().font_family ?? "JetBrains Mono").text_size(11)
        .child(EDITOR ? this.diffPane(t)
          : div().id("changes-diff-x").size_full().overflow_x_scrollbar()
            .child(div().w_full().min_w(this.diffWidth).h_full().child(v_virtual_list("changes-diff", this.rows.length, 20, (i) => String(i),
              (range) => this.rows.slice(range.start, range.end).map((row) => this.diffRow(row, t))).size_full())))
        .when(!EDITOR, (e) => e.child(Scrollbar.vertical("changes-diff").viewport_from_layout().absolute().inset_0()))
        .when(!this.open && this.files.length > 0, (e) => e.child(div().absolute().top(6).px(12).text_color(t.dim).child("Select a file to view its changes"))))
      .when(Boolean(this.note), (e) => e.child(div().px(10).pb(8).text_size(11).text_color(t.ansi1).child(this.note)))))
      // While resizing, this layer follows the pointer anywhere over the panel.
      .when(Boolean(this.sideDrag), (el) => el.child(div().id("changes-side-drag").absolute().inset_0().cursor_col_resize()
        .on_mouse_move((e, cx) => {
          if (!this.sideDrag) return;
          const max = Math.max(SIDE_MIN, (this.panelW || 600) - 240); // leave room for the files and diff
          const w = Math.min(Math.max(this.sideDrag.w0 + (e.position.x - this.sideDrag.x0), SIDE_MIN), max);
          if (w !== this.sideW) { this.sideW = w; cx.notify(); }
        })
        .on_mouse_up("left", (_e, cx) => { localStorage.setItem("sideW", String(Math.round(this.sideW))); this.sideDrag = null; cx.notify(); })));
  }
}
