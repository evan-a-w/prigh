// The editor mode's CodeMirror (vendor/codemirror.js) for the OCaml side
// (app/code_editor_widget.ml): window.prighCodeEditor.mount(host, options)
// gives a handle; the OCaml side owns the files, this only shows one and
// reports its edits, selection and scrolling.
(() => {
  const CM = window.PrighCodeMirror;
  const {
    EditorState, EditorSelection, Compartment, StateField, StateEffect,
    RangeSetBuilder, EditorView, keymap, lineNumbers, highlightActiveLine,
    highlightActiveLineGutter, drawSelection, Decoration, WidgetType, gutter,
    GutterMarker, history, defaultKeymap, historyKeymap, undo, redo,
    indentMore, cursorCharLeft, cursorCharRight, HighlightStyle,
    syntaxHighlighting, StreamLanguage, indentUnit, bracketMatching,
    indentOnInput, searchKeymap, highlightSelectionMatches, tags: t,
  } = CM;

  const legacy = (mode) => () => StreamLanguage.define(mode);
  const languages = {
    ocaml: legacy(CM.oCaml),
    markdown: () => CM.markdown(),
    javascript: () => CM.javascript({ jsx: true }),
    typescript: () => CM.javascript({ jsx: true, typescript: true }),
    rust: () => CM.rust(),
    python: () => CM.python(),
    nix: () => CM.nix(),
    shell: legacy(CM.shell),
    json: () => CM.json(),
    css: () => CM.css(),
    html: () => CM.html(),
    scheme: legacy(CM.scheme),
    yaml: legacy(CM.yaml),
    toml: legacy(CM.toml),
    go: legacy(CM.go),
    c: legacy(CM.c),
    cpp: legacy(CM.cpp),
  };

  // prigh-web's highlighting (highlight.css): the colours are theme.css's.
  const style = HighlightStyle.define([
    { tag: [t.keyword, t.controlKeyword, t.moduleKeyword, t.operatorKeyword,
            t.definitionKeyword], color: "var(--hl-keyword)", fontWeight: "600" },
    { tag: [t.typeName, t.className, t.namespace, t.tagName],
      color: "var(--hl-type)" },
    { tag: [t.string, t.special(t.string), t.regexp, t.character],
      color: "var(--hl-string)" },
    { tag: [t.number, t.bool, t.atom, t.constant(t.name), t.null],
      color: "var(--hl-number)" },
    { tag: [t.comment, t.lineComment, t.blockComment, t.docComment],
      color: "var(--hl-comment)", fontStyle: "italic" },
    { tag: [t.meta, t.processingInstruction], color: "var(--muted)" },
    { tag: t.heading, color: "var(--chalk)", fontWeight: "700" },
    { tag: t.strong, fontWeight: "700" },
    { tag: t.emphasis, fontStyle: "italic" },
    { tag: t.link, color: "var(--muted)", textDecoration: "underline" },
    { tag: t.inserted, color: "var(--sage)" },
    { tag: t.deleted, color: "var(--brick)" },
  ]);

  // The agent's change: its removed lines as a block above where they were,
  // its added lines tinted.
  class Removed extends WidgetType {
    constructor(lines) { super(); this.lines = lines; }
    eq(other) { return other.lines.join("\n") === this.lines.join("\n"); }
    toDOM() {
      const box = document.createElement("div");
      box.className = "cm-agent-removed";
      box.setAttribute("aria-label", "Removed by the agent");
      for (const line of this.lines) {
        const row = document.createElement("div");
        row.textContent = line === "" ? "\u200b" : line;
        box.appendChild(row);
      }
      return box;
    }
    ignoreEvent() { return false; }
  }
  const setHunks = StateEffect.define();
  const hunksField = StateField.define({
    create: () => Decoration.none,
    update(deco, tr) {
      for (const e of tr.effects) if (e.is(setHunks)) return e.value;
      return deco.map(tr.changes);
    },
    provide: (f) => EditorView.decorations.from(f),
  });
  const hunkDecorations = (doc, hunks) => {
    const ranges = [];
    for (const h of hunks) {
      const at = Math.min(h.line, doc.lines);
      const pos = h.line > doc.lines ? doc.length : doc.line(at).from;
      if (h.removed.length)
        ranges.push(Decoration.widget({
          widget: new Removed(h.removed), block: true,
          side: h.line > doc.lines ? 1 : -1,
        }).range(pos));
      for (let i = 0; i < h.added && h.line + i <= doc.lines; i++)
        ranges.push(Decoration.line({ class: "cm-agent-added" })
          .range(doc.line(h.line + i).from));
    }
    return Decoration.set(ranges, true);
  };

  // Lines the agent changed: a bar in the gutter.
  const bar = new (class extends GutterMarker {
    toDOM() { const s = document.createElement("span"); s.className = "cm-agent-bar"; return s; }
  })();
  const setMarks = StateEffect.define();
  const marksField = StateField.define({
    create: () => [],
    update(marks, tr) {
      for (const e of tr.effects) if (e.is(setMarks)) return e.value;
      if (!tr.docChanged) return marks;
      return marks.map(([a, b]) => {
        const doc = tr.startState.doc;
        if (a > doc.lines) return [a, b];
        const from = tr.changes.mapPos(doc.line(a).from);
        const to = tr.changes.mapPos(doc.line(Math.min(b, doc.lines)).from);
        return [tr.state.doc.lineAt(from).number, tr.state.doc.lineAt(to).number];
      });
    },
  });
  const marksGutter = gutter({
    class: "cm-agent-gutter",
    markers(view) {
      const builder = new RangeSetBuilder();
      const doc = view.state.doc;
      const lines = new Set();
      for (const [a, b] of view.state.field(marksField))
        for (let l = a; l <= b && l <= doc.lines; l++) lines.add(l);
      for (const l of [...lines].sort((x, y) => x - y))
        builder.add(doc.line(l).from, doc.line(l).from, bar);
      return builder.finish();
    },
  });

  // Soft wrap with a hanging indent: a wrapped line's rest sits two columns
  // past its own indentation.
  const hanging = ViewPluginFor();
  function ViewPluginFor() {
    const deco = (view) => {
      const builder = new RangeSetBuilder();
      for (const { from, to } of view.visibleRanges) {
        for (let pos = from; pos <= to;) {
          const line = view.state.doc.lineAt(pos);
          const indent = /^[ \t]*/.exec(line.text)[0].replace(/\t/g, "  ").length;
          builder.add(line.from, line.from, Decoration.line({
            attributes: { style: `--indent:${indent}ch` },
          }));
          pos = line.to + 1;
        }
      }
      return builder.finish();
    };
    return CM.ViewPlugin.fromClass(class {
      constructor(view) { this.decorations = deco(view); }
      update(u) { if (u.docChanged || u.viewportChanged) this.decorations = deco(u.view); }
    }, { decorations: (v) => v.decorations });
  }

  function mount(host, o) {
    const language = new Compartment();
    let quiet = false;
    let pending = 0;
    const report = (view) => {
      if (pending) return;
      pending = requestAnimationFrame(() => {
        pending = 0;
        o.onChange(view.state.doc.toString());
      });
    };
    // Edits not reported yet go before the text is replaced.
    const flush = () => {
      if (!pending) return;
      cancelAnimationFrame(pending);
      pending = 0;
      o.onChange(view.state.doc.toString());
    };
    const selection = (state) => {
      const r = state.selection.main;
      const first = state.doc.lineAt(r.from).number;
      const last = state.doc.lineAt(r.empty ? r.to : Math.max(r.from, r.to - 1)).number;
      return [first, last, state.sliceDoc(r.from, r.to)];
    };
    const extensions = [
      lineNumbers(), highlightActiveLineGutter(), highlightActiveLine(),
      drawSelection(), history(), bracketMatching(), indentOnInput(),
      highlightSelectionMatches(), indentUnit.of("  "),
      EditorState.tabSize.of(2),
      EditorView.lineWrapping, hanging, hunksField, marksField, marksGutter,
      syntaxHighlighting(style), language.of([]),
      keymap.of([...defaultKeymap, ...historyKeymap, ...searchKeymap,
                 { key: "Tab", run: indentMore }]),
      EditorView.contentAttributes.of({
        "aria-label": "File text", autocapitalize: "off",
        autocorrect: "off", spellcheck: "false",
      }),
      EditorView.updateListener.of((u) => {
        if (u.docChanged && !quiet) report(u.view);
        if (u.selectionSet || u.docChanged) {
          const [first, last, text] = selection(u.state);
          o.onSelect(first, last, text);
        }
      }),
    ];
    const view = new EditorView({
      parent: host,
      state: EditorState.create({ doc: "", extensions }),
    });
    let lastTop = 0;
    view.scrollDOM.addEventListener("scroll", () => {
      const top = view.scrollDOM.scrollTop;
      if (Math.abs(top - lastTop) < 24) return;
      o.onScroll(top > lastTop && top > 40 ? "down" : "up");
      lastTop = top;
    }, { passive: true });

    // Whatever the page does next (a shortcut, a tap on Save or Ask) sees
    // the text as typed: edits still waiting for their frame go first.
    const before = () => flush();
    document.addEventListener("keydown", before, true);
    document.addEventListener("pointerdown", before, true);
    const run = (cmd) => cmd(view);
    return {
      setDoc(text, lang) {
        flush();
        quiet = true;
        view.dispatch({
          changes: { from: 0, to: view.state.doc.length, insert: text },
          selection: EditorSelection.cursor(Math.min(view.state.selection.main.head, text.length)),
          effects: [language.reconfigure((languages[lang] || (() => []))()),
                    setHunks.of(Decoration.none), setMarks.of([])],
        });
        quiet = false;
      },
      // A new file: its own undo history and the top of it in view.
      load(text, lang) {
        flush();
        view.setState(EditorState.create({ doc: "", extensions }));
        this.setDoc(text, lang);
        view.scrollDOM.scrollTop = 0;
      },
      setChange(hunks, marks) {
        view.dispatch({
          effects: [setHunks.of(hunkDecorations(view.state.doc, hunks)),
                    setMarks.of(marks)],
        });
      },
      text: () => view.state.doc.toString(),
      exec(name, arg) {
        switch (name) {
          case "insert":
            view.dispatch(view.state.replaceSelection(arg));
            break;
          case "indent": run(indentMore); break;
          case "undo": run(undo); break;
          case "redo": run(redo); break;
          case "left": run(cursorCharLeft); break;
          case "right": run(cursorCharRight); break;
          case "goto": {
            const line = view.state.doc.line(Math.max(1, Math.min(arg, view.state.doc.lines)));
            view.dispatch({ selection: EditorSelection.cursor(line.from),
                            effects: EditorView.scrollIntoView(line.from, { y: "center" }) });
            break;
          }
          case "find": CM.openSearchPanel(view); break;
        }
        if (name !== "find") view.focus();
      },
      focus: () => view.focus(),
      dispose: () => {
        document.removeEventListener("keydown", before, true);
        document.removeEventListener("pointerdown", before, true);
        view.destroy();
      },
    };
  }
  window.prighCodeEditor = { mount };
})();
