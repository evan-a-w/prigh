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

  const el = (tag, cls, text) => {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    return e;
  };
  const action = (label, cls, run) => {
    const b = el("button", "cm-agent-act " + cls, label);
    b.type = "button";
    b.addEventListener("mousedown", (ev) => ev.preventDefault());
    b.addEventListener("click", (ev) => { ev.preventDefault(); run(); });
    return b;
  };

  // One of the agent's changes: a head with its Keep and Undo, its removed
  // lines struck through above where they were; its added lines tinted.
  class Change extends WidgetType {
    constructor(h, index, total, on) {
      super(); this.h = h; this.index = index; this.total = total; this.on = on;
    }
    eq(o) {
      return o.index === this.index && o.total === this.total
        && o.h.removed.join("\n") === this.h.removed.join("\n")
        && o.h.added.length === this.h.added.length;
    }
    toDOM() {
      const box = el("div", "cm-agent-change");
      box.setAttribute("role", "group");
      box.setAttribute("aria-label", `The agent's change ${this.index + 1} of ${this.total}`);
      const head = el("div", "cm-agent-head");
      const n = this.h.added.length, r = this.h.removed.length;
      head.appendChild(el("span", "cm-agent-label",
        (this.total > 1 ? `${this.index + 1}/${this.total} · ` : "") + `+${n} −${r}`));
      head.appendChild(action("Undo", "", () => this.on(this.index, "undo")));
      head.appendChild(action("Keep", "primary", () => this.on(this.index, "keep")));
      box.appendChild(head);
      if (r) {
        const removed = el("div", "cm-agent-removed");
        removed.setAttribute("aria-label", "Removed by the agent");
        for (const line of this.h.removed)
          removed.appendChild(el("div", "", line === "" ? "\u200b" : line));
        box.appendChild(removed);
      }
      return box;
    }
    ignoreEvent() { return true; }
  }

  // The ask box: a prompt about the lines above it.
  class Ask extends WidgetType {
    constructor(label, state) { super(); this.label = label; this.state = state; }
    eq(o) { return o.label === this.label; }
    toDOM() {
      const st = this.state;
      const box = el("form", "cm-ask");
      box.setAttribute("aria-label", "Ask about " + this.label);
      const head = el("div", "cm-ask-head");
      head.appendChild(el("span", "cm-ask-chip", this.label));
      head.appendChild(action("Cancel", "", () => st.on({ cancel: true })));
      box.appendChild(head);
      const row = el("div", "cm-ask-row");
      const input = el("textarea", "cm-ask-input");
      input.rows = 1;
      input.placeholder = "Ask or instruct about these lines";
      input.setAttribute("aria-label", "Ask about " + this.label);
      input.setAttribute("enterkeyhint", "send");
      input.value = st.draft;
      const grow = () => {
        input.style.height = "auto";
        input.style.height = Math.min(input.scrollHeight, 160) + "px";
      };
      input.addEventListener("input", () => { st.draft = input.value; grow(); });
      input.addEventListener("keydown", (ev) => {
        if (ev.key === "Enter" && !ev.shiftKey && !ev.isComposing) {
          ev.preventDefault(); ev.stopPropagation();
          st.on({ send: input.value });
        } else if (ev.key === "Escape") {
          ev.preventDefault(); ev.stopPropagation();
          st.on({ cancel: true });
        }
      });
      row.appendChild(input);
      const send = action("Send", "primary cm-ask-send", () => st.on({ send: input.value }));
      send.setAttribute("aria-label", "Send");
      row.appendChild(send);
      box.appendChild(row);
      box.addEventListener("submit", (ev) => { ev.preventDefault(); st.on({ send: input.value }); });
      requestAnimationFrame(() => { input.focus({ preventScroll: true }); grow();
        box.scrollIntoView({ block: "nearest" }); });
      return box;
    }
    ignoreEvent() { return true; }
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
  const lineStart = (doc, line) =>
    line > doc.lines ? doc.length : doc.line(Math.max(1, line)).from;
  const hunkDecorations = (doc, hunks, on, ask) => {
    const ranges = [];
    hunks.forEach((h, i) => {
      ranges.push(Decoration.widget({
        widget: new Change(h, i, hunks.length, on), block: true,
        side: h.line > doc.lines ? 1 : -1,
      }).range(lineStart(doc, h.line)));
      for (let k = 0; k < h.added.length && h.line + k <= doc.lines; k++)
        ranges.push(Decoration.line({ class: "cm-agent-added" })
          .range(doc.line(h.line + k).from));
    });
    if (ask) {
      const line = doc.line(Math.max(1, Math.min(ask.last, doc.lines)));
      ranges.push(Decoration.widget({
        widget: new Ask(ask.label, ask.state), block: true, side: 1,
      }).range(line.to));
    }
    return Decoration.set(ranges, true);
  };

  // Lines the agent changed: a bar in the gutter; a change kept, a badge
  // that shows it again.
  const bar = new (class extends GutterMarker {
    toDOM() { return el("span", "cm-agent-bar"); }
  })();
  class Badge extends GutterMarker {
    constructor(index, on) { super(); this.index = index; this.on = on; }
    eq(o) { return o.index === this.index; }
    toDOM() {
      const b = el("button", "cm-agent-badge", String(this.index + 1));
      b.type = "button";
      b.title = "The agent's change, kept: show it again";
      b.setAttribute("aria-label", `Show the agent's change ${this.index + 1} again`);
      b.addEventListener("mousedown", (ev) => ev.preventDefault());
      b.addEventListener("click", (ev) => { ev.preventDefault(); this.on(this.index, "reopen"); });
      return b;
    }
  }
  const setMarks = StateEffect.define();
  const mapLine = (tr, l) => {
    const doc = tr.startState.doc;
    if (l > doc.lines) return l;
    return tr.state.doc.lineAt(tr.changes.mapPos(doc.line(l).from)).number;
  };
  const marksField = StateField.define({
    create: () => ({ bars: [], badges: [] }),
    update(marks, tr) {
      for (const e of tr.effects) if (e.is(setMarks)) return e.value;
      if (!tr.docChanged) return marks;
      return {
        bars: marks.bars.map(([a, b]) => [mapLine(tr, a), mapLine(tr, b)]),
        badges: marks.badges.map((b) => ({ ...b, line: mapLine(tr, b.line) })),
      };
    },
  });
  const marksGutter = gutter({
    class: "cm-agent-gutter",
    markers(view) {
      const builder = new RangeSetBuilder();
      const doc = view.state.doc;
      const { bars, badges } = view.state.field(marksField);
      const at = new Map();
      for (const [a, b] of bars)
        for (let l = a; l <= b && l <= doc.lines; l++) at.set(l, bar);
      for (const b of badges)
        if (b.line <= doc.lines) at.set(b.line, b.marker);
      for (const l of [...at.keys()].sort((x, y) => x - y))
        builder.add(doc.line(l).from, doc.line(l).from, at.get(l));
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
    const makeExtensions = (lang) => [
      lineNumbers(), highlightActiveLineGutter(), highlightActiveLine(),
      drawSelection(), history(), bracketMatching(), indentOnInput(),
      highlightSelectionMatches(), indentUnit.of("  "),
      EditorState.tabSize.of(2),
      EditorView.lineWrapping, hanging, hunksField, marksField, marksGutter,
      syntaxHighlighting(style), language.of((languages[lang] || (() => []))()),
      // Mod-l and Mod-i are the page's: ask in the chat, ask here.
      keymap.of([...defaultKeymap.filter((b) => b.key !== "Mod-l" && b.key !== "Mod-i"),
                 ...historyKeymap, ...searchKeymap,
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
      state: EditorState.create({ doc: "", extensions: makeExtensions(null) }),
    });
    const onHunk = (i, what) => o.onHunk(i, what);
    const askState = {
      label: null, draft: "",
      on: (r) => {
        if (r.cancel) { askState.draft = ""; o.onAsk(false, ""); }
        else { o.onAsk(true, r.send); }
      },
    };

    // A quick sideways swipe on a touch screen: the next or previous file.
    // The text stays where the finger is until it is clearly sideways.
    let swipe = null;
    host.addEventListener("pointerdown", (ev) => {
      if (ev.pointerType !== "touch" || !ev.isPrimary) return;
      if (!view.state.selection.main.empty) return;
      if (document.querySelectorAll(".code-tab").length === 1) return;
      const target = ev.target && ev.target.nodeType === 1 ? ev.target : ev.target?.parentElement;
      if (target && target.closest(".cm-ask, .cm-agent-change, .cm-panels")) return;
      swipe = {
        pointerId: ev.pointerId, x: ev.clientX, y: ev.clientY,
        t: ev.timeStamp, dx: 0, sideways: false,
      };
    }, { passive: true });
    host.addEventListener("pointermove", (ev) => {
      if (!swipe || ev.pointerType !== "touch" || ev.pointerId !== swipe.pointerId) return;
      const dx = ev.clientX - swipe.x, dy = ev.clientY - swipe.y;
      if (!swipe.sideways && Math.abs(dx) > 16 && Math.abs(dx) > 2 * Math.abs(dy))
        swipe.sideways = true;
      else if (!swipe.sideways && Math.abs(dy) > 16) { swipe = null; return; }
      if (swipe.sideways) {
        swipe.dx = dx;
        view.dom.style.transform = `translateX(${dx * 0.35}px)`;
        view.dom.style.opacity = String(1 - Math.min(Math.abs(dx) / 600, 0.4));
      }
    }, { passive: true });
    const endSwipe = (ev) => {
      if (!swipe) return;
      if (ev.type !== "blur" && ev.pointerId !== swipe.pointerId) return;
      const s = swipe;
      swipe = null;
      view.dom.style.transform = "";
      view.dom.style.opacity = "";
      if (ev.type === "pointerup" && s.sideways && Math.abs(s.dx) > 64
          && ev.timeStamp - s.t < 800 && view.state.selection.main.empty)
        o.onSwipe(s.dx < 0 ? 1 : -1);
    };
    window.addEventListener("pointerup", endSwipe, { passive: true });
    window.addEventListener("pointercancel", endSwipe, { passive: true });
    window.addEventListener("blur", endSwipe);
    let lastTop = 0;
    view.scrollDOM.addEventListener("scroll", () => {
      const top = Math.max(0, view.scrollDOM.scrollTop);
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
                    setHunks.of(Decoration.none), setMarks.of({ bars: [], badges: [] })],
        });
        quiet = false;
      },
      // A new file: its own undo history and the top of it in view.
      load(text, lang) {
        flush();
        quiet = true;
        view.setState(EditorState.create({ doc: text, extensions: makeExtensions(lang) }));
        quiet = false;
        const [first, last, sel] = selection(view.state);
        o.onSelect(first, last, sel);
        view.scrollDOM.scrollTop = 0;
        lastTop = 0;
      },
      // The agent's changes on show, those kept, and the ask box.
      setChange(hunks, kept, ask) {
        if (ask) {
          if (askState.label !== ask.label) askState.draft = "";
          askState.label = ask.label;
        }
        const bars = [...hunks, ...kept].filter((h) => h.added.length)
          .map((h) => [h.line, h.line + h.added.length - 1]);
        const badges = kept.map((h, i) => ({ line: h.line, marker: new Badge(i, onHunk) }));
        view.dispatch({
          effects: [setHunks.of(hunkDecorations(view.state.doc, hunks, onHunk,
                      ask && { ...ask, state: askState })),
                    setMarks.of({ bars, badges })],
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
        if (pending) { cancelAnimationFrame(pending); pending = 0; }
        document.removeEventListener("keydown", before, true);
        document.removeEventListener("pointerdown", before, true);
        window.removeEventListener("pointerup", endSwipe);
        window.removeEventListener("pointercancel", endSwipe);
        window.removeEventListener("blur", endSwipe);
        view.destroy();
      },
    };
  }
  window.prighCodeEditor = { mount };
})();
