// The render_ui calls ui.mjs's model makes: every component, in four calls.
export const dashboard = { components: [
  { type: "callout", tone: "info", title: "Build health", text: "Builds got **18% faster** since `dune 3.22`." },
  { type: "grid", columns: 3, children: [
    { type: "stat", label: "Median build", value: 42.5, unit: "s", delta: "-18%", trend: "down", good: "down", caption: "last 30 days", spark: [60, 58, 55, 51, 49, 47, 44, 42.5] },
    { type: "stat", label: "Pass rate", value: "97.2%", delta: "+1.4", trend: "up", good: "up" },
    { type: "stat", label: "Flaky tests", value: 3, trend: "flat" },
  ] },
  { type: "chart", kind: "line", title: "Build times by week", unit: "s", y_label: "seconds",
    labels: ["W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8"],
    series: [{ name: "main", values: [61, 58, null, 51, 47, 44, 45, 42] }, { name: "PRs", values: [70, 66, 63, 60, 55, 52, 50, 49] }] },
  { type: "grid", columns: 2, children: [
    { type: "chart", kind: "bar", title: "Failures by suite", height: "small", labels: ["backend", "tui", "web", "e2e"],
      series: [{ name: "this week", values: [2, 5, 1, 7] }, { name: "last week", values: [4, 3, 2, 9] }] },
    { type: "chart", kind: "donut", title: "Where the time goes", height: "small",
      slices: [{ label: "Compile", value: 24 }, { label: "Link", value: 9 }, { label: "Tests", value: 12 }, { label: "Nix", value: 5 }] },
  ] },
  { type: "progress", label: "Coverage", value: 78, tone: "success", caption: "target 80%" },
  { type: "table", id: "slow", title: "Slowest tests",
    columns: [{ key: "name", label: "Test" }, { key: "time", label: "Time (s)", format: "number", decimals: 1 },
      { key: "share", label: "Share", format: "percent" }, { key: "ok", label: "Passing" }],
    rows: [
      { name: "e2e/mobile", time: 31.2, share: 0.21, ok: true },
      { name: "tmux/agents", time: 18.04, share: 0.12, ok: false },
      { name: "web/test", time: 9.5, share: 0.064, ok: true },
      { name: "backend/rpc", time: 22.7, share: 0.15, ok: true },
    ] },
] };

export const charts = { components: [
  { type: "chart", kind: "area", title: "Memory over a run", unit: "MiB", labels: ["0s", "10s", "20s", "30s", "40s", "50s"],
    series: [{ name: "backend", values: [120, 180, 260, 240, 300, 280] }, { name: "browser", values: [300, 320, 410, 380, 390, 420] }] },
  { type: "grid", columns: 2, children: [
    { type: "chart", kind: "stacked_bar", title: "Tokens per turn", height: "small", labels: ["1", "2", "3", "4", "5"],
      series: [{ name: "input", values: [1200, 3400, 2800, 5100, 4300] }, { name: "output", values: [300, 900, 400, 1200, 800] }, { name: "cached", values: [0, 2000, 2500, 4000, 4100] }] },
    { type: "chart", kind: "horizontal_bar", title: "Cost by model", unit: "$", height: "small", labels: ["opus", "sonnet", "haiku"],
      series: [{ name: "cost", values: [4.2, 1.35, 0.22] }] },
    { type: "chart", kind: "pie", title: "Languages", height: "small",
      slices: [{ label: "OCaml", value: 82 }, { label: "Rust", value: 7 }, { label: "JS", value: 6 }, { label: "Nix", value: 3 }, { label: "Shell", value: 2 }] },
    { type: "chart", kind: "scatter", title: "Size vs build time", x_label: "lines", y_label: "s", height: "small",
      series: [{ name: "libs", points: [[1200, 3.1], [5400, 9.8], [800, 1.9], [9100, 15.2], [3000, 6.4]] },
        { name: "tests", points: [[2000, 7.5], [4100, 12.1], [600, 3.3]] }] },
  ] },
] };

export const learn = { components: [
  { type: "calculator", title: "Cache savings",
    inputs: [{ name: "builds", label: "Builds a day", default: 40, min: 0, max: 200, step: 1, slider: true },
      { name: "minutes", label: "Minutes saved", default: 1.5, unit: "min" }],
    outputs: [{ label: "Hours a month", expr: "builds * minutes * 30 / 60", decimals: 1, unit: "h" },
      { label: "Worth it", expr: "builds * minutes > 30 ? 1 : 0", format: "integer" }] },
  { type: "tabs", tabs: [
    { label: "Quiz", children: [{ type: "quiz", title: "OCaml", questions: [
      { question: "Which tool formats OCaml?", options: ["ocamlformat", "prettier", "gofmt"], answer: 0, explanation: "`ocamlformat`, with the project's `.ocamlformat`." },
      { question: "What does `Eio` use for concurrency?", options: ["threads", "effects"], answer: 1 }] }] },
    { label: "Flashcards", children: [{ type: "flashcards", title: "Terms", cards: [
      { front: "What is **Eio**?", back: "Effects-based direct-style IO for OCaml 5." },
      { front: "Bonsai?", back: "Incremental UI components." }] }] },
    { label: "Plan", children: [
      { type: "steps", items: [{ title: "Profile", status: "done" }, { title: "Cache deps", status: "current", text: "Use the `nix` binary cache" }, { title: "Split e2e", status: "todo" }, { title: "Old runner", status: "failed" }] },
      { type: "timeline", items: [{ title: "Started", time: "09:12" }, { title: "Regression found", time: "10:40", tone: "danger", text: "in the `tmux` tests" }, { title: "Fixed", time: "11:05", tone: "success" }] }] },
  ] },
  { type: "accordion", items: [
    { title: "The command", open: true, children: [{ type: "code", lang: "sh", title: "build", code: "nix develop -c dune build @runtest" }] },
    { title: "The change", children: [{ type: "diff", diff: "--- a/x.ml\n+++ b/x.ml\n@@ -1,2 +1,2 @@\n let x = 1\n-let y = 2\n+let y = 3\n" }] },
    { title: "Files", children: [{ type: "tree", entries: [{ path: "frontend/" }, { path: "frontend/web/lib/ui_view.ml", note: "new", tone: "success" }, { path: "backend/lib/tools/" }] }] },
  ] },
  { type: "kv", title: "Machine", items: [{ key: "CPU", value: "16 cores" }, { key: "OS", value: "NixOS 25.05" }] },
  { type: "badges", items: [{ text: "ocaml" }, { text: "passing", tone: "success" }, { text: "flaky", tone: "warning" }, { text: "blocked", tone: "danger" }] },
] };

export const answer = { components: [
  { type: "card", title: "Screenshots", subtitle: "before and after", children: [
    { type: "gallery", images: [{ path: "shots/before.png", caption: "Before" }, { path: "shots/after.png", caption: "After" }] }] },
  { type: "choices", id: "next", prompt: "What next?", options: [
    { label: "Cache deps", description: "the fastest win" }, { label: "Split e2e", description: "more parallel", tone: "warning" }] },
  { type: "form", id: "deploy", title: "Deploy options", description: "Where the build goes.", fields: [
    { name: "env", label: "Environment", kind: "select", options: ["staging", "prod"], default: "staging" },
    { name: "replicas", label: "Replicas", kind: "number", default: 2, min: 1, max: 9 },
    { name: "notify", label: "Notify the team", kind: "toggle", default: true },
    { name: "note", label: "Note", kind: "textarea", placeholder: "Optional" }], submit: "Deploy" },
  { type: "checklist", title: "Before release", items: [{ text: "Run `./test --full`", checked: true }, { text: "Update the docs" }] },
  { type: "divider" },
  { type: "markdown", text: "Pick one above, or fill in the form." },
] };
