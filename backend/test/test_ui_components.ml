open! Core
open! Prigh
module Json = Jsonaf

(* [render_ui]'s arguments as the model sends them: what the check accepts
   (the summaries, images to check and interactive components) or every
   problem it reports. *)
let check components =
  match
    Ui_components.check
      (Json.of_string (sprintf {|{"components": [%s]}|} components))
      ~resolve:(fun path -> Tool.resolve ~home:"/home/u" ~cwd:"/home/u/p" path)
  with
  | Ok checked -> print_s [%sexp (checked : Ui_components.Checked.t)]
  | Error problems -> List.iter problems ~f:print_endline
;;

let raw json =
  match
    Ui_components.check (Json.of_string json) ~resolve:(fun p -> "/abs/" ^ p)
  with
  | Ok checked -> print_s [%sexp (checked : Ui_components.Checked.t)]
  | Error problems -> List.iter problems ~f:print_endline
;;

let%expect_test "the arguments' shape" =
  raw {|{}|};
  raw {|{"components": []}|};
  raw {|{"components": {}}|};
  raw {|[]|};
  raw {|{"components": [{"type": "divider"}], "theme": "dark"}|};
  raw
    (sprintf
       {|{"components": [%s]}|}
       (String.concat
          ~sep:","
          (List.init 21 ~f:(fun _ -> {|{"type": "divider"}|}))));
  [%expect
    {|
    arguments.components: missing; render_ui needs it
    arguments.components: has none; give at least 1
    arguments.components: must be an array of components, not an object
    arguments: must be an object, not an array
    arguments.theme: render_ui does not take "theme"; it takes components
    arguments.components: has 21 components; give at most 20
    |}]
;;

let%expect_test "types and ids" =
  check
    {|{"type": "chrat"}, {"type": "sparkle"}, {"kind": "line"}, 5, {"type": 3}|};
  [%expect
    {|
    components[0].type: unknown type "chrat"; did you mean "chart"?
    components[1].type: unknown type "sparkle"; use one of card, grid, tabs, accordion, divider, markdown, callout, stat, kv, progress, badges, table, chart, timeline, steps, code, diff, tree, image, gallery, calculator, flashcards, quiz, form, choices or checklist
    components[2].type: missing; give one of card, grid, tabs, accordion, divider, markdown, callout, stat, kv, progress, badges, table, chart, timeline, steps, code, diff, tree, image, gallery, calculator, flashcards, quiz, form, choices or checklist
    components[3]: must be a component object ({"type": ...}), not a number
    components[4].type: must be a string, not a number; give one of card, grid, tabs, accordion, divider, markdown, callout, stat, kv, progress, badges, table, chart, timeline, steps, code, diff, tree, image, gallery, calculator, flashcards, quiz, form, choices or checklist
    |}];
  check
    {|{"type": "divider", "id": "a"},
      {"type": "card", "children": [{"type": "divider", "id": "a"}]},
      {"type": "divider", "id": " "},
      {"type": "divider", "colour": "red"}|};
  [%expect
    {|
    components[1].children[0].id: "a" is also components[0]'s id; ids must be unique within the call
    components[2].id: is empty; give a short name, e.g. "deploy"
    components[3].colour: divider does not take "colour"; it takes id
    |}]
;;

let%expect_test "layout" =
  check
    {|{"type": "card", "title": "Build", "subtitle": "*today*",
       "children": [{"type": "markdown", "text": "hi"}]},
      {"type": "grid", "columns": 3, "children": [{"type": "divider"}, {"type": "divider"}]},
      {"type": "tabs", "tabs": [{"label": "A", "children": [{"type": "divider"}]},
                                 {"label": "B", "children": [{"type": "divider"}]}]},
      {"type": "accordion", "items": [{"title": "More", "open": true,
                                       "children": [{"type": "divider"}]}]},
      {"type": "divider"}|};
  [%expect
    {|
    ((summaries
      ("card \"Build\"" "grid (2 components)" "tabs \"A, B\""
       "accordion (1 section)" divider))
     (images ()) (interactive ()))
    |}];
  check
    {|{"type": "card", "children": []},
      {"type": "card"},
      {"type": "grid", "columns": 5, "children": [{"type": "divider"}]},
      {"type": "grid", "columns": 1.5, "children": [{"type": "divider"}]},
      {"type": "tabs", "tabs": [{"label": "only", "children": [{"type": "divider"}]}]},
      {"type": "tabs", "tabs": [{"children": [{"type": "divider"}]}, {"label": "", "children": [], "icon": "x"}]},
      {"type": "accordion", "items": [{"title": "t", "open": "yes", "children": [{"type": "divider"}]}]}|};
  [%expect
    {|
    components[0].children: has none; give at least 1
    components[1].children: missing; card needs it
    components[2].columns: is 5; use 1 to 4
    components[3].columns: must be a whole number, not 1.5
    components[4].tabs: has 1 tab; give at least 2
    components[5].tabs[0].label: missing; a tab needs it
    components[5].tabs[1].label: is empty; give some text
    components[5].tabs[1].children: has none; give at least 1
    components[5].tabs[1].icon: a tab does not take "icon"; it takes label or children
    components[6].items[0].open: must be true or false, not a string
    |}];
  (* Four levels at most. *)
  let nest n =
    let rec go = function
      | 0 -> {|{"type": "divider"}|}
      | n -> sprintf {|{"type": "card", "children": [%s]}|} (go (n - 1))
    in
    go n
  in
  check (nest 3);
  check (nest 4);
  [%expect
    {|
    ((summaries (card)) (images ()) (interactive ()))
    components[0].children[0].children[0].children[0].children: nested too deep (at most 4 levels of components); flatten the layout
    |}];
  check
    (String.concat
       ~sep:","
       (List.init 6 ~f:(fun _ ->
          sprintf
            {|{"type": "grid", "children": [%s]}|}
            (String.concat
               ~sep:","
               (List.init 20 ~f:(fun _ -> {|{"type": "divider"}|}))))));
  [%expect
    {| components: 126 components in all; give at most 100 (split it into several calls or show less) |}]
;;

let%expect_test "text and status" =
  check
    {|{"type": "markdown", "text": "# Hi"},
      {"type": "callout", "tone": "warning", "title": "Careful", "text": "**x**"},
      {"type": "stat", "label": "Builds", "value": 42, "unit": "/day", "delta": "+12%",
       "trend": "up", "good": "up", "caption": "vs last week", "spark": [1, 2, 3]},
      {"type": "stat", "label": "Status", "value": "green"},
      {"type": "kv", "title": "Env", "items": [{"key": "os", "value": "linux"}]},
      {"type": "progress", "label": "Upload", "value": 30, "max": 40, "tone": "info", "caption": "3 of 4"},
      {"type": "badges", "items": [{"text": "ok", "tone": "success"}, {"text": "plain"}]}|};
  [%expect
    {|
    ((summaries
      (markdown "callout \"Careful\"" "stat \"Builds\"" "stat \"Status\""
       "kv \"Env\" (1 item)" "progress \"Upload\"" "badges (2 badges)"))
     (images ()) (interactive ()))
    |}];
  check
    {|{"type": "markdown"},
      {"type": "markdown", "text": "  "},
      {"type": "callout", "tone": "red", "text": "x"},
      {"type": "stat", "label": "x", "value": [1], "trend": "sideways", "good": "flat", "spark": [1]},
      {"type": "stat", "label": "x", "value": 1, "spark": [1, "2"]},
      {"type": "kv", "items": [{"key": "n", "value": 3}]},
      {"type": "progress", "value": 120},
      {"type": "progress", "value": 1, "max": 0},
      {"type": "progress", "value": -1, "max": 10},
      {"type": "badges", "items": []}|};
  [%expect
    {|
    components[0].text: missing; markdown needs it
    components[1].text: is empty; give some text
    components[2].tone: is "red"; use neutral, info, success, warning or danger
    components[3].value: must be a string or a number, not an array
    components[3].trend: is "sideways"; use up, down or flat
    components[3].good: is "flat"; use up or down
    components[3].spark: has 1 point; give at least 2
    components[4].spark[1]: must be a number, not a string
    components[5].items[0].value: must be a string, not a number: write "3"
    components[6].value: is 120, outside 0 to 100; give a value in that range (or set max)
    components[7].max: must be more than 0
    components[8].value: is -1, outside 0 to 10; give a value in that range (or set max)
    components[9].items: has none; give at least 1
    |}]
;;

let%expect_test "table" =
  check
    {|{"type": "table", "title": "Slowest tests", "sortable": false, "caption": "ms",
       "columns": [{"key": "name", "label": "Test"},
                   {"key": "ms", "align": "right", "format": "number", "decimals": 1},
                   {"key": "cost", "format": "currency", "currency": "EUR"}],
       "rows": [{"name": "a", "ms": 1.5, "cost": 2}, {"name": "b", "ms": null}]},
      {"type": "table", "columns": [{"key": "x"}], "rows": []}|};
  [%expect
    {|
    ((summaries ("table \"Slowest tests\" (2 rows)" "table (0 rows)"))
     (images ()) (interactive ()))
    |}];
  check
    {|{"type": "table",
       "columns": [{"key": "name"}, {"key": "name"}, {"label": "x"},
                   {"key": "a", "align": "middle", "format": "money", "currency": "dollars", "decimals": 12}],
       "rows": [{"name": "a", "nmae": "b"}, {"name": {"x": 1}}, ["a"], {"zzz": 1}]},
      {"type": "table", "columns": [], "rows": [{}]}|};
  [%expect
    {|
    components[0].columns[1].key: "name" is also components[0].columns[0]'s key; give each column its own
    components[0].columns[2].key: missing; a table column needs it
    components[0].columns[3].align: is "middle"; use left, right or center
    components[0].columns[3].format: is "money"; use text, number, percent, currency, date or md
    components[0].columns[3].currency: "dollars" is not an ISO currency code; write e.g. "USD" or "EUR"
    components[0].columns[3].decimals: is 12; use 0 to 10
    components[0].rows[0].nmae: no column has key "nmae"; did you mean "name"?
    components[0].rows[1].name: must be a string, number, boolean or null, not an object
    components[0].rows[2]: must be an object of cells by column key, not an array
    components[0].rows[3].zzz: no column has key "zzz"; add a column for it or use one of "a" or "name"
    components[1].columns: has none; give at least 1
    |}];
  check
    (sprintf
       {|{"type": "table", "columns": [%s], "rows": [%s]}|}
       (String.concat
          ~sep:","
          (List.init 13 ~f:(fun i -> sprintf {|{"key": "c%d"}|} i)))
       (String.concat ~sep:"," (List.init 501 ~f:(fun _ -> "{}"))));
  [%expect
    {|
    components[0].columns: has 13 columns; give at most 12
    components[0].rows: has 501 rows; give at most 500
    |}]
;;

let%expect_test "chart" =
  check
    {|{"type": "chart", "kind": "line", "title": "Build times", "unit": "s",
       "x_label": "week", "y_label": "time", "height": "small", "subtitle": "CI",
       "labels": ["w1", "w2", "w3"],
       "series": [{"name": "main", "values": [1, null, 3]}, {"name": "pr", "values": [2, 2, 2]}]},
      {"type": "chart", "kind": "bar", "labels": ["a"], "series": [{"name": "n", "values": [1]}]},
      {"type": "chart", "kind": "donut", "slices": [{"label": "a", "value": 0}, {"label": "b", "value": 2}]},
      {"type": "chart", "kind": "scatter", "series": [{"name": "s", "points": [[1, 2], [3.5, -4]]}]}|};
  [%expect
    {|
    ((summaries ("chart \"Build times\" (2 series)" chart chart chart))
     (images ()) (interactive ()))
    |}];
  check
    {|{"type": "chart", "kind": "line", "labels": ["a", "b"],
       "series": [{"name": "x", "values": [1]}, {"name": "y", "values": [1, "2"]}, {"values": 3}]},
      {"type": "chart", "kind": "line", "labels": [1, 2], "series": [{"name": "x", "values": [1, 2], "points": []}], "slices": []},
      {"type": "chart", "kind": "pie", "labels": ["a"], "slices": [{"label": "a", "value": -1}]},
      {"type": "chart", "kind": "pie", "slices": [{"label": "a", "value": 0}]},
      {"type": "chart", "kind": "scatter", "labels": [], "series": [{"name": "s", "points": [[1], 2, [1, "x"]], "values": []}]},
      {"type": "chart", "kind": "radar"},
      {"type": "chart", "kind": "line", "height": "huge", "labels": [], "series": []}|};
  [%expect
    {|
    components[0].series[0].values: 1 value for 2 labels; give one per label (null for a gap)
    components[0].series[1].values[1]: must be a number, not a string
    components[0].series[2].name: missing; a chart series needs it
    components[0].series[2].values: must be an array of numbers, not a number
    components[1].slices: a line chart does not take slices; it takes labels and series
    components[1].labels[0]: must be a string, not a number: write "1"
    components[1].labels[1]: must be a string, not a number: write "2"
    components[1].series[0].points: a line chart does not take points; it takes values (one per label)
    components[2].labels: a pie chart does not take labels; it takes slices
    components[2].slices[0].value: is -1; slices cannot be negative
    components[3].slices: are all 0; give the slices' sizes
    components[4].labels: a scatter chart does not take labels; it takes series of points
    components[4].series[0].values: a scatter chart does not take values; it takes points ([[x, y], ...])
    components[4].series[0].points[0]: must be an [x, y] pair of numbers, not an array of 1
    components[4].series[0].points[1]: must be an [x, y] pair of numbers, not a number
    components[4].series[0].points[2][1]: must be a number, not a string
    components[5].kind: is "radar"; use line, area, bar, stacked_bar, horizontal_bar, pie, donut or scatter
    components[6].height: is "huge"; use small, medium or large
    components[6].labels: has none; give at least 1
    components[6].series: has none; give at least 1
    |}];
  let points n = String.concat ~sep:"," (List.init n ~f:(fun _ -> "[0, 0]")) in
  check
    (sprintf
       {|{"type": "chart", "kind": "scatter", "series": [{"name": "a", "points": [%s]}, {"name": "b", "points": [%s]}]}|}
       (points 1500)
       (points 600));
  [%expect
    {| components[0].series: has 2100 points in all; give at most 2000 (sample them) |}]
;;

let%expect_test "timeline, steps, code, diff, tree" =
  check
    {|{"type": "timeline", "title": "Deploys", "items": [{"title": "v1", "time": "Mon", "text": "*ok*", "tone": "success"}]},
      {"type": "steps", "title": "Plan", "items": [{"title": "a", "status": "done"}, {"title": "b", "text": "x", "status": "current"}]},
      {"type": "code", "lang": "ocaml", "title": "main.ml", "code": "let () = ()"},
      {"type": "diff", "title": "fix", "diff": "--- a\n+++ b\n"},
      {"type": "tree", "title": "src", "entries": [{"path": "src/"}, {"path": "src/a.ml", "note": "new", "tone": "success"}]}|};
  [%expect
    {|
    ((summaries
      ("timeline \"Deploys\" (1 item)" "steps \"Plan\" (2 steps)"
       "code \"main.ml\"" "diff \"fix\"" "tree \"src\" (2 entries)"))
     (images ()) (interactive ()))
    |}];
  check
    {|{"type": "timeline", "items": [{"time": "Mon"}]},
      {"type": "steps", "items": [{"title": "a", "status": "doing"}]},
      {"type": "code", "code": ""},
      {"type": "diff"},
      {"type": "tree", "entries": [{"path": "/etc/x"}, {"path": "a//b"}, {"path": ""}]}|};
  [%expect
    {|
    components[0].items[0].title: missing; a timeline item needs it
    components[1].items[0].status: is "doing"; use done, current, todo or failed
    components[2].code: is empty; give some text
    components[3].diff: missing; diff needs it
    components[4].entries[0].path: "/etc/x" starts with "/"; give paths relative to the tree's root, e.g. "src/main.ml"
    components[4].entries[1].path: "a//b" has an empty segment; separate names with one "/"
    components[4].entries[2].path: is empty; give some text
    |}]
;;

let%expect_test
    "images: absolute paths go to the host check, relative ones say which to \
     use"
  =
  check
    {|{"type": "image", "path": "/home/u/p/shot.png", "caption": "it", "alt": "a shot", "width": "medium"},
      {"type": "gallery", "title": "Shots", "images": [{"path": "/tmp/a.png", "caption": "a"}, {"path": "/tmp/b.gif", "alt": "b"}]}|};
  [%expect
    {|
    ((summaries ("image /home/u/p/shot.png" "gallery \"Shots\" (2 images)"))
     (images
      ((components[0].path /home/u/p/shot.png)
       (components[1].images[0].path /tmp/a.png)
       (components[1].images[1].path /tmp/b.gif)))
     (interactive ()))
    |}];
  check
    {|{"type": "image", "path": "shot.png", "width": "huge"},
      {"type": "image", "path": "~/pics/x.png"},
      {"type": "image"},
      {"type": "gallery", "images": [{"path": "../a.png"}]},
      {"type": "gallery", "images": []}|};
  [%expect
    {|
    components[0].path: "shot.png" is relative; give the absolute path: use /home/u/p/shot.png
    components[0].width: is "huge"; use small, medium or full
    components[1].path: "~/pics/x.png" is relative; give the absolute path: use /home/u/pics/x.png
    components[2].path: missing; image needs it
    components[3].images[0].path: "../a.png" is relative; give the absolute path: use /home/u/p/../a.png
    components[4].images: has none; give at least 1
    |}]
;;

let%expect_test "calculator" =
  check
    {|{"type": "calculator", "title": "Mortgage",
       "inputs": [{"name": "principal", "label": "Loan", "default": 300000, "min": 0, "max": 1000000, "step": 1000, "unit": "$", "slider": true},
                  {"name": "rate", "label": "Rate", "default": 5},
                  {"name": "years", "label": "Years", "default": 30}],
       "outputs": [{"label": "Monthly", "format": "currency", "decimals": 2,
                    "expr": "principal * (rate/1200) / (1 - pow(1 + rate/1200, -years*12))"},
                   {"label": "Big?", "expr": "principal > 500000 ? 1 : 0", "format": "integer"},
                   {"label": "Odd", "expr": "-2^2 + max(1, 2, 3) % 2 + round(sqrt(abs(-4))) + 1e3 + .5 != 3", "unit": "x"}]}|};
  [%expect
    {|
    ((summaries ("calculator \"Mortgage\"")) (images ()) (interactive ()))
    |}];
  check
    {|{"type": "calculator",
       "inputs": [{"name": "1x", "label": "a", "default": 1},
                  {"name": "max", "label": "b", "default": 1},
                  {"name": "a", "label": "c", "default": 1},
                  {"name": "a", "label": "d", "default": 1},
                  {"name": "s", "label": "e", "default": 1, "slider": true},
                  {"name": "r", "label": "f", "default": 11, "min": 0, "max": 10, "step": 0},
                  {"name": "q", "label": "g", "default": 1, "min": 5, "max": 5},
                  {"name": "n", "label": "h"}],
       "outputs": [{"label": "o1", "expr": "a +"},
                   {"label": "o2", "expr": "rat * 2"},
                   {"label": "o3", "expr": "avg(a)"},
                   {"label": "o4", "expr": "pow(a) + sqrt(a, a) + min()"},
                   {"label": "o5", "expr": "(a"},
                   {"label": "o6", "expr": "a = 1"},
                   {"label": "o7", "expr": "a $ 1"},
                   {"label": "o8", "expr": ""},
                   {"label": "o9", "expr": "a b"},
                   {"label": "o10", "expr": "a ? 1"},
                   {"label": "o11", "expr": "sqrt + 1", "format": "money", "decimals": -1}]}|};
  [%expect
    {|
    components[0].inputs[0].name: "1x" is not an identifier; use letters, digits and _, e.g. "monthly_rate"
    components[0].inputs[1].name: "max" is a function's name; call the input something else
    components[0].inputs[3].name: "a" is also components[0].inputs[2]'s name; give each input its own
    components[0].inputs[4]: a slider needs min and max
    components[0].inputs[5].step: must be more than 0
    components[0].inputs[5].default: is 11, outside 0 to 10
    components[0].inputs[6].max: is 5, not more than min 5
    components[0].inputs[7].default: missing; a calculator input needs it
    components[0].outputs[0].expr: expected a value at the end in "a +"
    components[0].outputs[1].expr: unknown name "rat"; the inputs are max, a, a, s, r, q, n
    components[0].outputs[2].expr: unknown function "avg"; the functions are min, max, abs, round, floor, ceil, sqrt, pow, log, ln, exp
    components[0].outputs[3].expr: pow takes 2 arguments, not 1
    components[0].outputs[3].expr: sqrt takes 1 argument, not 2
    components[0].outputs[3].expr: min takes at least 1 argument
    components[0].outputs[4].expr: expected ")" at column 3, found the end in "(a"
    components[0].outputs[5].expr: "=" at column 3: compare with "==" in "a = 1"
    components[0].outputs[6].expr: unexpected "$" at column 3 in "a $ 1"
    components[0].outputs[7].expr: is empty; give some text
    components[0].outputs[8].expr: unexpected "b" at column 3 in "a b"
    components[0].outputs[9].expr: expected ":" at column 6, found the end in "a ? 1"
    components[0].outputs[10].format: is "money"; use number, percent, currency or integer
    components[0].outputs[10].decimals: is -1; use 0 to 10
    components[0].outputs[10].expr: sqrt is a function: call it, e.g. sqrt(x)
    |}]
;;

let%expect_test "calculator expressions parse with the documented precedence" =
  List.iter
    [ "1 + 2 * 3"
    ; "-2^2"
    ; "2^3^2"
    ; "a < b == 1"
    ; "a ? b : c ? d : e"
    ; "10 % 3 - -1"
    ; "min(1, 2) * (3 + 4)"
    ; "1.5e-3"
    ]
    ~f:(fun s ->
      match Ui_expr.parse s with
      | Ok e -> print_s [%sexp (s : string), (e : Ui_expr.t)]
      | Error e -> print_endline e);
  [%expect
    {|
    ("1 + 2 * 3" (Binary + (Number 1) (Binary * (Number 2) (Number 3))))
    (-2^2 (Neg (Binary ^ (Number 2) (Number 2))))
    (2^3^2 (Binary ^ (Number 2) (Binary ^ (Number 3) (Number 2))))
    ("a < b == 1" (Binary == (Binary < (Name a) (Name b)) (Number 1)))
    ("a ? b : c ? d : e"
     (Cond (Name a) (Name b) (Cond (Name c) (Name d) (Name e))))
    ("10 % 3 - -1" (Binary - (Binary % (Number 10) (Number 3)) (Neg (Number 1))))
    ("min(1, 2) * (3 + 4)"
     (Binary * (Call min ((Number 1) (Number 2)))
      (Binary + (Number 3) (Number 4))))
    (1.5e-3 (Number 0.0015))
    |}]
;;

let%expect_test "flashcards and quiz" =
  check
    {|{"type": "flashcards", "title": "Verbs", "cards": [{"front": "ser", "back": "to be"}]},
      {"type": "quiz", "title": "Check", "questions": [{"question": "2+2?", "options": ["3", "4"], "answer": 1, "explanation": "math"}]}|};
  [%expect
    {|
    ((summaries ("flashcards \"Verbs\" (1 card)" "quiz \"Check\" (1 question)"))
     (images ()) (interactive ()))
    |}];
  check
    {|{"type": "flashcards", "cards": [{"front": "x"}]},
      {"type": "quiz", "questions": [{"question": "q", "options": ["a"], "answer": 0},
                                     {"question": "q", "options": ["a", "b"], "answer": 2},
                                     {"question": "q", "options": ["a", "b"], "answer": 0.5},
                                     {"question": "q", "options": ["a", "b", "c", "d", "e", "f", "g"], "answer": 0}]}|};
  [%expect
    {|
    components[0].cards[0].back: missing; a flashcard needs it
    components[1].questions[0].options: has 1 option; give at least 2
    components[1].questions[1].answer: is 2, but there are 2 options; give the right option's index from 0 to 1
    components[1].questions[2].answer: must be a whole number, not 0.5
    components[1].questions[3].options: has 7 options; give at most 6
    |}]
;;

let%expect_test "form" =
  check
    {|{"type": "form", "id": "deploy", "title": "Deploy options", "description": "*where*", "submit": "Go",
       "fields": [{"name": "env", "label": "Env", "kind": "select", "options": ["dev", "prod"], "default": "prod", "required": true},
                  {"name": "replicas", "label": "Replicas", "kind": "number", "min": 1, "max": 10, "step": 1, "default": 3},
                  {"name": "note", "label": "Note", "kind": "textarea", "placeholder": "why", "help": "optional"},
                  {"name": "when", "label": "When", "kind": "date", "default": "2026-11-06"},
                  {"name": "flags", "label": "Flags", "kind": "checkbox", "options": ["a", "b"], "default": ["b"]},
                  {"name": "fast", "label": "Fast", "kind": "toggle", "default": true},
                  {"name": "pct", "label": "Pct", "kind": "slider", "min": 0, "max": 100},
                  {"name": "how", "label": "How", "kind": "radio", "options": ["x", "y"]},
                  {"name": "who", "label": "Who", "kind": "text", "default": "me"}]}|};
  [%expect
    {|
    ((summaries ("form \"Deploy options\"")) (images ())
     (interactive ("form \"Deploy options\"")))
    |}];
  check
    {|{"type": "form", "fields": []},
      {"type": "form", "id": "f",
       "fields": [{"name": "a", "label": "A", "kind": "dropdown"},
                  {"name": "a", "label": "B", "kind": "select"},
                  {"name": "c", "label": "C", "kind": "text", "options": ["x"], "min": 1},
                  {"name": "d", "label": "D", "kind": "slider", "min": 3},
                  {"name": "e", "label": "E", "kind": "number", "min": 5, "max": 1, "default": "3"},
                  {"name": "f", "label": "F", "kind": "select", "options": ["x"], "default": "y"},
                  {"name": "g", "label": "G", "kind": "checkbox", "options": ["x"], "default": ["x", "z"]},
                  {"name": "h", "label": "H", "kind": "toggle", "default": "yes"},
                  {"name": "i", "label": "I", "kind": "date", "default": "2026-13-01"},
                  {"name": "j", "label": "J", "kind": "number", "min": 0, "max": 5, "default": 9},
                  {"label": "K", "kind": "text", "required": "yes"}]}|};
  [%expect
    {|
    components[0].id: missing; a form needs an id (unique in the call) to reply
    components[0].fields: has none; give at least 1
    components[1].fields[0].kind: is "dropdown"; use text, textarea, number, select, radio, checkbox, toggle, slider or date
    components[1].fields[1].name: "a" is also components[1].fields[0]'s name; give each field its own
    components[1].fields[1].options: missing; a form field needs it
    components[1].fields[2].options: a text field has no options; use kind select, radio or checkbox
    components[1].fields[2].min: only number and slider fields take min
    components[1].fields[3]: a slider needs min and max
    components[1].fields[4].max: is 1, not more than min 5
    components[1].fields[4].default: a number field's default must be a number, not a string
    components[1].fields[5].default: "y" is not one of the options ("x")
    components[1].fields[6].default: "z" is not one of the options ("x")
    components[1].fields[7].default: a toggle field's default must be true or false, not a string
    components[1].fields[8].default: "2026-13-01" is not a date; write YYYY-MM-DD
    components[1].fields[9].default: is 9, outside min to max
    components[1].fields[10].name: missing; a form field needs it
    components[1].fields[10].required: must be true or false, not a string
    |}]
;;

let%expect_test "choices and checklist" =
  check
    {|{"type": "choices", "id": "next", "prompt": "Which first?", "multiple": true,
       "options": [{"label": "e2e", "description": "slow", "tone": "warning"}, {"label": "Unit", "value": "unit"}]},
      {"type": "checklist", "title": "Todo", "items": [{"text": "a", "checked": true}, {"text": "b"}]},
      {"type": "checklist", "id": "pick", "title": "Pick", "submit": "Send", "items": [{"text": "a"}]},
      {"type": "choices", "id": "bare", "options": [{"label": "x"}]}|};
  [%expect
    {|
    ((summaries
      ("choices \"Which first?\"" "checklist \"Todo\" (2 items)"
       "checklist \"Pick\" (1 item)" "choices \"bare\""))
     (images ())
     (interactive
      ("choices \"Which first?\"" "checklist \"Pick\" (1 item)"
       "choices \"bare\"")))
    |}];
  check
    {|{"type": "choices", "options": [{"label": "a"}, {"label": "b", "value": "a"}]},
      {"type": "choices", "id": "c", "options": []},
      {"type": "checklist", "submit": "Send", "items": [{"text": "a", "checked": 1}]}|};
  [%expect
    {|
    components[0].id: missing; choices needs an id (unique in the call) to reply
    components[0].options[1]: sends "a" like components[0].options[0]; give each option its own value
    components[1].options: has none; give at least 1
    components[2].items[0].checked: must be true or false, not a number
    components[2].id: missing; a checklist with submit needs an id (unique in the call) to reply
    |}]
;;

let%expect_test "ids, nested ones too" =
  print_s
    [%sexp
      (Ui_components.ids
         (Json.of_string
            {|{"components": [
                {"type": "form", "id": "a"},
                {"type": "card", "children": [{"type": "choices", "id": "b"}]},
                {"type": "tabs", "tabs": [{"label": "t", "children": [{"type": "checklist", "id": "c"}]}]},
                {"type": "accordion", "items": [{"title": "t", "children": [{"type": "divider", "id": "d"}]}]}]}|})
       : string list)];
  [%expect {| (a b c d) |}]
;;
