open! Core
open! Import

let name = "render_ui"

let description =
  {|Shows rich components inline in the user's chat in prigh-web: charts, tables, stats, images and more. Frontends render them from these arguments; you get a short acknowledgement. Only prigh-web displays them: on other clients the call fails, so follow the client notes and answer in text there.

Use this tool rarely. Prose and markdown are the default, and markdown tables are fine for tabular data. Only call it when the content is clearly a better experience as a component than as text: a chart of numeric data, an image or gallery of files, a diff, a large table worth sorting, a calculator. Usually call it at most once in a turn.

Never use it to ask the user anything: questions, choices, confirmations and collecting input go in prose, or through ask_user where it exists. Never use it for short answers, status updates, summaries, plans, code edits or lists that markdown handles, and never repeat a component's content in prose.

Arguments: {"components": [Component, ...]} (1-20 top level, 100 in all). Every component has "type" and may have "id" (unique in the call; required by form, choices and a checklist with submit). (md) strings take inline markdown. tone: neutral|info|success|warning|danger.

Layout:
- card {title?, subtitle?(md), children:[...]}
- grid {columns? 1-4 (default 2), children} (one column on phones)
- tabs {tabs:[{label, children}]} (2-8)
- accordion {items:[{title, children, open?}]} (1-20)
- divider {}
Nesting goes at most 4 levels deep, at most 20 children each.

Text and status:
- markdown {text} (full markdown)
- callout {tone?, title?, text(md)}
- stat {label, value (string|number), unit?, delta? ("+12%"), trend? up|down|flat, good? up|down (which trend is good), caption?(md), spark?:[number] (2-200)}
- kv {title?, items:[{key, value(md)}]} (1-50)
- progress {label?, value (shown clamped to 0-max), max? (default 100), tone?, caption?}
- badges {items:[{text, tone?}]} (1-30)

Data:
- table {title?, columns:[{key, label?, align? left|right|center, format? text|number|percent|currency|date|md, currency? (ISO, default USD), decimals? (0-10)}], rows:[{<key>: string|number|bool|null}], sortable? (default true), caption?} (at most 12 columns, 500 rows)
- chart {kind, title?, subtitle?, unit?, x_label?, y_label?, height? small|medium|large}
  kind line|area|bar|stacked_bar|horizontal_bar: labels:[string] (1-200), series:[{name, values:[number|null]}] (1-8, one value per label, null = gap)
  kind pie|donut: slices:[{label, value >= 0}] (1-12)
  kind scatter: series:[{name, points:[[x, y]]}] (1-8, 2000 points in all)
- timeline {title?, items:[{title, time?, text?(md), tone?}]} (1-50)
- steps {title?, items:[{title, text?(md), status? done|current|todo|failed}]} (1-50)
- code {lang?, title?, code}
- diff {title?, diff} (unified diff)
- tree {title?, entries:[{path ("src/a/b.ml"; directories end with "/"), note?(md), tone?}]} (1-500)

Media (files on the tool host; their bytes never reach you):
- image {path (absolute), caption?(md), alt?, width? small|medium|full} (png, jpeg, gif, webp or svg; at most 20 MB)
- gallery {title?, images:[{path, caption?, alt?}]} (1-24)

Interactive, in the browser only:
- calculator {title?, inputs:[{name (identifier), label, default, min?, max?, step?, unit?, slider?}] (1-20), outputs:[{label, expr, format? number|percent|currency|integer, decimals? (0-10), unit?}] (1-20)}
  expr: numbers, input names, + - * / ^ %, unary -, ( ), < <= > >= == != (1 or 0), c ? a : b, min(...) max(...) abs round floor ceil sqrt log (base 10) ln exp (one argument), pow(x, y).
- flashcards {title?, cards:[{front(md), back(md)}]} (1-50)
- quiz {title?, questions:[{question(md), options:[string] (2-6), answer (0-based index), explanation?(md)}]} (1-50)

Interactive, replying to you, only when the user explicitly asks for a form or an interactive input UI (the user's answer arrives as their next message, naming the component):
- form {id, title?, description?(md), fields:[{name, label, kind: text|textarea|number|select|radio|checkbox|toggle|slider|date, options? (select, radio, checkbox; 1-50), default?, min?, max?, step? (number and slider: numbers, a slider needs min and max; date: "YYYY-MM-DD" min and max, step in days), required?, placeholder?, help?}] (1-30), submit? (button label)}
- choices {id, prompt?(md), options:[{label, value?, description?, tone?}] (1-12), multiple?} (buttons)
- checklist {id?, title?, items:[{text(md), checked?}] (1-50), submit?} (with submit and id, sends the checked items)

Example:
{"components": [{"type": "chart", "kind": "line", "title": "Build time", "unit": "s", "labels": ["Mon", "Tue", "Wed"], "series": [{"name": "main", "values": [61, 58, 49]}]}, {"type": "table", "title": "Slowest tests", "columns": [{"key": "test"}, {"key": "secs", "label": "Seconds", "format": "number", "decimals": 1}], "rows": [{"test": "e2e", "secs": 31.4}]}]}|}
;;

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description
  ; parameters =
      `Object
        [ "type", `String "object"
        ; ( "properties"
          , `Object
              [ ( "components"
                , `Object
                    [ "type", `String "array"
                    ; "description", `String "The components, shown in order"
                    ; ( "items"
                      , `Object
                          [ "type", `String "object"
                          ; ( "properties"
                            , `Object
                                [ ( "type"
                                  , `Object
                                      [ "type", `String "string"
                                      ; ( "enum"
                                        , `Array
                                            (List.map
                                               Ui_components.types
                                               ~f:(fun t -> `String t)) )
                                      ] )
                                ; "id", `Object [ "type", `String "string" ]
                                ] )
                          ; "required", `Array [ `String "type" ]
                          ] )
                    ] )
              ] )
        ; "required", `Array [ `String "components" ]
        ]
  }
;;

let refusal client =
  sprintf
    "The user is on %s, which cannot display components: answer in text."
    (Client_kind.describe client)
;;

let head_length = 4096

(* What is wrong with the image at [path] ([field] in the arguments) on the
   tool host, if anything. *)
let image_problem (ui : Ui_host.t) (field, path) : Ui_host.Problem.t option =
  match ui.read_head path ~length:head_length with
  | Error (Host _ as problem) -> Some problem
  | Error (File why) ->
    Some
      (File
         (sprintf "%s: %s; give the absolute path of an image file" field why))
  | Ok (size, head) ->
    (match Image.sniff_displayable head with
     | None ->
       Some
         (File
            (sprintf
               "%s: %s is not an image (png, jpeg, gif, webp or svg); give an \
                image file"
               field
               path))
     | Some _ when size > Image.max_preview_bytes ->
       Some
         (File
            (sprintf
               "%s: %s is %.1f MB, over the 20 MB that are shown; make a \
                smaller copy and show that"
               field
               path
               (Float.of_int size /. 1e6)))
     | Some _ -> None)
;;

let errors problems =
  let sentence p =
    if String.is_suffix p ~suffix:"." || String.is_suffix p ~suffix:"?"
    then p
    else p ^ "."
  in
  Tool.Result.error
    (sprintf
       "Nothing was shown. Fix %s and call render_ui again:\n%s"
       (if List.length problems = 1 then "this" else "these")
       (String.concat
          ~sep:"\n"
          (List.map problems ~f:(fun p -> "- " ^ sentence p))))
;;

let ack (checked : Ui_components.Checked.t) =
  let shown =
    sprintf "Shown: %s." (String.concat ~sep:", " checked.summaries)
  in
  match checked.interactive with
  | [] -> shown
  | interactive ->
    sprintf
      "%s The user can answer through %s; an answer arrives as their next \
       message."
      shown
      (String.concat ~sep:", " interactive)
;;

(* Images are checked side by side, a few at a time. *)
let max_image_checks = 8

let run (context : Tool.Context.t) args =
  let client = context.ui.client () in
  if not (Client_kind.shows_components client)
  then Tool.Result.error (refusal client)
  else (
    match Ui_components.check args ~resolve:(Tool.resolve_path context) with
    | Error problems -> errors problems
    | Ok checked ->
      let problems =
        Fiber.List.filter_map
          ~max_fibers:max_image_checks
          (image_problem context.ui)
          checked.images
      in
      (match
         List.find_map problems ~f:(function
           | Host why -> Some why
           | File _ -> None)
       with
       | Some why ->
         Tool.Result.error
           (sprintf
              "Nothing was shown: cannot check the images (%s). Tell the user, \
               or answer without images."
              why)
       | None ->
         (match
            List.filter_map problems ~f:(function
              | File p -> Some p
              | Host _ -> None)
          with
          | [] -> Tool.Result.ok (ack checked)
          | problems -> errors problems)))
;;

let tool = { Tool.spec; run }
