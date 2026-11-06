open! Core
open! Import
module C = Ui_spec.Component

let cache : (string * Ui_spec.t option) String.Table.t = String.Table.create ()

let spec (call : Tool_call.t) =
  match Hashtbl.find cache call.id with
  | Some (args, spec) when phys_equal args call.arguments || String.equal args call.arguments -> spec
  | _ ->
    let spec = Ui_spec.of_arguments call.arguments in
    Hashtbl.set cache ~key:call.id ~data:(call.arguments, spec);
    spec
;;

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let text cls s = Node.span ~attrs:[ Attr.class_ cls ] [ Node.text s ]

let title_node = function
  | Some t -> Node.div ~attrs:[ Attr.class_ "ui-title" ] [ Node.text t ]
  | None -> Node.none
;;

let tone_cls (t : Ui_spec.Tone.t) = "tone-" ^ Ui_spec.Tone.to_string t

let scalar (s : Ui_spec.Scalar.t) =
  match s with
  | Text s -> s
  | Number f -> Ui_format.number f
;;

let spark values =
  match values with
  | [] | [ _ ] -> Node.none
  | values ->
    let lo = List.fold values ~init:Float.infinity ~f:Float.min in
    let hi = List.fold values ~init:Float.neg_infinity ~f:Float.max in
    let span = if Float.(hi -. lo < 1e-12) then 1. else hi -. lo in
    let n = List.length values in
    let w = 100. and h = 32. in
    let pts =
      List.mapi values ~f:(fun i v ->
        Some
          ( Float.of_int i *. w /. Float.of_int (n - 1)
          , h -. 2. -. ((v -. lo) /. span *. (h -. 4.)) ))
    in
    let line = Ui_svg.path_of_points pts in
    Ui_svg.svg
      ~cls:"ui-spark"
      ~width:w
      ~height:h
      [ Ui_svg.el "path" [ "class", "ui-spark-area"; "d", sprintf "%s L %s %s L 0 %s Z" line (Ui_svg.num w) (Ui_svg.num h) (Ui_svg.num h) ] []
      ; Ui_svg.el "path" [ "class", "ui-spark-line"; "d", line; "vector-effect", "non-scaling-stroke" ] []
      ]
;;

let stat ~label ~value ~unit ~delta ~trend ~good ~caption ~spark:values =
  let trend : Ui_spec.Trend.t option =
    match trend, delta with
    | Some t, _ -> Some t
    | None, Some (Ui_spec.Scalar.Number f) ->
      Some (if Float.(f > 0.) then Up else if Float.(f < 0.) then Down else Flat)
    | None, Some (Text s) ->
      (match String.lstrip s with
       | s when String.is_prefix s ~prefix:"+" -> Some Up
       | s when String.is_prefix s ~prefix:"-" || String.is_prefix s ~prefix:"−" -> Some Down
       | _ -> None)
    | None, None -> None
  in
  let mood =
    match (trend : Ui_spec.Trend.t option), (good : Ui_spec.Trend.t option) with
    | Some Flat, _ | None, _ | _, None | _, Some Flat -> "neutral"
    | Some t, Some g -> if Ui_spec.Trend.equal t g then "good" else "bad"
  in
  let arrow =
    match trend with
    | Some Up -> "↑"
    | Some Down -> "↓"
    | Some Flat -> "→"
    | None -> ""
  in
  div
    "ui-stat"
    [ div "ui-stat-label" [ Node.text label ]
    ; div
        "ui-stat-row"
        [ Node.span
            ~attrs:[ Attr.class_ "ui-stat-value" ]
            [ Node.text (scalar value)
            ; (match unit with
               | Some u -> text "ui-stat-unit" u
               | None -> Node.none)
            ]
        ; (match delta with
           | Some d ->
             Node.span
               ~attrs:[ Attr.classes [ "ui-delta"; mood ] ]
               [ Node.text (String.strip (arrow ^ " " ^ scalar d)) ]
           | None -> Node.none)
        ]
    ; spark values
    ; (match caption with
       | Some c -> Ui_context.md ~cls:"ui-stat-caption" c
       | None -> Node.none)
    ]
;;

let progress ~label ~value ~max ~tone ~caption =
  let pct = Float.clamp_exn (value /. max) ~min:0. ~max:1. in
  div
    ("ui-progress " ^ tone_cls tone)
    [ div
        "ui-progress-head"
        [ (match label with
           | Some l -> text "ui-progress-label" l
           | None -> Node.none)
        ; text
            "ui-progress-value"
            (if Float.equal max 100.
             then Ui_format.number ~decimals:0 value ^ "%"
             else sprintf "%s / %s" (Ui_format.number value) (Ui_format.number max))
        ]
    ; Node.div
        ~attrs:
          [ Attr.class_ "ui-bar"
          ; Attr.role "progressbar"
          ; Attr.create "aria-valuemin" "0"
          ; Attr.create "aria-valuemax" (Ui_svg.num max)
          ; Attr.create "aria-valuenow" (Ui_svg.num value)
          ; Attr.create "aria-label" (Option.value label ~default:"Progress")
          ]
        [ Node.div
            ~attrs:
              [ Attr.class_ "ui-bar-fill"
              ; Attr.style (Css_gen.create ~field:"width" ~value:(sprintf "%.2f%%" (pct *. 100.)))
              ]
            []
        ]
    ; (match caption with
       | Some c -> Ui_context.md ~cls:"ui-caption" c
       | None -> Node.none)
    ]
;;

let timeline ~title (items : Ui_spec.Item.t list) =
  div
    "ui-timeline"
    [ title_node title
    ; Node.ol
        (List.map items ~f:(fun i ->
           Node.li
             ~attrs:[ Attr.classes [ "ui-event"; tone_cls i.tone ] ]
             [ Node.span ~attrs:[ Attr.class_ "ui-dot"; Attr.create "aria-hidden" "true" ] []
             ; div
                 "ui-event-body"
                 [ div
                     "ui-event-head"
                     [ text "ui-event-title" i.title
                     ; (match i.time with
                        | Some t -> text "ui-time" t
                        | None -> Node.none)
                     ]
                 ; (match i.text with
                    | Some t -> Ui_context.md ~cls:"ui-event-text" t
                    | None -> Node.none)
                 ]
             ]))
    ]
;;

let steps ~title items =
  div
    "ui-steps"
    [ title_node title
    ; Node.ol
        (List.map items ~f:(fun ((i : Ui_spec.Item.t), (status : Ui_spec.Step_status.t)) ->
           let cls, mark =
             match status with
             | Done -> "done", Html.mark ~cls:"done" ~label:"Done" Check
             | Current -> "now", Html.mark ~cls:"now" ~label:"In progress" Next
             | Failed -> "failed", Html.mark ~cls:"failed" ~label:"Failed" Cross
             | Todo -> "todo", Html.mark ~cls:"todo" ~label:"To do" Ring
           in
           Node.li
             ~attrs:
               ([ Attr.class_ ("ui-step " ^ cls) ]
                @ match status with Current -> [ Attr.create "aria-current" "step" ] | _ -> [])
             [ mark
             ; div
                 "ui-step-body"
                 [ text "ui-step-title" i.title
                 ; (match i.text with
                    | Some t -> Ui_context.md ~cls:"ui-step-text" t
                    | None -> Node.none)
                 ]
             ]))
    ]
;;

let fence code =
  let longest =
    String.split_lines code
    |> List.map ~f:(fun l ->
      let l = String.strip l in
      if String.is_prefix l ~prefix:"```"
      then String.length (String.take_while l ~f:(Char.equal '`'))
      else 0)
    |> List.fold ~init:2 ~f:Int.max
  in
  String.make (longest + 1) '`'
;;

let code ~lang ~title code =
  let f = fence code in
  div
    "ui-code"
    [ title_node title
    ; Markdown_view.render (sprintf "%s%s\n%s\n%s" f (Option.value lang ~default:"") code f)
    ]
;;

let tree ~title entries =
  let rows =
    Ui_tree.rows (List.map entries ~f:(fun (path, note, tone) -> path, (note, tone)))
    |> List.map ~f:(fun (r : _ Ui_tree.Row.t) ->
      let note, tone = Option.value r.info ~default:(None, Ui_spec.Tone.Neutral) in
      Node.li
        ~attrs:
          [ Attr.classes [ "ui-tree-row"; tone_cls tone; (if r.dir then "dir" else "file") ]
          ; Ui_style.attr [ "--depth", Int.to_string r.depth ]
          ; Attr.title r.path
          ]
        [ Node.span
            ~attrs:[ Attr.class_ "ui-tree-icon"; Attr.create "aria-hidden" "true" ]
            [ (if r.dir then Html.Icon.view Folder else Node.none) ]
        ; text "ui-tree-name" r.name
        ; (match note with
           | Some n -> Ui_context.md ~cls:"ui-tree-note" n
           | None -> Node.none)
        ])
  in
  div "ui-tree" [ title_node title; Node.ul rows ]
;;

let unknown type_ =
  div
    "ui-unknown"
    [ Node.text
        (if String.is_empty type_
         then "A component without a type"
         else sprintf "A %S component this page can't show: update prigh-web" type_)
    ]
;;

let rec component (ctx : Ui_context.t) ~path (c : C.t) =
  let children p cs = List.mapi cs ~f:(fun k c -> component ctx ~path:(sprintf "%s.%d" p k) c) in
  let reply_id = Option.value c.id ~default:path in
  let reply_title = Option.value (C.title c) ~default:reply_id in
  let node =
    match c.body with
    | Card { title; subtitle; children = cs } ->
      Node.section
        ~attrs:[ Attr.class_ "ui-card" ]
        [ (match title, subtitle with
           | None, None -> Node.none
           | _ ->
             Node.header
               [ (match title with
                  | Some t -> Node.h3 ~attrs:[ Attr.class_ "ui-card-title" ] [ Node.text t ]
                  | None -> Node.none)
               ; (match subtitle with
                  | Some s -> Ui_context.md ~cls:"ui-sub" s
                  | None -> Node.none)
               ])
        ; div "ui-stack" (children path cs)
        ]
    | Grid { columns; children = cs } ->
      Node.div
        ~attrs:
          [ Attr.class_ "ui-grid"
          ; Ui_style.attr [ "--cols", Int.to_string columns ]
          ]
        (children path cs)
    | Tabs tabs ->
      let n = List.length tabs in
      let selected =
        Option.bind (Ui_context.get ctx ~path "tab") ~f:Int.of_string_opt
        |> Option.value ~default:0
        |> Int.clamp_exn ~min:0 ~max:(Int.max 0 (n - 1))
      in
      let id k = sprintf "ui-%s-%s-tab%d" ctx.call_id path k in
      let pick k = Ui_context.set ctx ~path "tab" (Int.to_string k) in
      div
        "ui-tabs"
        [ Node.div
            ~attrs:[ Attr.class_ "ui-tablist"; Attr.role "tablist" ]
            (List.mapi tabs ~f:(fun k (label, _) ->
               let on = k = selected in
               Node.button
                 ~attrs:
                   [ Attr.type_ "button"
                   ; Attr.id (id k)
                   ; Attr.classes ([ "ui-tab" ] @ if on then [ "on" ] else [])
                   ; Attr.role "tab"
                   ; Attr.create "aria-selected" (Bool.to_string on)
                   ; Attr.create "aria-controls" (id k ^ "-panel")
                   ; Attr.tabindex (if on then 0 else -1)
                   ; Attr.on_click (fun _ -> pick k)
                   ; Attr.on_keydown (fun ev ->
                       match Js_of_ocaml.Js.Optdef.to_option ev##.key with
                       | Some key ->
                         (match Js_of_ocaml.Js.to_string key with
                          | "ArrowRight" -> pick ((k + 1) % n)
                          | "ArrowLeft" -> pick ((k - 1 + n) % n)
                          | "Home" -> pick 0
                          | "End" -> pick (n - 1)
                          | _ -> Effect.Ignore)
                       | None -> Effect.Ignore)
                   ]
                 [ Node.text label ]))
        ; (match List.nth tabs selected with
           | Some (_, cs) ->
             Node.div
               ~attrs:
                 [ Attr.class_ "ui-tabpanel ui-stack"
                 ; Attr.id (id selected ^ "-panel")
                 ; Attr.role "tabpanel"
                 ; Attr.create "aria-labelledby" (id selected)
                 ]
               (children (sprintf "%s.%d" path selected) cs)
           | None -> Node.none)
        ]
    | Accordion { items } ->
      div
        "ui-accordion"
        (List.mapi items ~f:(fun k (item : C.accordion_item) ->
           let key = sprintf "open%d" k in
           let opened =
             match Ui_context.get ctx ~path key with
             | Some v -> String.equal v "1"
             | None -> item.open_
           in
           let id = sprintf "ui-%s-%s-acc%d" ctx.call_id path k in
           Node.div
             ~attrs:[ Attr.classes ([ "ui-acc-item" ] @ if opened then [ "open" ] else []) ]
             [ Node.button
                 ~attrs:
                   [ Attr.type_ "button"
                   ; Attr.class_ "ui-acc-head"
                   ; Attr.create "aria-expanded" (Bool.to_string opened)
                   ; Attr.create "aria-controls" id
                   ; Attr.on_click (fun _ -> Ui_context.set ctx ~path key (if opened then "0" else "1"))
                   ]
                 [ text "ui-acc-title" item.title; text "chev" "" ]
             ; (if opened
                then
                  Node.div
                    ~attrs:[ Attr.class_ "ui-acc-body ui-stack"; Attr.id id ]
                    (children (sprintf "%s.%d" path k) item.children)
                else Node.none)
             ]))
    | Divider -> Node.hr ~attrs:[ Attr.class_ "ui-divider" ] ()
    | Markdown t -> div "ui-markdown" [ Markdown_view.render t ]
    | Callout { tone; title; text = t } ->
      Node.div
        ~attrs:
          [ Attr.classes [ "ui-callout"; tone_cls tone ]
          ; Attr.role
              (match tone with
               | Danger | Warning -> "alert"
               | _ -> "note")
          ]
        [ (match title with
           | Some t -> div "ui-callout-title" [ Node.text t ]
           | None -> Node.none)
        ; Ui_context.md t
        ]
    | Stat { label; value; unit; delta; trend; good; caption; spark } ->
      stat ~label ~value ~unit ~delta ~trend ~good ~caption ~spark
    | Kv { title; items } ->
      div
        "ui-kv"
        [ title_node title
        ; Node.dl
            (List.concat_map items ~f:(fun (k, v) ->
               [ Node.dt [ Node.text k ]; Node.dd [ Ui_context.md v ] ]))
        ]
    | Progress { label; value; max; tone; caption } ->
      progress ~label ~value ~max ~tone ~caption
    | Badges items ->
      div
        "ui-badges"
        (List.map items ~f:(fun (t, tone) -> Node.span ~attrs:[ Attr.classes [ "ui-badge"; tone_cls tone ] ] [ Node.text t ]))
    | Table t -> Ui_table.view ctx ~path t
    | Chart chart -> Ui_chart.view ctx ~path chart
    | Timeline { title; items } -> timeline ~title items
    | Steps { title; items } -> steps ~title items
    | Code { lang; title; code = c } -> code ~lang ~title c
    | Diff { title; diff } ->
      div "ui-diff" [ title_node title; Diff_view.unified ~max_rows:40 ~fold:(ctx.fold, sprintf "%s#ui%s" ctx.call_id path) diff ]
    | Tree { title; entries } -> tree ~title entries
    | Image { image; width } -> Ui_media.image ctx image ~width
    | Gallery { title; images } -> Ui_media.gallery ctx ~title images
    | Calculator t -> Ui_learn.calculator ctx ~path t
    | Flashcards { title; cards } -> Ui_learn.flashcards ctx ~path ~title cards
    | Quiz { title; questions } -> Ui_learn.quiz ctx ~path ~title questions
    | Form { title; description; fields; submit } ->
      Ui_reply.form ctx ~path ~component:reply_id ~title:reply_title ~description fields ~submit ~heading:title
    | Choices { prompt; options; multiple } ->
      Ui_reply.choices ctx ~path ~component:reply_id ~title:reply_title ~prompt options ~multiple
    | Checklist { title; items; submit } ->
      Ui_reply.checklist ctx ~path ~component:(Option.some_if (Option.is_some submit) reply_id) ~title:reply_title ~heading:title items ~submit
    | Unknown { type_; _ } -> unknown type_
  in
  node
;;

let view ctx (t : Ui_spec.t) =
  Node.div
    ~attrs:[ Attr.class_ "genui ui-stack" ]
    (List.mapi t ~f:(fun k c -> component ctx ~path:(Int.to_string k) c))
;;

let skeleton () =
  Node.div
    ~attrs:
      [ Attr.class_ "genui ui-skeleton"
      ; Attr.role "status"
      ; Attr.create "aria-label" "Preparing components"
      ]
    [ div "ui-skel-line short" []; div "ui-skel-block" []; div "ui-skel-line" [] ]
;;
