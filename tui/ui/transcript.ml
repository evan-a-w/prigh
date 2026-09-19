open! Core
module P = Prigh_protocol

module Item = struct
  type t =
    | User of string
    | Assistant of
        { text : string
        ; final : bool
        }
    | Thinking of string
    | Tool of
        { call : P.Tool_call.t
        ; result : P.Message.Tool_result.t option
        ; live_tail : string option
        }
    | Notice of Severity.t * string
    | Block of Content.t
    | Compaction of string
  [@@deriving sexp_of, equal]
end

module Stream_kind = struct
  type t =
    | Text
    | Thinking
  [@@deriving sexp_of, equal]
end

type t =
  { items_rev : Item.t list
  ; stream : (Stream_kind.t * string) option
  }
[@@deriving sexp_of]

let empty = { items_rev = []; stream = None }
let items t = List.rev t.items_rev
let add t item = { t with items_rev = item :: t.items_rev }
let notice ?(severity = Severity.Info) t text = add t (Notice (severity, text))
let clear t = { t with items_rev = [] }

let flush t =
  match t.stream with
  | None -> t
  | Some (kind, text) ->
    let t = { t with stream = None } in
    if String.is_empty (String.strip text)
    then t
    else (
      match kind with
      | Text -> add t (Assistant { text; final = false })
      | Thinking -> add t (Thinking text))
;;

let append t kind text =
  match t.stream with
  | Some (k, existing) when Stream_kind.equal k kind ->
    { t with stream = Some (kind, existing ^ text) }
  | _ -> { (flush t) with stream = Some (kind, text) }
;;

let mark_final t =
  let rec go = function
    | [] -> []
    | Item.Assistant a :: rest -> Item.Assistant { a with final = true } :: rest
    | item :: rest -> item :: go rest
  in
  { t with items_rev = go t.items_rev }
;;

let tail_lines = 5

let update_tail previous chunk =
  let combined = Option.value previous ~default:"" ^ chunk in
  let lines = String.split_lines combined in
  let kept = List.drop lines (Int.max 0 (List.length lines - tail_lines)) in
  let text = String.concat ~sep:"\n" kept in
  if String.is_suffix combined ~suffix:"\n" && not (String.is_empty text)
  then text ^ "\n"
  else text
;;

let add_tool t (call : P.Tool_call.t) =
  add (flush t) (Tool { call; result = None; live_tail = None })
;;

let append_tool_output t ~call_id chunk =
  let rec go acc = function
    | [] -> List.rev acc
    | Item.Tool tool :: rest
      when String.equal tool.call.id call_id && Option.is_none tool.result ->
      let live_tail = Some (update_tail tool.live_tail chunk) in
      List.rev_append acc (Item.Tool { tool with live_tail } :: rest)
    | item :: rest -> go (item :: acc) rest
  in
  { t with items_rev = go [] t.items_rev }
;;

let end_tool t ~(call : P.Tool_call.t) ~(result : P.Message.Tool_result.t) =
  let t = flush t in
  let rec go acc found = function
    | [] -> found, List.rev acc
    | Item.Tool tool :: rest
      when String.equal tool.call.id call.id && Option.is_none tool.result ->
      go (Item.Tool { tool with result = Some result } :: acc) true rest
    | item :: rest -> go (item :: acc) found rest
  in
  let found, items_rev = go [] false t.items_rev in
  let t = { t with items_rev } in
  if found
  then t
  else add t (Tool { call; result = Some result; live_tail = None })
;;

let pair_result t (r : P.Message.Tool_result.t) =
  let rec go acc = function
    | [] -> List.rev acc
    | Item.Tool tool :: rest
      when String.equal tool.call.id r.tool_call_id
           && Option.is_none tool.result ->
      List.rev_append acc (Item.Tool { tool with result = Some r } :: rest)
    | item :: rest -> go (item :: acc) rest
  in
  { t with items_rev = go [] t.items_rev }
;;

let add_message t (m : P.Message.t) =
  match m with
  | User text -> add t (User text)
  | Tool_result r -> pair_result t r
  | Assistant a ->
    let final = P.Stop_reason.equal P.Stop_reason.End_turn a.stop_reason in
    let t =
      List.fold a.content ~init:t ~f:(fun t c ->
        match c with
        | Text text when not (String.is_empty (String.strip text)) ->
          add t (Assistant { text; final })
        | Text _ -> t
        | Thinking text when not (String.is_empty (String.strip text)) ->
          add t (Thinking text)
        | Thinking _ -> t
        | Tool_call call ->
          add t (Tool { call; result = None; live_tail = None }))
    in
    (match a.stop_reason with
     | Error e -> add t (Notice (Error, "error: " ^ e))
     | Aborted -> add t (Notice (Warn, "[aborted]"))
     | Length ->
       add t (Notice (Warn, "[output truncated by the model's length limit]"))
     | End_turn | Tool_use -> t)
;;

let gray = Style.fg Gray
let dim = Style.dim Style.plain
let red = Style.fg Red
let green = Style.fg Green
let magenta = Style.fg Magenta

let summarise_arguments (call : P.Tool_call.t) =
  match P.Json.parse call.arguments with
  | Ok (`Object fields) ->
    String.concat
      ~sep:" "
      (List.map fields ~f:(fun (key, value) ->
         let shown =
           match value with
           | `String s -> s
           | other -> P.Json.to_string other
         in
         let one_line = String.concat ~sep:"⏎" (String.split_lines shown) in
         key ^ "=" ^ Text_width.truncate one_line ~width:80))
  | _ -> call.arguments
;;

let first_string_argument (call : P.Tool_call.t) =
  match P.Json.parse call.arguments with
  | Ok (`Object fields) ->
    List.find_map fields ~f:(fun (_, value) ->
      match value with
      | `String s -> Some s
      | _ -> None)
  | _ -> None
;;

let rec pretty_json ?(indent = 0) (json : P.Json.t) : string list =
  let pad = String.make indent ' ' in
  match json with
  | `Object [] -> [ pad ^ "{}" ]
  | `Object fields ->
    ((pad ^ "{")
     :: List.concat_map fields ~f:(fun (key, value) ->
       match value with
       | `Object _ | `Array _ ->
         sprintf "%s  %s:" pad key :: pretty_json ~indent:(indent + 4) value
       | _ -> [ sprintf "%s  %s: %s" pad key (P.Json.to_string value) ]))
    @ [ pad ^ "}" ]
  | `Array [] -> [ pad ^ "[]" ]
  | `Array items ->
    ((pad ^ "[")
     :: List.concat_map items ~f:(fun item ->
       pretty_json ~indent:(indent + 2) item))
    @ [ pad ^ "]" ]
  | _ -> [ pad ^ P.Json.to_string json ]
;;

let count_lines text = List.length (String.split_lines (String.rstrip text))
let plural_lines n = if n = 1 then "1 line" else sprintf "%d lines" n

let first_line text =
  match String.split_lines text with
  | first :: _ -> first
  | [] -> ""
;;

let render_call (call : P.Tool_call.t) : Content.t =
  [ [ { Content.Span.text = "⚙ " ^ call.name; style = magenta }
    ; { text = " " ^ summarise_arguments call; style = dim }
    ]
  ]
;;

let render_call_full (call : P.Tool_call.t) : Content.t =
  let header : Content.Line.t =
    [ { Content.Span.text = "⚙ " ^ call.name; style = magenta } ]
  in
  let arguments =
    match P.Json.parse call.arguments with
    | Ok json ->
      List.map (pretty_json json) ~f:(fun line ->
        Content.Line.of_string ~style:dim ("  " ^ line))
    | Error _ -> Content.lines ~style:dim call.arguments
  in
  header :: arguments
;;

let render_result ?(show_more = true) (r : P.Message.Tool_result.t) ~max_lines
  : Content.t
  =
  let lines = String.split_lines (String.rstrip r.text) in
  let shown = List.take lines max_lines in
  let more =
    if show_more && List.length lines > max_lines
    then [ sprintf "… (%d more)" (List.length lines - max_lines) ]
    else []
  in
  let style = if r.is_error then red else gray in
  List.map (shown @ more) ~f:(fun line ->
    Content.Line.of_string ~style ("  " ^ line))
;;

let render_live_tail live_tail ~max_lines : Content.t =
  match live_tail with
  | None -> []
  | Some text ->
    let lines = String.split_lines text in
    let lines = List.drop lines (Int.max 0 (List.length lines - max_lines)) in
    List.map lines ~f:(fun line ->
      Content.Line.of_string ~style:gray ("  " ^ line))
;;

let merged_tool_line
  (call : P.Tool_call.t)
  (result : P.Message.Tool_result.t option)
  : Content.Line.t
  =
  let argument =
    match first_string_argument call with
    | Some s when not (String.is_empty (String.strip s)) ->
      " "
      ^ Text_width.truncate
          (String.concat ~sep:" " (String.split_lines s))
          ~width:60
    | _ -> ""
  in
  let status, style, count =
    match result with
    | None -> "…", Style.fg Yellow, ""
    | Some r ->
      ( (if r.is_error then "✗" else "✓")
      , (if r.is_error then red else green)
      , " " ^ plural_lines (count_lines r.text) )
  in
  match result with
  | Some r when r.is_error ->
    Content.Line.of_string
      ~style:red
      (sprintf "⚙ %s%s %s%s" call.name argument status count)
  | _ ->
    [ { Content.Span.text = "⚙ " ^ call.name; style = magenta }
    ; { text = argument; style = dim }
    ; { text = " " ^ status; style }
    ; { text = count; style = dim }
    ]
;;

let render_tool
  ~(verbosity : Verbosity.t)
  (call : P.Tool_call.t)
  result
  live_tail
  : Content.t
  =
  match verbosity with
  | Quiet ->
    let merged = merged_tool_line call result in
    (match result with
     | Some r when r.is_error ->
       merged :: render_result ~show_more:false r ~max_lines:3
     | _ -> [ merged ])
  | Normal ->
    let output =
      match result with
      | None -> render_live_tail live_tail ~max_lines:1
      | Some r -> render_result r ~max_lines:(if r.is_error then 8 else 5)
    in
    render_call call @ output
  | Verbose ->
    let output =
      match result with
      | None -> render_live_tail live_tail ~max_lines:tail_lines
      | Some r -> render_result r ~max_lines:Int.max_value
    in
    render_call_full call @ output
;;

let render_item (item : Item.t) ~(verbosity : Verbosity.t) : Content.t =
  match item with
  | User text ->
    List.map (String.split_lines text) ~f:(fun l ->
      [ { Content.Span.text = "> "; style = Style.bold (Style.fg Green) }
      ; { text = l; style = Style.bold Style.plain }
      ])
  | Assistant { text; final } ->
    (match verbosity with
     | Quiet when not final ->
       Markdown.render (first_line text) @ Content.lines ~style:dim "…"
     | Quiet | Normal | Verbose -> Markdown.render text)
  | Thinking text ->
    (match verbosity with
     | Quiet -> []
     | Normal ->
       let lines = List.take (String.split_lines text) 3 in
       List.map lines ~f:(fun line ->
         Content.Line.of_string ~style:dim ("  " ^ line))
     | Verbose ->
       List.map (String.split_lines text) ~f:(fun line ->
         Content.Line.of_string ~style:dim ("  " ^ line)))
  | Tool { call; result; live_tail } ->
    render_tool ~verbosity call result live_tail
  | Notice (severity, text) ->
    (match verbosity, severity with
     | Quiet, Info -> []
     | _ ->
       let style =
         match severity with
         | Info -> Style.fg Yellow
         | Warn -> Style.bold (Style.fg Yellow)
         | Error -> Style.bold (Style.fg Red)
       in
       Content.lines ~style text)
  | Block content -> content
  | Compaction summary ->
    let heading = Content.lines ~style:(Style.fg Cyan) "context compacted" in
    (match verbosity with
     | Verbose when not (String.is_empty (String.strip summary)) ->
       heading @ Content.lines ~style:dim summary
     | Quiet | Normal | Verbose -> heading)
;;

let live_items t : Item.t list =
  match t.stream with
  | None -> []
  | Some (Text, text) -> [ Item.Assistant { text; final = false } ]
  | Some (Thinking, text) -> [ Item.Thinking text ]
;;

let all_items_rev t = List.rev_append (live_items t) t.items_rev

let render_tail t ~width ~rows ~skip ~verbosity : Content.t =
  let needed = rows + skip in
  let rec gather items acc count =
    if count >= needed
    then acc
    else (
      match items with
      | [] -> acc
      | item :: rest ->
        let lines = Content.wrap (render_item item ~verbosity) ~width in
        gather rest (lines @ acc) (count + List.length lines))
  in
  let lines = gather (all_items_rev t) [] 0 in
  let total = List.length lines in
  let drop_front = Int.max 0 (total - needed) in
  let window = List.drop lines drop_front in
  List.take window (Int.max 0 (List.length window - skip))
;;

let line_count t ~width ~verbosity =
  List.sum (module Int) (all_items_rev t) ~f:(fun item ->
    List.length (Content.wrap (render_item item ~verbosity) ~width))
;;

let render_window t ~width ~rows ~top ~verbosity : Content.t =
  let total = line_count t ~width ~verbosity in
  let skip = Int.max 0 (total - top - rows) in
  render_tail t ~width ~rows ~skip ~verbosity
;;
