open! Core
module P = Prigh_protocol

module Item = struct
  type t =
    | User of string
    | Assistant of string
    | Thinking of string
    | Tool_call of P.Tool_call.t
    | Tool_result of P.Message.Tool_result.t
    | Notice of Severity.t * string
    | Block of Content.t
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
  ; tool_tail : string option
  }
[@@deriving sexp_of]

let empty = { items_rev = []; stream = None; tool_tail = None }
let items t = List.rev t.items_rev
let add t item = { t with items_rev = item :: t.items_rev }

let add_message t (m : P.Message.t) =
  match m with
  | User text -> add t (User text)
  | Tool_result r -> add t (Tool_result r)
  | Assistant a ->
    let t =
      List.fold a.content ~init:t ~f:(fun t c ->
        match c with
        | Text text when not (String.is_empty (String.strip text)) ->
          add t (Assistant text)
        | Text _ -> t
        | Thinking text when not (String.is_empty (String.strip text)) ->
          add t (Thinking text)
        | Thinking _ -> t
        | Tool_call call -> add t (Tool_call call))
    in
    (match a.stop_reason with
     | Error e -> add t (Notice (Error, "error: " ^ e))
     | Aborted -> add t (Notice (Warn, "[aborted]"))
     | Length ->
       add t (Notice (Warn, "[output truncated by the model's length limit]"))
     | End_turn | Tool_use -> t)
;;

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
      | Text -> add t (Assistant text)
      | Thinking -> add t (Thinking text))
;;

let append t kind text =
  match t.stream with
  | Some (k, existing) when Stream_kind.equal k kind ->
    { t with stream = Some (kind, existing ^ text) }
  | _ -> { (flush t) with stream = Some (kind, text) }
;;

let set_tool_tail t tool_tail = { t with tool_tail }
let tool_tail t = t.tool_tail

let append_tool_output t chunk =
  let combined = Option.value t.tool_tail ~default:"" ^ chunk in
  let lines = String.split_lines combined in
  let last =
    match List.rev lines with
    | [] -> ""
    | last :: prev :: _ when String.is_empty last -> prev
    | last :: _ -> last
  in
  { t with tool_tail = Some last }
;;

let gray = Style.fg Gray
let dim = Style.dim Style.plain

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

let render_tool_result (r : P.Message.Tool_result.t) ~expand_tools : Content.t =
  let lines = String.split_lines (String.rstrip r.text) in
  let max_lines = if expand_tools then Int.max_value else 8 in
  let shown = List.take lines max_lines in
  let more =
    if List.length lines > max_lines
    then
      [ sprintf
          "… (%d more lines; Ctrl+O expands)"
          (List.length lines - max_lines)
      ]
    else []
  in
  let style = if r.is_error then Style.fg Red else gray in
  List.map (shown @ more) ~f:(fun l -> Content.Line.of_string ~style ("  " ^ l))
;;

let render_item (item : Item.t) ~expand_tools : Content.t =
  match item with
  | User text ->
    List.map (String.split_lines text) ~f:(fun l ->
      [ { Content.Span.text = "> "; style = Style.bold (Style.fg Green) }
      ; { text = l; style = Style.bold Style.plain }
      ])
  | Assistant text -> Markdown.render text
  | Thinking text ->
    List.map (String.split_lines text) ~f:(fun l ->
      Content.Line.of_string ~style:dim ("  " ^ l))
  | Tool_call call ->
    [ [ { Content.Span.text = "⚙ " ^ call.name; style = Style.fg Magenta }
      ; { text = " " ^ summarise_arguments call; style = dim }
      ]
    ]
  | Tool_result r -> render_tool_result r ~expand_tools
  | Notice (severity, text) ->
    let style =
      match severity with
      | Info -> Style.fg Yellow
      | Warn -> Style.bold (Style.fg Yellow)
      | Error -> Style.bold (Style.fg Red)
    in
    Content.lines ~style text
  | Block content -> content
;;

let live_items t : Item.t list =
  let stream =
    match t.stream with
    | None -> []
    | Some (Text, text) -> [ Item.Assistant text ]
    | Some (Thinking, text) -> [ Item.Thinking text ]
  in
  let tool =
    match t.tool_tail with
    | None -> []
    | Some tail ->
      [ Item.Block [ Content.Line.of_string ~style:gray ("  " ^ tail) ] ]
  in
  stream @ tool
;;

let all_items_rev t = List.rev_append (live_items t) t.items_rev

let render_tail t ~width ~rows ~skip ~expand_tools : Content.t =
  let needed = rows + skip in
  let rec gather items acc count =
    if count >= needed
    then acc
    else (
      match items with
      | [] -> acc
      | item :: rest ->
        let lines = Content.wrap (render_item item ~expand_tools) ~width in
        gather rest (lines @ acc) (count + List.length lines))
  in
  let lines = gather (all_items_rev t) [] 0 in
  let total = List.length lines in
  let drop_front = Int.max 0 (total - needed) in
  let window = List.drop lines drop_front in
  List.take window (Int.max 0 (List.length window - skip))
;;

let line_count t ~width ~expand_tools =
  List.sum (module Int) (all_items_rev t) ~f:(fun item ->
    List.length (Content.wrap (render_item item ~expand_tools) ~width))
;;
