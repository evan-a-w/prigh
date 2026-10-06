open! Core
open! Import

module Subagent = struct
  module Status = struct
    type t =
      | Running
      | Done of
          { turns : int
          ; cost_usd : float
          }
      | Failed of string
    [@@deriving sexp_of, equal]
  end

  type t =
    { agent_id : string
    ; task : string
    ; model : string
    ; status : Status.t
    ; turns : int
    ; report : string option
    ; last_tool : string option
    ; nested : string list
    }
  [@@deriving sexp_of, equal]
end

module Item = struct
  type t =
    | User of P.Message.User.t
    | Assistant of
        { text : string
        ; final : bool
        }
    | Thinking of string
    | Tool of
        { call : P.Tool_call.t
        ; result : P.Message.Tool_result.t option
        ; live_tail : string option
        ; subagent : Subagent.t option
        }
    | Notice of Severity.t * string
    | Block of Content.t
    | Compaction of string
    | Delivery of string
    | Skill of
        { skill : Skill_message.t
        ; images : P.Image.t list
        }
    | Handover of Handover_message.t
  [@@deriving sexp_of, equal]
end

let user_item (user : P.Message.User.t) : Item.t =
  if Option.is_some (Delivery.parse user.text)
  then Delivery user.text
  else (
    match Skill_message.parse user.text with
    | Some skill -> Skill { skill; images = user.images }
    | None ->
      (match Handover_message.parse user.text with
       | Some handover -> Handover handover
       | None -> User user))
;;

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

let tail_lines = Tool_render.live_tail_lines

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
  add
    (flush t)
    (Tool { call; result = None; live_tail = None; subagent = None })
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
    | Item.Tool tool :: rest when String.equal tool.call.id call.id ->
      go (Item.Tool { tool with result = Some result } :: acc) true rest
    | item :: rest -> go (item :: acc) found rest
  in
  let found, items_rev = go [] false t.items_rev in
  let t = { t with items_rev } in
  if found
  then t
  else
    add
      t
      (Tool { call; result = Some result; live_tail = None; subagent = None })
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
  | User user -> add t (user_item user)
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
          add
            t
            (Tool { call; result = None; live_tail = None; subagent = None }))
    in
    (match a.stop_reason with
     | Error e -> add t (Notice (Error, "error: " ^ e))
     | Aborted -> add t (Notice (Warn, "[aborted]"))
     | Length ->
       add t (Notice (Warn, "[output truncated by the model's length limit]"))
     | End_turn | Tool_use -> t)
;;

let one_line text =
  Text_width.truncate
    (String.concat ~sep:" " (String.split_lines (String.strip text)))
    ~width:60
;;

let assistant_text (a : P.Message.Assistant.t) =
  String.concat
    ~sep:"\n"
    (List.filter_map a.content ~f:(function
       | P.Content.Text text -> Some text
       | P.Content.Thinking _ | P.Content.Tool_call _ -> None))
;;

let update_tool_subagent t ~call_id ~f =
  let rec go acc = function
    | [] -> List.rev acc
    | Item.Tool tool :: rest when String.equal tool.call.id call_id ->
      List.rev_append
        acc
        (Item.Tool { tool with subagent = f tool.subagent } :: rest)
    | item :: rest -> go (item :: acc) rest
  in
  { t with items_rev = go [] t.items_rev }
;;

let rec update_subagent (s : Subagent.t) (event : P.Event.t) =
  match event with
  | P.Event.Tool_start call ->
    let line = Tool_render.summary call in
    { s with last_tool = Some line; nested = s.nested @ [ line ] }
  | P.Event.Turn_start -> { s with turns = s.turns + 1 }
  | P.Event.Message_end (P.Message.Assistant a) ->
    let text = assistant_text a in
    if String.is_empty (String.strip text)
    then s
    else { s with report = Some text }
  | P.Event.Subagent_start { task; _ } ->
    let line = "subagent " ^ one_line task in
    { s with last_tool = Some line; nested = s.nested @ [ line ] }
  | P.Event.Subagent { event; _ } -> update_subagent s event
  | _ -> s
;;

let mark_subagent t ~call_id ~agent_id ~task ~model =
  let rec go acc = function
    | [] -> List.rev acc
    | Item.Tool tool :: rest when String.equal tool.call.id call_id ->
      List.rev_append
        acc
        (Item.Tool
           { tool with
             subagent =
               Some
                 { Subagent.agent_id
                 ; task
                 ; model
                 ; status = Running
                 ; turns = 0
                 ; report = None
                 ; last_tool = None
                 ; nested = []
                 }
           }
         :: rest)
    | item :: rest -> go (item :: acc) rest
  in
  { t with items_rev = go [] t.items_rev }
;;

let finish_subagent
      t
      ~call_id
      ~agent_id
      ~turns
      ~cost_usd
      (result : P.Event.Subagent_result.t)
  =
  let status =
    if result.is_error
    then Subagent.Status.Failed result.text
    else Subagent.Status.Done { turns; cost_usd }
  in
  let synth =
    { P.Message.Tool_result.tool_call_id = call_id
    ; tool_name = "subagent"
    ; text = result.text
    ; is_error = result.is_error
    ; images = []
    ; at = None
    }
  in
  let rec go acc = function
    | [] -> List.rev acc
    | Item.Tool tool :: rest when String.equal tool.call.id call_id ->
      let subagent =
        match tool.subagent with
        | Some s -> Some { s with status; turns; report = Some result.text }
        | None ->
          Some
            { Subagent.agent_id
            ; task = ""
            ; model = ""
            ; status
            ; turns
            ; report = Some result.text
            ; last_tool = None
            ; nested = []
            }
      in
      List.rev_append
        acc
        (Item.Tool { tool with subagent; result = Some synth } :: rest)
    | item :: rest -> go (item :: acc) rest
  in
  { t with items_rev = go [] t.items_rev }
;;

(** The single event-to-transcript function shared by the main transcript and
    every subagent view. *)
let apply t (event : P.Event.t) =
  match event with
  | P.Event.State state -> if state.running then t else flush t
  | P.Event.Message_start (P.Message.User user) -> add t (user_item user)
  | P.Event.Message_start _ -> t
  | P.Event.Message_update { delta = P.Delta.Text_delta text; _ } ->
    append t Text text
  | P.Event.Message_update { delta = P.Delta.Thinking_delta text; _ } ->
    append t Thinking text
  | P.Event.Message_update _ -> t
  | P.Event.Message_end (P.Message.Assistant a) ->
    let t = flush t in
    let t =
      match a.stop_reason with
      | P.Stop_reason.End_turn -> mark_final t
      | _ -> t
    in
    (match a.stop_reason with
     | P.Stop_reason.Error e -> notice ~severity:Error t ("error: " ^ e)
     | P.Stop_reason.Aborted -> notice ~severity:Warn t "[aborted]"
     | P.Stop_reason.Length ->
       notice ~severity:Warn t "[output truncated by the model's length limit]"
     | P.Stop_reason.End_turn | P.Stop_reason.Tool_use -> t)
  | P.Event.Message_end _ -> t
  | P.Event.Tool_start call -> add_tool t call
  | P.Event.Tool_output { chunk; call_id } ->
    append_tool_output t ~call_id chunk
  | P.Event.Tool_end { call; result } -> end_tool t ~call ~result
  | P.Event.Compacted summary -> add t (Compaction summary)
  | P.Event.Notice text -> notice t text
  | P.Event.Subagent_start { call_id; agent_id; task; model; _ } ->
    mark_subagent t ~call_id ~agent_id ~task ~model
  | P.Event.Subagent { call_id; event = inner; _ } ->
    update_tool_subagent t ~call_id ~f:(fun current ->
      match current with
      | None -> current
      | Some s -> Some (update_subagent s inner))
  | P.Event.Subagent_end { call_id; agent_id; turns; cost_usd; result; _ } ->
    finish_subagent t ~call_id ~agent_id ~turns ~cost_usd result
  | P.Event.Agent_start
  | P.Event.Agent_end _
  | P.Event.Turn_start
  | P.Event.Turn_end _
  | P.Event.Queue_update _
  | P.Event.Tool_confirm _
  | P.Event.Auth _
  | P.Event.Tool_exec _
  | P.Event.Tool_exec_cancel _
  | P.Event.Terminal_open _
  | P.Event.Terminal_frame _
  | P.Event.Terminal_close _
  | P.Event.Btw_delta _
  | P.Event.Config_changed _ -> t
;;

let gray = Style.fg Gray
let dim = Style.dim Style.plain
let red = Style.fg Red
let green = Style.fg Green
let yellow = Style.fg Yellow
let cyan = Style.fg Cyan

let lines_at ?(depth = 1) ~style text =
  List.map
    (String.split_lines (String.rstrip text))
    ~f:(fun line -> Log_line.indent ~depth (Content.Line.of_string ~style line))
;;

let report ?(style = gray) ~(verbosity : Verbosity.t) text =
  match verbosity with
  | Quiet -> []
  | Normal -> Tool_render.output ~style ~head:5 text
  | Verbose -> Tool_render.output ~style ~head:Int.max_value text
;;

let cancelled report =
  List.exists (String.split_lines report) ~f:(fun line ->
    String.equal (String.strip line) "[cancelled]")
;;

let render_subagent ~verbosity ~width (s : Subagent.t) =
  let chip = Tool_render.chip in
  let turns n = if n = 1 then "1 turn" else sprintf "%d turns" n in
  let mark, chips =
    match s.status with
    | Running ->
      ( Tool_render.Mark.Running
      , if s.turns > 0 then [ chip (turns s.turns) ] else [] )
    | Done { turns = n; cost_usd } ->
      Done, [ chip (turns n); chip (sprintf "$%.2f" cost_usd) ]
    | Failed message when cancelled message -> Interrupted, [ chip "cancelled" ]
    | Failed _ -> Failed, [ chip ~style:red "failed" ]
  in
  let header =
    Tool_render.header
      ~width
      ~mark
      ~name:"subagent"
      ~arg:(one_line s.task)
      chips
  in
  let step line =
    Log_line.indent ~depth:2 [ { text = "↳ " ^ line; style = gray } ]
  in
  header
  ::
  (match s.status with
   | Running -> List.map (Option.to_list s.last_tool) ~f:step
   | Failed message ->
     report ~style:(if cancelled message then gray else red) ~verbosity message
   | Done _ ->
     let nested =
       match verbosity with
       | Verbose -> List.map s.nested ~f:step
       | Quiet | Normal -> []
     in
     nested @ report ~verbosity (Option.value s.report ~default:""))
;;

(* The user's turn starts with a blank line and carries the bar. *)
let turn_bar : Content.Span.t = { text = "▌"; style = yellow }

let user_lines text images =
  List.map (String.split_lines text) ~f:(fun line ->
    Log_line.bar
      turn_bar
      (Content.Line.of_string ~style:(Style.bold Style.plain) line))
  @ List.map images ~f:(fun image ->
    Log_line.bar
      turn_bar
      (Content.Line.of_string ~style:cyan (P.Image.to_string_hum image)))
;;

let turn_start = Log_line.flush []

(* The skill's file is long and the user did not type it: a header stands for
   it, and only verbose mode shows it. *)
let render_skill (skill : Skill_message.t) images ~(verbosity : Verbosity.t) =
  let header : Content.Line.t =
    [ { text = "skill "; style = dim }
    ; { text = skill.name; style = Style.bold Style.plain }
    ]
  in
  let header, body =
    match verbosity with
    | Quiet | Normal -> header, []
    | Verbose ->
      ( header @ [ { text = "  " ^ skill.location; style = dim } ]
      , List.map (String.split_lines skill.body) ~f:(fun line ->
          Log_line.bar turn_bar (Content.Line.of_string ~style:dim line)) )
  in
  (turn_start :: Log_line.bar turn_bar header :: body)
  @ user_lines skill.args images
;;

let thinking_rail : Content.Span.t = { text = "┆"; style = dim }

let render_thinking text ~(verbosity : Verbosity.t) =
  let lines = String.split_lines (String.strip text) in
  let lines =
    match verbosity with
    | Quiet -> []
    | Normal when List.length lines > 3 -> List.take lines 3 @ [ "…" ]
    | Normal | Verbose -> lines
  in
  List.map lines ~f:(fun line ->
    Log_line.bar
      thinking_rail
      (Content.Line.of_string ~style:(Style.italic dim) line))
;;

let rows (item : Item.t) ~(verbosity : Verbosity.t) ~width : Log_line.t list =
  let markdown text =
    let width =
      if width = Int.max_value
      then None
      else Some (width - Log_line.gutter_width)
    in
    List.map (Markdown.render ?width text) ~f:Log_line.indent
  in
  match item with
  | User { text; images; at = _ } -> turn_start :: user_lines text images
  | Skill { skill; images } -> render_skill skill images ~verbosity
  | Handover handover ->
    let summary : Content.Span.t =
      { text = Handover_message.summary handover; style = yellow }
    in
    (* The failed reply above it already shows the error. *)
    (match verbosity with
     | Quiet | Normal -> [ Log_line.indent [ summary ] ]
     | Verbose ->
       [ Log_line.indent
           [ summary; { text = sprintf " (%s)" handover.error; style = dim } ]
       ])
  | Assistant { text; final } ->
    (match verbosity with
     | Quiet when not final ->
       let first =
         match String.split_lines (String.strip text) with
         | first :: _ :: _ -> first ^ " …"
         | [ first ] -> first
         | [] -> ""
       in
       markdown first
     | Quiet | Normal | Verbose -> markdown text)
  | Thinking text -> render_thinking text ~verbosity
  | Tool { call; result; live_tail; subagent } ->
    (match subagent with
     | Some s -> render_subagent ~verbosity ~width s
     | None -> Tool_render.render ~verbosity ~width call result ~live_tail)
  | Notice (severity, text) ->
    (match verbosity, severity with
     | Quiet, (Info | Debug) -> []
     | Normal, Debug -> []
     | _ ->
       let style =
         match severity with
         | Debug -> dim
         | Info -> yellow
         | Warn -> Style.bold yellow
         | Error -> Style.bold red
       in
       lines_at ~style text)
  | Block content -> List.map content ~f:(Log_line.indent ~depth:1)
  | Delivery text ->
    List.concat_map
      (Option.value (Delivery.parse text) ~default:[])
      ~f:(fun (section : Delivery.Section.t) ->
        let header =
          Log_line.mark
            { text = "↩"; style = (if section.ok then green else red) }
            [ { text = section.kind; style = Style.bold Style.plain }
            ; { text = " " ^ section.id; style = Style.plain }
            ; { text = " \"" ^ one_line section.task ^ "\""; style = dim }
            ; { text = "  " ^ section.status
              ; style = (if section.ok then dim else red)
              }
            ]
        in
        header :: report ~verbosity (String.concat ~sep:"\n" section.body))
  | Compaction summary ->
    let heading =
      Log_line.indent (Content.Line.of_string ~style:cyan "context compacted")
    in
    (match verbosity with
     | Verbose when not (String.is_empty (String.strip summary)) ->
       heading :: lines_at ~style:dim summary
     | Quiet | Normal | Verbose -> [ heading ])
;;

let render_item ?(width = Int.max_value) item ~verbosity =
  Log_line.wrap (rows item ~verbosity ~width) ~width
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
        let lines = render_item item ~width ~verbosity in
        gather rest (lines @ acc) (count + List.length lines))
  in
  let lines = gather (all_items_rev t) [] 0 in
  let total = List.length lines in
  let drop_front = Int.max 0 (total - needed) in
  let window = List.drop lines drop_front in
  List.take window (Int.max 0 (List.length window - skip))
;;

let line_count t ~width ~verbosity =
  List.sum
    (module Int)
    (all_items_rev t)
    ~f:(fun item -> List.length (render_item item ~width ~verbosity))
;;

let render_all t ~width ~verbosity : Content.t =
  List.concat_map
    (List.rev (all_items_rev t))
    ~f:(fun item -> render_item item ~width ~verbosity)
;;

let user_message_lines t ~width ~verbosity : int list =
  let rec go items acc offset =
    match items with
    | [] -> List.rev acc
    | item :: rest ->
      let acc =
        match item with
        (* Past the blank line that opens the turn. *)
        | Item.User _ | Item.Skill _ -> (offset + 1) :: acc
        | _ -> acc
      in
      go rest acc (offset + List.length (render_item item ~width ~verbosity))
  in
  go (List.rev (all_items_rev t)) [] 0
;;

let render_window t ~width ~rows ~top ~verbosity : Content.t =
  let total = line_count t ~width ~verbosity in
  let skip = Int.max 0 (total - top - rows) in
  render_tail t ~width ~rows ~skip ~verbosity
;;
