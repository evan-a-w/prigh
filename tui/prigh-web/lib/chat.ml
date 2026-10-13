open! Core
open! Import

type t =
  { rev_entries : entry list
  ; tools : tool String.Map.t
  ; running : bool
  ; first : int
  }

and entry =
  | User of Message.User.t
  | Assistant of
      { message : Message.Assistant.t
      ; streaming : bool
      }
  | Shell of Tool_call.t
  | Compaction of string

and tool =
  { call : Tool_call.t
  ; output : string
  ; lines : int
  ; missed : bool
  ; result : Message.Tool_result.t option
  ; subagent : subagent option
  }

and subagent =
  { agent_id : string
  ; task : string
  ; model : string
  ; chat : t
  ; turns : int
  ; cost_usd : float option
  ; result : Event.Subagent_result.t option
  }
[@@deriving sexp_of]

module Subagent = struct
  type nonrec t = subagent =
    { agent_id : string
    ; task : string
    ; model : string
    ; chat : t
    ; turns : int
    ; cost_usd : float option
    ; result : Event.Subagent_result.t option
    }
  [@@deriving sexp_of]
end

module Tool = struct
  type t = tool =
    { call : Tool_call.t
    ; output : string
    ; lines : int
    ; missed : bool
    ; result : Message.Tool_result.t option
    ; subagent : Subagent.t option
    }
  [@@deriving sexp_of]
end

module Entry = struct
  type t = entry =
    | User of Message.User.t
    | Assistant of
        { message : Message.Assistant.t
        ; streaming : bool
        }
    | Shell of Tool_call.t
    | Compaction of string
  [@@deriving sexp_of]
end

let empty =
  { rev_entries = []; tools = String.Map.empty; running = false; first = 0 }
;;

let first t = t.first
let running t = t.running

let set_running t running =
  if Bool.equal t.running running then t else { t with running }
;;

let entries t = List.rev t.rev_entries
let tool t id = Map.find t.tools id
let add t entry = { t with rev_entries = entry :: t.rev_entries }

let add_call t (call : Tool_call.t) =
  let tools =
    Map.update t.tools call.id ~f:(function
      | Some tool -> { tool with call }
      | None ->
        { call
        ; output = ""
        ; lines = 0
        ; missed = false
        ; result = None
        ; subagent = None
        })
  in
  { t with tools }
;;

let add_calls t (message : Message.Assistant.t) =
  List.fold message.content ~init:t ~f:(fun t -> function
    | Tool_call call -> add_call t call
    | Text _ | Thinking _ | Server_tool _ -> t)
;;

let update_tool t id ~f =
  match Map.find t.tools id with
  | None -> t
  | Some tool -> { t with tools = Map.set t.tools ~key:id ~data:(f tool) }
;;

let set_result t (result : Message.Tool_result.t) =
  update_tool t result.tool_call_id ~f:(fun tool ->
    { tool with result = Some result })
;;

(* The streaming assistant message is the newest entry. *)
let set_assistant t message ~streaming =
  let t = add_calls t message in
  match t.rev_entries with
  | Assistant { streaming = true; _ } :: rest ->
    { t with rev_entries = Assistant { message; streaming } :: rest }
  | _ -> add t (Assistant { message; streaming })
;;

let add_message t (message : Message.t) =
  match message with
  | User u -> add t (User u)
  | Assistant a -> set_assistant t a ~streaming:false
  | Tool_result r -> set_result t r
;;

let live_limit = 32_768

let newlines s =
  let rec go pos n =
    match String.index_from s pos '\n' with
    | Some i -> go (i + 1) (n + 1)
    | None -> n
  in
  go 0 0
;;

(* Only the end of a running call's output shows: keeping all of it would
   copy it all at every chunk. *)
let append_output (tool : tool) chunk =
  let output = tool.output ^ chunk in
  let lines = tool.lines + newlines chunk in
  let length = String.length output in
  if length <= 2 * live_limit
  then { tool with output; lines }
  else (
    let from = length - live_limit in
    let start =
      match String.index_from output from '\n' with
      | Some i -> i + 1
      | None ->
        (* One long line: cut it at a character's first byte. *)
        let rec start i =
          if i < length && Char.to_int output.[i] land 0xC0 = 0x80
          then start (i + 1)
          else i
        in
        start from
    in
    { tool with output = String.drop_prefix output start; lines })
;;

let shell_command (call : Tool_call.t) =
  match Json.parse call.arguments with
  | Ok json ->
    (match Json.field json "command" with
     | Some (`String command) -> command
     | _ -> "")
  | Error _ -> ""
;;

let missed_output t =
  { t with
    tools =
      Map.map t.tools ~f:(fun tool ->
        if Option.is_some tool.result || tool.missed
        then tool
        else { tool with missed = true })
  }
;;

(* A running session's calls without a result streamed before the page came. *)
let of_messages ?(running = false) messages =
  let t = List.fold messages ~init:{ empty with running } ~f:add_message in
  if running then missed_output t else t
;;

let prepend t messages =
  let older = of_messages messages in
  { t with
    rev_entries = t.rev_entries @ older.rev_entries
  ; tools =
      Map.merge_skewed older.tools t.tools ~combine:(fun ~key:_ _ newer ->
        newer)
  ; first = t.first - List.length older.rev_entries
  }
;;

let subagents_to_load t =
  Map.data t.tools
  |> List.filter_map ~f:(fun tool ->
    if String.equal tool.call.name "subagent" && Option.is_none tool.subagent
    then Some tool.call.id
    else None)
;;

let set_subagent t ~call_id subagent =
  update_tool t call_id ~f:(fun tool ->
    match tool.subagent with
    | Some _ -> tool
    | None -> { tool with subagent = Some subagent })
;;

let rec apply t (event : Event.t) =
  let t =
    match event with
    | Agent_start
    | Turn_start
    | Message_start _
    | Message_update _
    | Tool_start _
    | Tool_output _ -> { t with running = true }
    | Agent_end _ -> { t with running = false }
    | _ -> t
  in
  match event with
  | Message_start (User u) ->
    (* [!command]'s output, added to the context, is already on show. *)
    (match t.rev_entries with
     | Shell call :: _
       when String.is_prefix
              u.text
              ~prefix:(sprintf "$ %s\n" (shell_command call)) -> t
     | _ -> add t (User u))
  | Message_start (Assistant a) -> set_assistant t a ~streaming:true
  | Message_update { partial; delta = _ } ->
    set_assistant t partial ~streaming:true
  | Message_end (Assistant a) -> set_assistant t a ~streaming:false
  | Message_end (Tool_result r) -> set_result t r
  | Message_start (Tool_result _) | Message_end (User _) -> t
  | Tool_start call ->
    let t = add_call t call in
    if String.equal call.name "shell" then add t (Shell call) else t
  | Tool_output { call_id; chunk } ->
    update_tool t call_id ~f:(fun tool -> append_output tool chunk)
  | Tool_end { call = _; result } -> set_result t result
  | Subagent_start { call_id; agent_id; task; model; tools = _ } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with
        subagent =
          Some
            { agent_id
            ; task
            ; model
            ; chat = empty
            ; turns = 0
            ; cost_usd = None
            ; result = None
            }
      })
  | Subagent { call_id; agent_id = _; event } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with
        subagent =
          Option.map tool.subagent ~f:(fun s ->
            let turns =
              match event with
              | Turn_start -> s.turns + 1
              | _ -> s.turns
            in
            { s with chat = apply s.chat event; turns })
      })
  | Subagent_end { call_id; result; turns; cost_usd; _ } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with
        subagent =
          Option.map tool.subagent ~f:(fun s ->
            { s with result = Some result; turns; cost_usd = Some cost_usd })
      })
  | Compacted summary -> add t (Compaction summary)
  | Agent_start
  | Agent_end _
  | Turn_start
  | Turn_end _
  | Tool_confirm _
  | Tool_confirm_done _
  | Ask_user _
  | Ask_user_done _
  | State _
  | Config_changed _
  | Notice _
  | Queue_update _
  | Auth _
  | Tool_exec _
  | Tool_exec_cancel _
  | Tool_exec_input _
  | Terminal_open _
  | Terminal_frame _
  | Terminal_close _
  | Terminal_kill _
  | Btw_delta _
  | Sessions_changed -> t
;;

let rec received (event : Event.t) ~at : Event.t =
  let stamp (message : Message.t) : Message.t =
    match message with
    | User ({ at = None; _ } as u) -> User { u with at = Some at }
    | Assistant ({ at = None; _ } as a) -> Assistant { a with at = Some at }
    | Tool_result ({ at = None; _ } as r) -> Tool_result { r with at = Some at }
    | User { at = Some _; _ }
    | Assistant { at = Some _; _ }
    | Tool_result { at = Some _; _ } -> message
  in
  match event with
  | Message_start m -> Message_start (stamp m)
  | Message_end m -> Message_end (stamp m)
  | Subagent s -> Subagent { s with event = received s.event ~at }
  | _ -> event
;;

let rec find_subagent t agent_id =
  Map.data t.tools
  |> List.find_map ~f:(fun tool ->
    Option.bind tool.subagent ~f:(fun s ->
      if String.equal s.agent_id agent_id
      then Some s
      else find_subagent s.chat agent_id))
;;

let rec map_subagent t agent_id ~f =
  { t with
    tools =
      Map.map t.tools ~f:(fun tool ->
        match tool.subagent with
        | None -> tool
        | Some s when String.equal s.agent_id agent_id ->
          { tool with subagent = Some (f s) }
        | Some s ->
          { tool with
            subagent = Some { s with chat = map_subagent s.chat agent_id ~f }
          })
  }
;;

let set_nested_subagent t ~parent ~call_id subagent =
  match parent with
  | None -> set_subagent t ~call_id subagent
  | Some parent ->
    map_subagent t parent ~f:(fun s ->
      { s with chat = set_subagent s.chat ~call_id subagent })
;;

let find_tool t ~agent_id ~call_id =
  match agent_id with
  | None -> tool t call_id
  | Some agent_id ->
    Option.bind (find_subagent t agent_id) ~f:(fun s -> tool s.chat call_id)
;;

let set_output t ~agent_id ~call_id text =
  let set chat =
    update_tool chat call_id ~f:(fun tool ->
      { tool with
        result =
          Option.map tool.result ~f:(fun r -> { r with text; elided = None })
      })
  in
  match agent_id with
  | None -> set t
  | Some agent_id ->
    map_subagent t agent_id ~f:(fun s -> { s with chat = set s.chat })
;;

let same_entry (a : entry) (b : entry) =
  match a, b with
  | User a, User b -> Message.User.equal a b
  | Assistant a, Assistant b ->
    Bool.equal a.streaming b.streaming
    && Message.Assistant.equal a.message b.message
  | Compaction a, Compaction b -> String.equal a b
  | Shell a, Shell b -> Tool_call.equal a b
  | (User _ | Assistant _ | Shell _ | Compaction _), _ -> false
;;

(* A call's card stays as it was (with the output it streamed) while the
   backend says the same about it; an unfinished subagent's transcript is
   loaded again. *)
let same_tool (old : tool) (fresh : tool) =
  let fresh_result =
    (* An output cut short in [fresh] that [old] has whole. *)
    match old.result, fresh.result with
    | Some o, Some f when Option.is_some f.elided && Option.is_none o.elided ->
      Some { f with text = o.text; elided = None }
    | _, result -> result
  in
  Tool_call.equal old.call fresh.call
  && Option.equal Message.Tool_result.equal old.result fresh_result
  && Option.for_all old.subagent ~f:(fun s -> Option.is_some s.result)
;;

let reconcile ~old fresh =
  let rec entries old fresh_entries =
    match old, fresh_entries with
    | o :: old, f :: rest when same_entry o f -> o :: entries old rest
    (* What only the page shows: a [!command]'s card (in the context as the
       user message with its output, or not at all for [!!command]), a
       compaction's summary. *)
    | (Shell call as o) :: old, User u :: rest
      when String.is_prefix
             u.text
             ~prefix:(sprintf "$ %s\n" (shell_command call)) ->
      o :: entries old rest
    | ((Shell _ | Compaction _) as o) :: old, fresh_entries ->
      o :: entries old fresh_entries
    | _ :: old, f :: rest -> f :: entries old rest
    (* The message being streamed is only in [get_messages] once it ends. *)
    | [ (Assistant { streaming = true; _ } as o) ], [] when fresh.running ->
      [ o ]
    | _, rest -> rest
  in
  let rev_entries =
    List.rev (entries (List.rev old.rev_entries) (List.rev fresh.rev_entries))
  in
  let tools =
    Map.mapi fresh.tools ~f:(fun ~key ~data ->
      match Map.find old.tools key with
      | Some o when same_tool o data -> o
      | _ -> data)
  in
  let same_list = List.equal phys_equal in
  if
    Bool.equal old.running fresh.running
    && same_list old.rev_entries rev_entries
    && Map.equal phys_equal old.tools tools
  then old
  else { rev_entries; tools; running = fresh.running; first = old.first }
;;
