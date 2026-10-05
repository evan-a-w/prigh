open! Core
open! Import

type t =
  { rev_entries : entry list
  ; tools : tool String.Map.t
  ; running : bool
  }

and entry =
  | User of Message.User.t
  | Assistant of
      { message : Message.Assistant.t
      ; streaming : bool
      }
  | Notice of string
  | Shell of Tool_call.t
  | Compaction of string

and tool =
  { call : Tool_call.t
  ; output : string
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
    | Notice of string
    | Shell of Tool_call.t
    | Compaction of string
  [@@deriving sexp_of]
end

let empty = { rev_entries = []; tools = String.Map.empty; running = false }
let running t = t.running
let entries t = List.rev t.rev_entries
let tool t id = Map.find t.tools id
let add t entry = { t with rev_entries = entry :: t.rev_entries }
let add_notice t text = add t (Notice text)

let add_call t (call : Tool_call.t) =
  let tools =
    Map.update t.tools call.id ~f:(function
      | Some tool -> { tool with call }
      | None -> { call; output = ""; result = None; subagent = None })
  in
  { t with tools }
;;

let add_calls t (message : Message.Assistant.t) =
  List.fold message.content ~init:t ~f:(fun t -> function
    | Tool_call call -> add_call t call
    | Text _ | Thinking _ -> t)
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

let shell_command (call : Tool_call.t) =
  match Json.parse call.arguments with
  | Ok json ->
    (match Json.field json "command" with
     | Some (`String command) -> command
     | _ -> "")
  | Error _ -> ""
;;

let of_messages messages = List.fold messages ~init:empty ~f:add_message

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
    update_tool t call_id ~f:(fun tool ->
      { tool with output = tool.output ^ chunk })
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
  | State _
  | Config_changed _
  | Notice _
  | Queue_update _
  | Auth _
  | Tool_exec _
  | Tool_exec_cancel _
  | Terminal_open _
  | Terminal_frame _
  | Terminal_close _
  | Btw_delta _ -> t
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
