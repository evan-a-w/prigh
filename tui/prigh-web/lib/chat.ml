open! Core
open! Import

type t =
  { rev_entries : entry list
  ; tools : tool String.Map.t
  }

and entry =
  | User of Message.User.t
  | Assistant of
      { message : Message.Assistant.t
      ; streaming : bool
      }
  | Notice of string

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
  ; result : Event.Subagent_result.t option
  }
[@@deriving sexp_of]

module Subagent = struct
  type nonrec t = subagent =
    { agent_id : string
    ; task : string
    ; model : string
    ; chat : t
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
  [@@deriving sexp_of]
end

let empty = { rev_entries = []; tools = String.Map.empty }
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

let of_messages messages = List.fold messages ~init:empty ~f:add_message

let rec apply t (event : Event.t) =
  match event with
  | Message_start (User u) -> add t (User u)
  | Message_start (Assistant a) -> set_assistant t a ~streaming:true
  | Message_update { partial; delta = _ } ->
    set_assistant t partial ~streaming:true
  | Message_end (Assistant a) -> set_assistant t a ~streaming:false
  | Message_end (Tool_result r) -> set_result t r
  | Message_start (Tool_result _) | Message_end (User _) -> t
  | Tool_start call -> add_call t call
  | Tool_output { call_id; chunk } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with output = tool.output ^ chunk })
  | Tool_end { call = _; result } -> set_result t result
  | Subagent_start { call_id; agent_id; task; model; tools = _ } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with
        subagent = Some { agent_id; task; model; chat = empty; result = None }
      })
  | Subagent { call_id; agent_id = _; event } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with
        subagent =
          Option.map tool.subagent ~f:(fun s ->
            { s with chat = apply s.chat event })
      })
  | Subagent_end { call_id; result; _ } ->
    update_tool t call_id ~f:(fun tool ->
      { tool with
        subagent =
          Option.map tool.subagent ~f:(fun s -> { s with result = Some result })
      })
  | Compacted summary -> add_notice t ("Compacted: " ^ summary)
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
