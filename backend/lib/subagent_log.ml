open! Core
open! Import

module State = struct
  type t =
    | Running
    | Complete
    | Failed
  [@@deriving sexp_of]

  let to_string = function
    | Running -> "running"
    | Complete -> "complete"
    | Failed -> "failed"
  ;;
end

module Summary = struct
  type t =
    { id : string
    ; call_id : string
    ; parent : string option
    ; task : string
    ; model : string
    ; state : State.t
    ; started_at : float
    ; updated_at : float
    ; ended_at : float option
    ; turns : int
    ; tool_calls : int
    ; current_tool : (string * float) option
    ; message_count : int
    ; stale : bool
    ; result : Tool_result.t option
    }
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { id : string
    ; call_id : string
    ; parent : string option
    ; task : string
    ; model : string
    ; started_at : float
    ; mutable updated_at : float
    ; mutable ended : (float * int) option (** time, run it ended in *)
    ; mutable result : Tool_result.t option
    ; mutable turns : int
    ; mutable tool_calls : int
    ; mutable current_tool : (string * float) option
    ; messages : Timed_message.t Queue.t
    }
end

type t =
  { entries : Entry.t Queue.t (** oldest first *)
  ; by_id : Entry.t String.Table.t
  ; mutable runs : int
  }

let create () =
  { entries = Queue.create (); by_id = String.Table.create (); runs = 0 }
;;

let clear t =
  Queue.clear t.entries;
  Hashtbl.clear t.by_id
;;

let rec record_in t ~now ~parent (event : Agent_event.t) =
  let touch id f =
    Option.iter (Hashtbl.find t.by_id id) ~f:(fun (e : Entry.t) ->
      e.updated_at <- now;
      f e)
  in
  match event with
  | Agent_start -> if Option.is_none parent then t.runs <- t.runs + 1
  | Subagent_start { call_id; agent_id; task; model; tools = _ } ->
    let entry =
      { Entry.id = agent_id
      ; call_id
      ; parent
      ; task
      ; model
      ; started_at = now
      ; updated_at = now
      ; ended = None
      ; result = None
      ; turns = 0
      ; tool_calls = 0
      ; current_tool = None
      ; messages = Queue.create ()
      }
    in
    Hashtbl.set t.by_id ~key:agent_id ~data:entry;
    Queue.enqueue t.entries entry
  | Subagent_end { agent_id; result; _ } ->
    touch agent_id (fun e ->
      e.result <- Some result;
      e.ended <- Some (now, t.runs);
      e.current_tool <- None)
  | Subagent { agent_id; event = inner; call_id = _ } ->
    (match inner with
     | Subagent_start _ | Subagent_end _ | Subagent _ ->
       record_in t ~now ~parent:(Some agent_id) inner
     | Turn_start -> touch agent_id (fun e -> e.turns <- e.turns + 1)
     | Tool_start call ->
       touch agent_id (fun e ->
         e.tool_calls <- e.tool_calls + 1;
         e.current_tool <- Some (call.name, now))
     | Tool_end _ -> touch agent_id (fun e -> e.current_tool <- None)
     | Message_end message ->
       touch agent_id (fun e ->
         Queue.enqueue e.messages { Timed_message.message; at = Some now })
     | _ -> touch agent_id ignore)
  | _ -> ()
;;

let record t ~now event = record_in t ~now ~parent:None event

let summary t (e : Entry.t) =
  { Summary.id = e.id
  ; call_id = e.call_id
  ; parent = e.parent
  ; task = e.task
  ; model = e.model
  ; state =
      (match e.result with
       | None -> Running
       | Some { is_error = true; _ } -> Failed
       | Some _ -> Complete)
  ; started_at = e.started_at
  ; updated_at = e.updated_at
  ; ended_at = Option.map e.ended ~f:fst
  ; turns = e.turns
  ; tool_calls = e.tool_calls
  ; current_tool = e.current_tool
  ; message_count = Queue.length e.messages
  ; stale = Option.exists e.ended ~f:(fun (_, run) -> run < t.runs)
  ; result = e.result
  }
;;

let summaries t = Queue.to_list t.entries |> List.map ~f:(summary t)

let find t key =
  let entry =
    match Hashtbl.find t.by_id key with
    | Some e -> Some e
    | None ->
      Queue.find t.entries ~f:(fun (e : Entry.t) -> String.equal e.call_id key)
  in
  Option.map entry ~f:(fun e -> summary t e, Queue.to_list e.messages)
;;
