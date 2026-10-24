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

(* One line of [subagents.jsonl]: what is needed to rebuild the entries, in
   the order it was recorded. Streaming progress is not written. *)
module Line = struct
  module Result = struct
    type t =
      { text : string
      ; is_error : bool
      ; images : Image.t list [@jsonaf.default []] [@jsonaf_drop_default.equal]
      }
    [@@deriving jsonaf]

    let of_tool_result ({ text; is_error; images } : Tool_result.t) =
      { text; is_error; images }
    ;;

    let to_tool_result { text; is_error; images } =
      { Tool_result.text; is_error; images }
    ;;
  end

  type t =
    | Run of { run : int }
    | Start of
        { id : string
        ; call_id : string
        ; parent : string option [@jsonaf.option]
        ; task : string
        ; model : string
        ; at : float
        ; run : int
        }
    | Message of
        { id : string
        ; at : float
        ; turns : int
        ; tool_calls : int
        ; message : Message.t
        }
    | End of
        { id : string
        ; at : float
        ; run : int
        ; turns : int
        ; tool_calls : int
        ; result : Result.t
        }
  [@@deriving jsonaf]
end

type t =
  { entries : Entry.t Queue.t (** oldest first *)
  ; by_id : Entry.t String.Table.t
  ; mutable runs : int
  ; mutable dir : unit -> string option
  }

let create () =
  { entries = Queue.create ()
  ; by_id = String.Table.create ()
  ; runs = 0
  ; dir = (fun () -> None)
  }
;;

let file_name = "subagents.jsonl"

let write t (line : Line.t) =
  Option.iter (t.dir ()) ~f:(fun dir ->
    let path = Filename.concat dir file_name in
    let append () =
      Out_channel.with_file path ~append:true ~f:(fun oc ->
        Out_channel.output_string oc (Json.to_string (Line.jsonaf_of_t line));
        Out_channel.newline oc)
    in
    match line with
    | Run _ -> if Sys_unix.file_exists_exn path then append ()
    | Start _ | Message _ | End _ ->
      Core_unix.mkdir_p dir;
      append ())
;;

let start t ~id ~call_id ~parent ~task ~model ~at =
  let entry =
    { Entry.id
    ; call_id
    ; parent
    ; task
    ; model
    ; started_at = at
    ; updated_at = at
    ; ended = None
    ; result = None
    ; turns = 0
    ; tool_calls = 0
    ; current_tool = None
    ; messages = Queue.create ()
    }
  in
  Hashtbl.set t.by_id ~key:id ~data:entry;
  Queue.enqueue t.entries entry
;;

let end_ t (e : Entry.t) ~at ~result =
  e.result <- Some result;
  e.ended <- Some (at, t.runs);
  e.current_tool <- None
;;

let rec record_in t ~now ~parent (event : Agent_event.t) =
  let touch id f =
    Option.iter (Hashtbl.find t.by_id id) ~f:(fun (e : Entry.t) ->
      e.updated_at <- now;
      f e)
  in
  match event with
  | Agent_start ->
    if Option.is_none parent
    then (
      t.runs <- t.runs + 1;
      write t (Run { run = t.runs }))
  | Subagent_start { call_id; agent_id; task; model; tools = _ } ->
    start t ~id:agent_id ~call_id ~parent ~task ~model ~at:now;
    write
      t
      (Start
         { id = agent_id; call_id; parent; task; model; at = now; run = t.runs })
  | Subagent_end { agent_id; result; _ } ->
    touch agent_id (fun e ->
      end_ t e ~at:now ~result;
      write
        t
        (End
           { id = e.id
           ; at = now
           ; run = t.runs
           ; turns = e.turns
           ; tool_calls = e.tool_calls
           ; result = Line.Result.of_tool_result result
           }))
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
         Queue.enqueue e.messages { Timed_message.message; at = Some now };
         write
           t
           (Message
              { id = e.id
              ; at = now
              ; turns = e.turns
              ; tool_calls = e.tool_calls
              ; message
              }))
     | _ -> touch agent_id ignore)
  | _ -> ()
;;

let record t ~now event = record_in t ~now ~parent:None event

let replay t (line : Line.t) =
  let with_entry id f = Option.iter (Hashtbl.find t.by_id id) ~f in
  match line with
  | Run { run } -> t.runs <- Int.max t.runs run
  | Start { id; call_id; parent; task; model; at; run } ->
    t.runs <- Int.max t.runs run;
    start t ~id ~call_id ~parent ~task ~model ~at
  | Message { id; at; turns; tool_calls; message } ->
    with_entry id (fun e ->
      e.updated_at <- at;
      e.turns <- turns;
      e.tool_calls <- tool_calls;
      Queue.enqueue e.messages { Timed_message.message; at = Some at })
  | End { id; at; run; turns; tool_calls; result } ->
    t.runs <- Int.max t.runs run;
    with_entry id (fun e ->
      e.updated_at <- at;
      e.turns <- turns;
      e.tool_calls <- tool_calls;
      e.result <- Some (Line.Result.to_tool_result result);
      e.ended <- Some (at, run))
;;

let interrupted = "[interrupted] the backend stopped before it finished"

(* Parsed logs: a session opened again after its agent was evicted need not
   parse its log again. *)
let lines : Line.t list File_cache.t =
  File_cache.create ~max_bytes:(64 * 1024 * 1024) ()
;;

let read_lines path =
  Or_error.try_with (fun () ->
    List.filter_map (In_channel.read_lines path) ~f:(fun line ->
      match Line.t_of_jsonaf (Json.of_string line) with
      | line -> Some line
      | exception _ -> None))
;;

let load t ~dir =
  Queue.clear t.entries;
  Hashtbl.clear t.by_id;
  t.runs <- 0;
  t.dir <- dir;
  Option.iter (dir ()) ~f:(fun dir ->
    let path = Filename.concat dir file_name in
    if Sys_unix.file_exists_exn path
    then (
      (match File_cache.find_or_read lines path ~read:read_lines with
       | Ok lines -> List.iter lines ~f:(replay t)
       | Error _ -> ());
      Queue.iter t.entries ~f:(fun e ->
        if Option.is_none e.result
        then
          (* Not written, so that a later load finds the same end. *)
          end_ t e ~at:e.updated_at ~result:(Tool_result.error interrupted))))
;;

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

let ancestors t (e : Entry.t) =
  let rec go acc (e : Entry.t) =
    match Option.bind e.parent ~f:(Hashtbl.find t.by_id) with
    | None -> List.rev acc
    | Some parent -> go (parent :: acc) parent
  in
  go [] e
;;

let orphan_ends t ~alive ~why =
  Queue.to_list t.entries
  |> List.rev
  |> List.filter_map ~f:(fun (e : Entry.t) ->
    let ancestors = ancestors t e in
    let root = Option.value (List.last ancestors) ~default:e in
    if Option.is_some e.result || alive root.id
    then None
    else (
      let end_ =
        Agent_event.Subagent_end
          { call_id = e.call_id
          ; agent_id = e.id
          ; usage = Usage.zero
          ; turns = e.turns
          ; cost_usd = 0.
          ; result = Tool_result.error ("[interrupted] " ^ why root.id)
          }
      in
      Some
        (List.fold ancestors ~init:end_ ~f:(fun event (parent : Entry.t) ->
           Agent_event.Subagent
             { call_id = parent.call_id; agent_id = parent.id; event }))))
;;

let find t key =
  let entry =
    match Hashtbl.find t.by_id key with
    | Some e -> Some e
    | None ->
      Queue.find t.entries ~f:(fun (e : Entry.t) -> String.equal e.call_id key)
  in
  Option.map entry ~f:(fun e -> summary t e, Queue.to_list e.messages)
;;
