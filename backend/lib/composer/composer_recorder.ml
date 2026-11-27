open! Core
open! Import

module Call = struct
  type t =
    { call : Content.Tool_call.t
    ; canonical : Canonical_tool.t
    ; kind : Adapter_event.Tool_kind.t
    ; parent : string option (* the [Subagent] call it runs under *)
    ; order : int
    ; output : Buffer.t (* streamed so far *)
    ; mutable started : bool (* [Tool_start] emitted *)
    ; mutable ended : bool
    }

  let is_subagent t =
    match t.kind with
    | Subagent -> true
    | _ -> false
  ;;
end

(* The assistant message being streamed. *)
module Open_message = struct
  type t =
    { builder : Assistant_builder.t
    ; stream : int
    ; mutable seq : int
    ; mutable calls : Call.t list (* reversed *)
    ; mutable last : [ `Text of string | `Thinking of string | `Call ] option
    }
end

(* An ended assistant message whose calls are not all answered yet. *)
module Step = struct
  type t =
    { assistant : Message.Assistant.t
    ; calls : Call.t list
    ; mutable results : Message.Tool_result.t list (* reversed *)
    }
end

type t =
  { adapter : string
  ; mutable model : string
  ; cwd : string (* the session's: where the harness's subagents work *)
  ; emit : Agent_event.t -> unit
  ; notice : string -> unit
  ; mutable added : Message.t list (* reversed *)
  ; mutable open_ : Open_message.t option
  ; mutable steps : Step.t list
  ; calls : Call.t String.Table.t
  ; mutable next_order : int
  ; streamed : Buffer.t String.Table.t (* text and thinking items *)
  ; approvals : Adapter_event.Approval_reply.t String.Table.t
  ; mutable usage : Usage.t (* not yet attached to a message *)
  ; mutable cost_usd : float
  ; mutable context : int option * int option
  ; mutable last_input : int option
  ; mutable thread_id : string option
  ; mutable stop : Adapter_event.Stop.t option
  ; mutable plans : int
  ; mutable finished : bool
  ; mutable steered : Message.t list (* reversed: waiting for open calls *)
  ; mutable in_turn : bool (* the harness was asked and has not ended *)
  }

let model_name = External_model.name

let create ~adapter ~model ~cwd ~emit ~notice ~prompts =
  emit Agent_event.Agent_start;
  List.iter prompts ~f:(fun m ->
    emit (Agent_event.Message_start m);
    emit (Message_end m));
  { adapter
  ; model = model_name ~adapter model
  ; cwd
  ; emit
  ; notice
  ; added = List.rev prompts
  ; open_ = None
  ; steps = []
  ; calls = String.Table.create ()
  ; next_order = 0
  ; streamed = String.Table.create ()
  ; approvals = String.Table.create ()
  ; usage = Usage.zero
  ; cost_usd = 0.
  ; context = None, None
  ; last_input = None
  ; thread_id = None
  ; stop = None
  ; plans = 0
  ; finished = false
  ; steered = []
  ; in_turn = false
  }
;;

let stop t = t.stop
let thread_id t = t.thread_id
let model t = t.model

let default_model t id =
  if String.equal t.model (model_name ~adapter:t.adapter None)
  then t.model <- model_name ~adapter:t.adapter (Some id)
;;

let cost_usd t = t.cost_usd
let context t = t.context
let last_input t = t.last_input

let note_approval t ~tool_id reply =
  Hashtbl.set t.approvals ~key:tool_id ~data:reply
;;

let error_text (kind : Adapter_event.Error_kind.t) ~message =
  let with_hint hint =
    if String.is_empty message then hint else sprintf "%s: %s" message hint
  in
  match kind with
  | Usage_limit { resets_at } ->
    sprintf
      "%s %s%s"
      (if String.is_empty message then "usage limit" else message)
      Usage_limit.marker
      (Option.value_map resets_at ~default:"" ~f:Usage_limit.reset_note)
  | Auth_required { hint } | Not_installed { hint } -> with_hint hint
  | Crashed { stderr_tail } ->
    if String.is_empty stderr_tail
    then message
    else sprintf "%s\n%s" message (String.rstrip stderr_tail)
  | Protocol | Other -> message
;;

let append_message t m =
  t.added <- m :: t.added;
  t.emit (Message_start m);
  t.emit (Message_end m)
;;

(* [event] as seen from the top: wrapped in a [Subagent] event per
   subagent call it happens under. *)
let rec wrap t ~parent event =
  match parent with
  | None -> event
  | Some id ->
    let outer =
      Option.bind (Hashtbl.find t.calls id) ~f:(fun (c : Call.t) -> c.parent)
    in
    wrap
      t
      ~parent:outer
      (Agent_event.Subagent { call_id = id; agent_id = id; event })
;;

let emit_in t ~parent event = t.emit (wrap t ~parent event)

let delta t (o : Open_message.t) event =
  let block = Assistant_builder.apply_block o.builder event in
  o.seq <- o.seq + 1;
  t.emit
    (Message_update
       { delta = event
       ; block
       ; stream = o.stream
       ; seq = o.seq
       ; partial = (fun () -> Assistant_builder.snapshot o.builder)
       })
;;

(* The harness, for people: [Codex], [Claude Code]. *)
let label t =
  match Adapter_id.of_string t.adapter with
  | Ok adapter -> Adapter_id.label adapter
  | Error _ -> t.adapter
;;

let emit_waiting t =
  t.emit (Agent_waiting { agent = label t; starting = false })
;;

let starting t = t.emit (Agent_waiting { agent = label t; starting = true })

let waiting t =
  t.in_turn <- true;
  emit_waiting t
;;

(* Between steps of a turn: the harness works on, silent until it says
   more. *)
let waiting_again t =
  if t.in_turn && Option.is_none t.open_ && List.is_empty t.steps
  then emit_waiting t
;;

let open_new t =
  t.emit Turn_start;
  emit_waiting t;
  let o =
    { Open_message.builder = Assistant_builder.create ~model:t.model
    ; stream = Agent_loop.next_stream ()
    ; seq = 0
    ; calls = []
    ; last = None
    }
  in
  t.emit (Message_start (Assistant (Assistant_builder.snapshot o.builder)));
  t.open_ <- Some o;
  o
;;

let start_call t (c : Call.t) =
  if not c.started
  then (
    c.started <- true;
    emit_in t ~parent:c.parent (Tool_start c.call);
    if Call.is_subagent c
    then (
      let task =
        match c.canonical.arguments with
        | `Object fields ->
          (match List.Assoc.find fields ~equal:String.equal "task" with
           | Some (`String task) -> task
           | _ -> c.canonical.native_name)
        | _ -> c.canonical.native_name
      in
      emit_in
        t
        ~parent:c.parent
        (Subagent_start
           { call_id = c.call.id
           ; agent_id = c.call.id
           ; task
           ; name = c.canonical.native_name
           ; model = t.model
           ; thinking = Thinking.default
           ; tools = []
           ; host = None
           ; engine = None
           ; cwd = t.cwd
           ; worktree = None
           ; forked = false
           ; continued = false
           });
      emit_in t ~parent:(Some c.call.id) Turn_start))
;;

(* Ends the open message: its calls start. *)
let close t ~stop_reason =
  match t.open_ with
  | None -> ()
  | Some o ->
    t.open_ <- None;
    let usage = t.usage in
    t.usage <- Usage.zero;
    let assistant = Assistant_builder.finish o.builder ~stop_reason ~usage in
    t.added <- Assistant assistant :: t.added;
    t.emit (Message_end (Assistant assistant));
    (match List.rev o.calls with
     | [] -> t.emit (Turn_end { assistant; tool_results = [] })
     | calls ->
       t.steps <- t.steps @ [ { Step.assistant; calls; results = [] } ];
       List.iter calls ~f:(start_call t))
;;

(* The open message, for text or thinking: one whose calls were made is
   ended first. *)
let message_for_text t =
  match t.open_ with
  | Some o when List.is_empty o.calls -> o
  | Some _ ->
    close t ~stop_reason:Tool_use;
    open_new t
  | None -> open_new t
;;

let in_open_message t (c : Call.t) =
  match t.open_ with
  | Some o -> List.exists o.calls ~f:(fun c' -> phys_equal c c')
  | None -> false
;;

let streamed t item =
  Hashtbl.find_or_add t.streamed item ~default:(fun () -> Buffer.create 256)
;;

let text t ~thinking ~item s =
  let o = message_for_text t in
  let same_kind_other_item =
    match o.last, thinking with
    | Some (`Text other), false | Some (`Thinking other), true ->
      not (String.equal other item)
    | _ -> false
  in
  let s = if same_kind_other_item then "\n\n" ^ s else s in
  o.last <- Some (if thinking then `Thinking item else `Text item);
  delta t o (if thinking then Thinking_delta s else Text_delta s)
;;

let text_delta t ~thinking ~item s =
  if not (String.is_empty s)
  then (
    Buffer.add_string (streamed t item) s;
    text t ~thinking ~item s)
;;

(* The whole text of an item: what the deltas missed, if they were a
   prefix of it. *)
let text_done t ~thinking ~item whole =
  let so_far = Buffer.contents (streamed t item) in
  match String.chop_prefix whole ~prefix:so_far with
  | Some rest -> text_delta t ~thinking ~item rest
  | None -> ()
;;

(* Leaves the open message if [c] is one of its calls, so it starts. *)
let ensure_started t (c : Call.t) =
  if in_open_message t c then close t ~stop_reason:Tool_use;
  start_call t c
;;

let tool_start t ~id ~kind ~native_name ~input ~parent =
  let canonical = Canonical_tool.of_start ~kind ~native_name ~input in
  let call =
    { Content.Tool_call.id
    ; name = canonical.name
    ; arguments = Json.to_string canonical.arguments
    ; extra = None
    }
  in
  let parent =
    Option.filter parent ~f:(fun p ->
      match Hashtbl.find t.calls p with
      | Some c -> Call.is_subagent c
      | None -> false)
  in
  let c =
    { Call.call
    ; canonical
    ; kind
    ; parent
    ; order = t.next_order
    ; output = Buffer.create 0
    ; started = false
    ; ended = false
    }
  in
  t.next_order <- t.next_order + 1;
  Hashtbl.set t.calls ~key:id ~data:c;
  match parent with
  | Some p ->
    Option.iter (Hashtbl.find t.calls p) ~f:(ensure_started t);
    start_call t c
  | None ->
    let o = Option.value_or_thunk t.open_ ~default:(fun () -> open_new t) in
    let index = List.length o.calls in
    o.calls <- c :: o.calls;
    o.last <- Some `Call;
    delta t o (Tool_call_start { index; id; name = call.name });
    delta t o (Tool_call_delta { index; arguments = call.arguments })
;;

let denied_text : Adapter_event.Approval_reply.t option -> string option =
  function
  | Some (Deny None) -> Some "[denied by user]"
  | Some (Deny (Some reason)) ->
    Some (sprintf "[denied by user] Their reason: %s" reason)
  | Some Cancel -> Some "[cancelled]"
  | Some (Allow | Allow_always) | None -> None
;;

let rec end_call t (c : Call.t) ~ok ~output ~changes ~images =
  if not c.ended
  then (
    ensure_started t c;
    c.ended <- true;
    let text =
      let denied =
        if ok then None else denied_text (Hashtbl.find t.approvals c.call.id)
      in
      match denied with
      | Some text -> text
      | None ->
        let output =
          if String.is_empty output then Buffer.contents c.output else output
        in
        let text =
          Canonical_tool.result_text c.canonical ~ok ~output ~changes
        in
        if String.is_empty text && not ok then "[failed]" else text
    in
    let result =
      { Message.Tool_result.tool_call_id = c.call.id
      ; tool_name = c.call.name
      ; text
      ; is_error = not ok
      ; images
      }
    in
    if Call.is_subagent c
    then (
      end_pending t ~under:(Some c.call.id);
      emit_in
        t
        ~parent:c.parent
        (Subagent_end
           { call_id = c.call.id
           ; agent_id = c.call.id
           ; usage = Usage.zero
           ; turns = 1
           ; cost_usd = 0.
           ; result =
               { Tool_result.text = result.text
               ; is_error = result.is_error
               ; images = result.images
               }
           }));
    emit_in t ~parent:c.parent (Tool_end { call = c.call; result });
    if Option.is_none c.parent
    then (
      append_message t (Tool_result result);
      List.iter t.steps ~f:(fun step ->
        if List.exists step.calls ~f:(fun c' -> phys_equal c c')
        then step.results <- result :: step.results);
      let done_, waiting =
        List.partition_tf t.steps ~f:(fun step ->
          List.for_all step.calls ~f:(fun (c : Call.t) -> c.ended))
      in
      t.steps <- waiting;
      List.iter done_ ~f:(fun step ->
        t.emit
          (Turn_end
             { assistant = step.assistant
             ; tool_results = List.rev step.results
             }));
      if not (List.is_empty done_) then waiting_again t))

(* Ends, as cancelled (or denied), the calls still running: those under the
   subagent call [under], or all; the latest first, so a subagent's calls
   end before it. *)
and end_pending t ~under =
  let rec is_under (c : Call.t) =
    match under, c.parent with
    | None, _ -> true
    | Some _, None -> false
    | Some id, Some p ->
      String.equal id p
      || Option.value_map (Hashtbl.find t.calls p) ~default:false ~f:is_under
  in
  Hashtbl.data t.calls
  |> List.filter ~f:(fun (c : Call.t) -> (not c.ended) && is_under c)
  |> List.sort ~compare:(fun (a : Call.t) b -> Int.compare b.order a.order)
  |> List.iter ~f:(fun (c : Call.t) ->
    let so_far = Buffer.contents c.output in
    let output =
      if String.is_empty so_far
      then "[cancelled]"
      else sprintf "%s\n[cancelled]" (String.rstrip so_far)
    in
    end_call t c ~ok:false ~output ~changes:[] ~images:[])
;;

let plan t (plan : Plan.t) =
  t.plans <- t.plans + 1;
  let id = sprintf "plan-%d" t.plans in
  let canonical = Canonical_tool.of_plan plan in
  tool_start
    t
    ~id
    ~kind:Todo
    ~native_name:canonical.native_name
    ~input:canonical.arguments
    ~parent:None;
  Option.iter (Hashtbl.find t.calls id) ~f:(fun c ->
    end_call
      t
      c
      ~ok:true
      ~output:
        (sprintf
           "Plan updated (%d of %d steps completed)"
           (List.count plan.steps ~f:(fun s ->
              Plan.Status.equal s.status Completed))
           (List.length plan.steps))
      ~changes:[]
      ~images:[])
;;

let flush_steered t =
  let steered = List.rev t.steered in
  t.steered <- [];
  List.iter steered ~f:(append_message t)
;;

let user_message t m =
  t.steered <- m :: t.steered;
  match t.open_ with
  | _ when not (List.is_empty t.steps) -> ()
  | Some o when not (List.is_empty o.calls) -> ()
  | Some _ ->
    close t ~stop_reason:End_turn;
    flush_steered t;
    waiting_again t
  | None ->
    flush_steered t;
    waiting_again t
;;

let turn_end t (stop : Adapter_event.Stop.t) =
  t.in_turn <- false;
  t.stop <- Some stop;
  let stop_reason : Stop_reason.t =
    match stop with
    | Done -> End_turn
    | Interrupted -> Aborted
    | Failed { kind; message } -> Error (error_text kind ~message)
  in
  (match t.open_, stop with
   | Some o, Done when not (List.is_empty o.calls) ->
     close t ~stop_reason:Tool_use
   | Some _, _ -> close t ~stop_reason
   | None, Failed _ ->
     ignore (open_new t : Open_message.t);
     close t ~stop_reason
   | None, Done when not (Usage.equal t.usage Usage.zero) ->
     ignore (open_new t : Open_message.t);
     close t ~stop_reason
   | None, (Done | Interrupted) -> ());
  end_pending t ~under:None;
  flush_steered t
;;

let feed t (event : Adapter_event.t) =
  match event with
  | Thread_started { thread_id; model } ->
    t.thread_id <- Some thread_id;
    Option.iter model ~f:(fun model ->
      t.model <- model_name ~adapter:t.adapter (Some model))
  | Turn_started _ ->
    t.stop <- None;
    t.in_turn <- true
  | Text_delta { item; text } -> text_delta t ~thinking:false ~item text
  | Text_done { item; text } -> text_done t ~thinking:false ~item text
  | Thinking_delta { item; text } -> text_delta t ~thinking:true ~item text
  | Thinking_done { item; text } -> text_done t ~thinking:true ~item text
  | Tool_start { id; kind; native_name; title = _; input; parent } ->
    if not (Hashtbl.mem t.calls id)
    then tool_start t ~id ~kind ~native_name ~input ~parent
  | Tool_update { id; output } ->
    Option.iter (Hashtbl.find t.calls id) ~f:(fun c ->
      if not c.ended
      then (
        ensure_started t c;
        if not (String.is_empty output)
        then (
          Buffer.add_string c.output output;
          emit_in
            t
            ~parent:c.parent
            (Tool_output { call_id = id; chunk = output }))))
  | Tool_end { id; ok; output; changes; images } ->
    Option.iter (Hashtbl.find t.calls id) ~f:(fun c ->
      end_call t c ~ok ~output ~changes ~images)
  | Plan p -> plan t p
  | Usage { usage; cost_usd; context_tokens; context_window } ->
    t.usage <- Usage.add t.usage usage;
    if usage.input > 0 then t.last_input <- Some usage.input;
    Option.iter cost_usd ~f:(fun c -> t.cost_usd <- t.cost_usd +. c);
    let tokens, window = t.context in
    t.context
    <- ( Option.first_some context_tokens tokens
       , Option.first_some context_window window )
  | Compacted { tokens_before; tokens_after } ->
    t.notice
      (sprintf
         "%s compacted its context%s"
         (label t)
         (match tokens_before, tokens_after with
          | Some before, Some after -> sprintf " (%d → %d tokens)" before after
          | _ -> ""))
  | Retrying { attempt; message } ->
    t.notice
      (sprintf
         "%s is retrying%s: %s"
         (label t)
         (Option.value_map attempt ~default:"" ~f:(sprintf " (attempt %d)"))
         message)
  | Notice message -> t.notice message
  | Turn_end { turn_id = _; stop } -> turn_end t stop
  | Turn_diff _
  | Approval_request _
  | User_question _
  | Request_withdrawn _
  | Session_title _ -> ()
;;

let finish t =
  if not t.finished
  then (
    t.finished <- true;
    match t.stop with
    | Some _ ->
      close t ~stop_reason:End_turn;
      end_pending t ~under:None;
      flush_steered t
    | None -> turn_end t Interrupted);
  let added = List.rev t.added in
  t.emit (Agent_end added);
  added
;;

let call_started t ~tool_id =
  match Hashtbl.find t.calls tool_id with
  | None -> None
  | Some c ->
    if not c.ended then ensure_started t c;
    Some (c.canonical, c.parent)
;;
