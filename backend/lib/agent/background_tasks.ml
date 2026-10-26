open! Core
open! Import

let short_task task =
  let line = String.strip task |> String.split_lines |> List.hd in
  let line = Option.value line ~default:"" in
  if String.length line > 60 then String.prefix line 59 ^ "…" else line
;;

module Kind = struct
  type t =
    | Subagent
    | Job
  [@@deriving sexp_of, equal]

  let word = function
    | Subagent -> "subagent"
    | Job -> "job"
  ;;

  let id_prefix = function
    | Subagent -> "a"
    | Job -> "j"
  ;;
end

module Outcome = struct
  type t =
    { status : string
    ; body : string
    ; is_error : bool
    }
  [@@deriving sexp_of]
end

module Task = struct
  type t =
    { id : string
    ; kind : Kind.t
    ; label : string
    ; started_at : float
    ; cancel : Cancellation.t
    ; finished : unit Promise.t
    ; output : Output_tail.t
    ; input : Stdin_feed.t option
    ; inbox : Subagent_inbox.t option
    ; mutable outcome : Outcome.t option
    ; mutable finished_at : float option
    ; mutable activity : string
    ; mutable delivered : bool
    }

  let id t = t.id
  let kind t = t.kind
  let label t = t.label
  let outcome t = t.outcome
  let delivered t = t.delivered
  let started_at t = t.started_at
  let finished_at t = t.finished_at
  let output t = t.output
  let input t = t.input
  let inbox t = t.inbox
  let running t = Option.is_none t.outcome

  let report t =
    let word = Kind.word t.kind in
    match t.outcome with
    | None -> sprintf "[%s %s running] %s" word t.id (short_task t.label)
    | Some outcome ->
      let header =
        sprintf "[%s %s %s] %s" word t.id outcome.status (short_task t.label)
      in
      if String.is_empty outcome.body
      then header
      else header ^ "\n" ^ outcome.body
  ;;
end

module Summary = struct
  type t =
    { id : string
    ; label : string
    ; running : bool
    ; status : string option
    }
  [@@deriving sexp_of]
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; mutable tasks : Task.t list (** oldest first *)
  ; mutable next_subagent_id : int
  ; mutable next_job_id : int
  ; mutable emit : Agent_event.t -> unit
  ; mutable on_change : unit -> unit
  ; mutable on_queue_change : unit -> unit
  ; mutable auto_background : unit -> float option
  }

let create ~env ~sw ~first_subagent_id ~first_job_id () =
  { env
  ; sw
  ; tasks = []
  ; next_subagent_id = first_subagent_id
  ; next_job_id = first_job_id
  ; emit = ignore
  ; on_change = ignore
  ; on_queue_change = ignore
  ; auto_background = (fun () -> None)
  }
;;

let connect
      ?(auto_background = fun () -> None)
      ?(on_queue_change = ignore)
      t
      ~emit
      ~on_change
  =
  t.emit <- emit;
  t.on_change <- on_change;
  t.on_queue_change <- on_queue_change;
  t.auto_background <- auto_background
;;

let auto_background t = t.auto_background ()

let seconds_text s =
  if Float.is_integer s then sprintf "%.0fs" s else sprintf "%gs" s
;;

let now t = Eio.Time.now (Eio.Stdenv.clock t.env)

let rec activity (event : Agent_event.t) =
  match event with
  | Subagent { event; _ } -> activity event
  | Tool_start call ->
    let args =
      String.map call.arguments ~f:(fun c ->
        if Char.is_whitespace c then ' ' else c)
    in
    Some (sprintf "%s %s" call.name (String.prefix args 60))
  | Message_start (Assistant _) -> Some "waiting for the model"
  | Message_update { delta = Thinking_delta _; _ } -> Some "thinking"
  | Message_update { delta = Text_delta _; _ } -> Some "writing"
  | Retry { attempt; max_attempts; error; delay } ->
    Some
      (sprintf
         "retrying (attempt %d/%d) in %s after: %s"
         attempt
         max_attempts
         (seconds_text delay)
         (String.prefix error 80))
  | _ -> None
;;

let avoid_subagent_ids t ids =
  List.iter ids ~f:(fun id ->
    Option.iter
      (Option.bind
         (String.chop_prefix id ~prefix:(Kind.id_prefix Subagent))
         ~f:Int.of_string_opt)
      ~f:(fun n -> t.next_subagent_id <- Int.max t.next_subagent_id (n + 1)))
;;

let next_id t (kind : Kind.t) =
  let n =
    match kind with
    | Subagent ->
      let n = t.next_subagent_id in
      t.next_subagent_id <- n + 1;
      n
    | Job ->
      let n = t.next_job_id in
      t.next_job_id <- n + 1;
      n
  in
  Kind.id_prefix kind ^ Int.to_string n
;;

let make_task ?input ?inbox ?output t ~kind ~label ~started_at ~cancel =
  let finished, resolve = Promise.create () in
  let task =
    { Task.id = next_id t kind
    ; kind
    ; label
    ; started_at
    ; cancel
    ; finished
    ; output = Option.value_or_thunk output ~default:Output_tail.create
    ; input
    ; inbox
    ; outcome = None
    ; finished_at = None
    ; activity = "starting"
    ; delivered = false
    }
  in
  t.tasks <- t.tasks @ [ task ];
  task, resolve
;;

let finish t (task : Task.t) outcome =
  task.outcome <- Some outcome;
  task.finished_at <- Some (now t);
  task.activity <- "finished"
;;

let failed_outcome kind exn =
  { Outcome.status = "failed"
  ; body = sprintf "%s failed: %s" (Kind.word kind) (Exn.to_string exn)
  ; is_error = true
  }
;;

(* The agent's switch is going away: the task is not running any more,
   whatever happens next. *)
let finish_on_switch_exit t task resolve =
  finish t task { Outcome.status = "cancelled"; body = ""; is_error = true };
  task.delivered <- true;
  Promise.resolve resolve ()
;;

let spawn ?input ?inbox t ~kind ~label ~run =
  Option.iter inbox ~f:(fun inbox ->
    Subagent_inbox.set_on_change inbox (fun () -> t.on_queue_change ()));
  let task, resolve =
    make_task
      ?input
      ?inbox
      t
      ~kind
      ~label
      ~started_at:(now t)
      ~cancel:(Cancellation.create ())
  in
  let emit event =
    Option.iter (activity event) ~f:(fun a -> task.activity <- a);
    t.emit event
  in
  Fiber.fork ~sw:t.sw (fun () ->
    let outcome =
      match
        run
          ~id:task.id
          ~cancel:task.cancel
          ~emit
          ~on_output:(Output_tail.add task.output)
      with
      | outcome -> outcome
      | exception (Eio.Cancel.Cancelled _ as exn) ->
        Option.iter inbox ~f:(fun inbox ->
          ignore (Subagent_inbox.close inbox : string list));
        finish_on_switch_exit t task resolve;
        raise exn
      | exception exn -> failed_outcome kind exn
    in
    (* Before it counts as finished, so no message is accepted that nothing
       would take. *)
    Option.iter inbox ~f:(fun inbox ->
      ignore (Subagent_inbox.close inbox : string list));
    finish t task outcome;
    (* Its report is still delivered (and its waiters woken) when the
       agent's bookkeeping fails; the switch is the server's. *)
    Guarded.run ~what:"a background task's report" (fun () -> t.on_change ());
    Promise.resolve resolve ());
  t.on_change ();
  task.id
;;

module Foreground = struct
  type 'a t =
    | Finished of 'a
    | Moved of Task.t
end

let run_foreground ?input t ~kind ~label ~after ~cancel ~on_output ~run ~outcome
  =
  let started_at = now t in
  let work_cancel = Cancellation.create () in
  let output = Output_tail.create () in
  let moved = ref None in
  let result, resolve_result = Promise.create () in
  let sink chunk =
    Output_tail.add output chunk;
    if Option.is_none !moved then on_output chunk
  in
  (* The work runs in the tasks' switch, not the caller's fiber, so that it
     can outlive the call once it is moved. *)
  Fiber.fork ~sw:t.sw (fun () ->
    let ran =
      match run ~cancel:work_cancel ~on_output:sink with
      | value -> Ok value
      | exception exn -> Error exn
    in
    match !moved, ran with
    | None, _ -> Promise.resolve resolve_result ran
    | Some (task, resolve), Error (Eio.Cancel.Cancelled _ as exn) ->
      finish_on_switch_exit t task resolve;
      raise exn
    | Some ((task : Task.t), resolve), ran ->
      let outcome =
        match ran with
        | Ok value ->
          (try outcome value ~output with
           | exn -> failed_outcome kind exn)
        | Error exn -> failed_outcome kind exn
      in
      finish t task outcome;
      Guarded.run ~what:"a background task's report" (fun () -> t.on_change ());
      Promise.resolve resolve ());
  let finished = function
    | Ok value -> Foreground.Finished value
    | Error exn -> raise exn
  in
  let waited =
    try
      Cancellation.protect cancel ~f:(fun () ->
        Fiber.first
          (fun () -> `Done (Promise.await result))
          (fun () ->
             Eio.Time.sleep (Eio.Stdenv.clock t.env) after;
             `Timeout))
    with
    | Eio.Cancel.Cancelled _ as exn ->
      Cancellation.cancel work_cancel;
      raise exn
  in
  match waited with
  | Some (`Done ran) -> finished ran
  | None ->
    Cancellation.cancel work_cancel;
    finished (Promise.await result)
  | Some `Timeout ->
    (match Promise.peek result with
     | Some ran -> finished ran
     | None ->
       let task, resolve =
         make_task ?input ~output t ~kind ~label ~started_at ~cancel:work_cancel
       in
       task.activity <- "running";
       moved := Some (task, resolve);
       t.on_change ();
       Moved task)
;;

let tasks t ~kind =
  List.filter t.tasks ~f:(fun (task : Task.t) -> Kind.equal task.kind kind)
;;

let running_count t = List.count t.tasks ~f:Task.running

let has_running ?kind t =
  List.exists t.tasks ~f:(fun task ->
    Task.running task
    && Option.value_map kind ~default:true ~f:(Kind.equal task.kind))
;;

let summaries t ~kind =
  List.filter_map (tasks t ~kind) ~f:(fun (task : Task.t) ->
    if task.delivered
    then None
    else
      Some
        { Summary.id = task.id
        ; label = task.label
        ; running = Task.running task
        ; status = Option.map task.outcome ~f:(fun (o : Outcome.t) -> o.status)
        })
;;

let mark_delivered t tasks =
  let fresh =
    List.filter tasks ~f:(fun (task : Task.t) -> not task.delivered)
  in
  List.iter fresh ~f:(fun (task : Task.t) -> task.delivered <- true);
  if not (List.is_empty fresh) then t.on_change ()
;;

let undelivered t =
  List.filter t.tasks ~f:(fun (task : Task.t) ->
    (not task.delivered) && not (Task.running task))
;;

let has_undelivered t = not (List.is_empty (undelivered t))

let take_undelivered t =
  let tasks = undelivered t in
  mark_delivered t tasks;
  tasks
;;

let delivery_message tasks =
  let reports = List.map tasks ~f:Task.report in
  Message.user
    ~origin:
      { Injected.empty with
        source =
          Some
            (Delivery
               { reports =
                   List.filter_map reports ~f:(fun report ->
                     String.chop_prefix report ~prefix:"["
                     |> Option.bind ~f:(String.lsplit2 ~on:']')
                     |> Option.map ~f:fst)
               })
      }
    (String.concat ~sep:"\n\n" reports)
;;

let find t ~kind id =
  let candidates = tasks t ~kind in
  match
    List.find candidates ~f:(fun (task : Task.t) -> String.equal task.id id)
  with
  | Some task -> Ok task
  | None ->
    Or_error.errorf
      "unknown %s %S; known: %s"
      (Kind.word kind)
      id
      (match candidates with
       | [] -> "none"
       | tasks -> String.concat ~sep:", " (List.map tasks ~f:Task.id))
;;

let inbox_of t id =
  match String.lsplit2 id ~on:'/' with
  | Some (parent, _) ->
    Or_error.errorf
      "%s runs inside %s (a subagent's own subagent) and takes no messages: \
       send it to %s"
      id
      parent
      parent
  | None ->
    Or_error.bind (find t ~kind:Subagent id) ~f:(fun task ->
      match task.inbox with
      | None -> Or_error.errorf "subagent %s takes no messages" id
      | Some inbox ->
        if Task.running task && Subagent_inbox.is_open inbox
        then Ok inbox
        else if task.delivered
        then
          Or_error.errorf
            "%s has finished and its report was delivered to the main agent: \
             tell the main agent instead, or have it start a new subagent"
            id
        else
          Or_error.errorf
            "%s has finished and its report is on its way to the main agent: \
             tell the main agent instead, or have it start a new subagent"
            id)
;;

let send t ~id kind ~from text =
  if String.is_empty (String.strip text)
  then Or_error.error_string "the message is empty: type what to tell the agent"
  else
    Or_error.bind (inbox_of t id) ~f:(fun inbox ->
      Subagent_inbox.push inbox kind ~from text)
;;

let queues t =
  List.filter_map (tasks t ~kind:Subagent) ~f:(fun task ->
    Option.bind task.inbox ~f:(fun inbox ->
      match Subagent_inbox.texts inbox with
      | [], [] -> None
      | steer, follow_up ->
        Some { Subagent_inbox.Listing.agent_id = task.id; steer; follow_up }))
;;

let await_any tasks =
  Fiber.any
    (List.map tasks ~f:(fun (task : Task.t) () -> Promise.await task.finished))
;;

let rec take_next ?interrupt t ~cancel =
  match take_undelivered t with
  | _ :: _ as tasks -> tasks
  | [] ->
    (match List.filter t.tasks ~f:Task.running with
     | [] -> []
     | running ->
       let wait () =
         match interrupt with
         | None ->
           await_any running;
           `Finished
         | Some interrupt ->
           Fiber.first
             (fun () ->
                await_any running;
                `Finished)
             (fun () ->
                interrupt ();
                `Interrupted)
       in
       (match Cancellation.protect cancel ~f:wait with
        | None | Some `Interrupted -> []
        | Some `Finished -> take_next ?interrupt t ~cancel))
;;

let cancel t ~kind id =
  Or_error.bind (find t ~kind id) ~f:(fun task ->
    if Task.running task
    then (
      Cancellation.cancel task.cancel;
      Ok ())
    else Or_error.errorf "%s %s already finished" (Kind.word kind) id)
;;

let cancel_and_wait t ~kind id ~cancel =
  Or_error.bind (find t ~kind id) ~f:(fun task ->
    Cancellation.cancel task.cancel;
    match
      Cancellation.protect cancel ~f:(fun () -> Promise.await task.finished)
    with
    | None ->
      Or_error.errorf "interrupted while cancelling %s %s" (Kind.word kind) id
    | Some () ->
      mark_delivered t [ task ];
      Ok task)
;;

let cancel_all ?(discard = false) t =
  List.iter t.tasks ~f:(fun task ->
    if Task.running task then Cancellation.cancel task.cancel);
  if discard then mark_delivered t t.tasks
;;

let forget_finished t = t.tasks <- List.filter t.tasks ~f:Task.running

let rec wait_all t =
  match List.filter t.tasks ~f:Task.running with
  | [] -> ()
  | running ->
    await_any running;
    wait_all t
;;

let bytes_text n =
  if n < 1024
  then sprintf "%d B" n
  else if n < 1024 * 1024
  then sprintf "%.1f KB" (Float.of_int n /. 1024.)
  else sprintf "%.1f MB" (Float.of_int n /. 1024. /. 1024.)
;;

let status_text t ~kind =
  match tasks t ~kind with
  | [] ->
    (match kind with
     | Subagent -> "no subagents"
     | Job -> "no jobs")
  | tasks ->
    let now = now t in
    String.concat
      ~sep:"\n"
      (List.map tasks ~f:(fun (task : Task.t) ->
         let state =
           match task.outcome with
           | None -> "running"
           | Some outcome when task.delivered -> outcome.status ^ ", delivered"
           | Some outcome -> outcome.status
         in
         let elapsed =
           Option.value task.finished_at ~default:now -. task.started_at
         in
         let last =
           match kind with
           | Subagent -> task.activity
           | Job ->
             sprintf
               "%s, %s"
               (bytes_text (Output_tail.total_bytes task.output))
               (Option.value
                  (Output_tail.last_line task.output)
                  ~default:"no output"
                |> short_task)
         in
         sprintf
           "%s  %s  %.0fs  %s  (last: %s)"
           task.id
           state
           elapsed
           (short_task task.label)
           last))
;;
