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

module Wait_result = struct
  type t =
    { finished : Task.t list
    ; running : Task.t list
    ; timed_out : bool
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; mutable tasks : Task.t list (** oldest first *)
  ; mutable next_subagent_id : int
  ; mutable next_job_id : int
  ; mutable emit : Agent_event.t -> unit
  ; mutable on_change : unit -> unit
  }

let create ~env ~sw ~first_subagent_id ~first_job_id () =
  { env
  ; sw
  ; tasks = []
  ; next_subagent_id = first_subagent_id
  ; next_job_id = first_job_id
  ; emit = ignore
  ; on_change = ignore
  }
;;

let connect t ~emit ~on_change =
  t.emit <- emit;
  t.on_change <- on_change
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
  | _ -> None
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

let spawn t ~kind ~label ~run =
  let id = next_id t kind in
  let finished, resolve = Promise.create () in
  let task =
    { Task.id
    ; kind
    ; label
    ; started_at = now t
    ; cancel = Cancellation.create ()
    ; finished
    ; output = Output_tail.create ()
    ; outcome = None
    ; finished_at = None
    ; activity = "starting"
    ; delivered = false
    }
  in
  t.tasks <- t.tasks @ [ task ];
  let emit event =
    Option.iter (activity event) ~f:(fun a -> task.activity <- a);
    t.emit event
  in
  Fiber.fork ~sw:t.sw (fun () ->
    let outcome =
      match
        run
          ~id
          ~cancel:task.cancel
          ~emit
          ~on_output:(Output_tail.add task.output)
      with
      | outcome -> outcome
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        { Outcome.status = "failed"
        ; body = sprintf "%s failed: %s" (Kind.word kind) (Exn.to_string exn)
        ; is_error = true
        }
    in
    task.outcome <- Some outcome;
    task.finished_at <- Some (now t);
    task.activity <- "finished";
    t.on_change ();
    Promise.resolve resolve ());
  t.on_change ();
  id
;;

let tasks t ~kind =
  List.filter t.tasks ~f:(fun (task : Task.t) -> Kind.equal task.kind kind)
;;

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
  Message.user (String.concat ~sep:"\n\n" (List.map tasks ~f:Task.report))
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

let await_any tasks =
  Fiber.any
    (List.map tasks ~f:(fun (task : Task.t) () -> Promise.await task.finished))
;;

let wait t ~kind ~ids ~all ~timeout ~cancel =
  let targets =
    match ids with
    | Some ids -> Or_error.all (List.map ids ~f:(find t ~kind))
    | None ->
      Ok
        (List.filter (tasks t ~kind) ~f:(fun (task : Task.t) ->
           not task.delivered))
  in
  Or_error.map targets ~f:(fun targets ->
    let deadline = Option.map timeout ~f:(fun s -> now t +. s) in
    let rec loop () =
      let running = List.filter targets ~f:Task.running in
      let finished = List.filter targets ~f:(Fn.non Task.running) in
      let done_ =
        List.is_empty running || ((not all) && not (List.is_empty finished))
      in
      let remaining =
        Option.map deadline ~f:(fun deadline -> deadline -. now t)
      in
      let stop timed_out =
        mark_delivered t finished;
        { Wait_result.finished; running; timed_out }
      in
      if done_
      then stop false
      else if Option.exists remaining ~f:(fun r -> Float.(r <= 0.))
      then stop true
      else (
        let woke =
          Cancellation.protect cancel ~f:(fun () ->
            match remaining with
            | None -> await_any running
            | Some r ->
              Fiber.first
                (fun () -> await_any running)
                (fun () -> Eio.Time.sleep (Eio.Stdenv.clock t.env) r))
        in
        match woke with
        | None -> stop false
        | Some () -> loop ())
    in
    loop ())
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
