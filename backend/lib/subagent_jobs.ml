open! Core
open! Import

let short_task task =
  let line = String.strip task |> String.split_lines |> List.hd in
  let line = Option.value line ~default:"" in
  if String.length line > 60 then String.prefix line 59 ^ "…" else line
;;

module Job = struct
  type t =
    { id : string
    ; task : string
    ; started_at : float
    ; cancel : Cancellation.t
    ; finished : unit Promise.t
    ; mutable result : Tool_result.t option
    ; mutable finished_at : float option
    ; mutable activity : string
    ; mutable delivered : bool
    }

  let id t = t.id
  let task t = t.task
  let result t = t.result
  let delivered t = t.delivered
  let running t = Option.is_none t.result

  let report t =
    match t.result with
    | None -> sprintf "[subagent %s running] %s" t.id (short_task t.task)
    | Some result ->
      sprintf
        "[subagent %s %s] %s\n%s"
        t.id
        (if result.is_error then "failed" else "finished")
        (short_task t.task)
        result.text
  ;;
end

module Summary = struct
  type t =
    { id : string
    ; task : string
    ; running : bool
    }
  [@@deriving sexp_of]
end

module Wait_result = struct
  type t =
    { finished : Job.t list
    ; running : Job.t list
    ; timed_out : bool
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; mutable jobs : Job.t list (** oldest first *)
  ; mutable next_id : int
  ; mutable emit : Agent_event.t -> unit
  ; mutable on_change : unit -> unit
  }

let create ~env ~sw ~first_id () =
  { env; sw; jobs = []; next_id = first_id; emit = ignore; on_change = ignore }
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

let spawn t ~task ~run =
  let id = sprintf "a%d" t.next_id in
  t.next_id <- t.next_id + 1;
  let finished, resolve = Promise.create () in
  let job =
    { Job.id
    ; task
    ; started_at = now t
    ; cancel = Cancellation.create ()
    ; finished
    ; result = None
    ; finished_at = None
    ; activity = "starting"
    ; delivered = false
    }
  in
  t.jobs <- t.jobs @ [ job ];
  let emit event =
    Option.iter (activity event) ~f:(fun a -> job.activity <- a);
    t.emit event
  in
  Fiber.fork ~sw:t.sw (fun () ->
    let result =
      match run ~id ~cancel:job.cancel ~emit with
      | result -> result
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        Tool_result.error (sprintf "subagent failed: %s" (Exn.to_string exn))
    in
    job.result <- Some result;
    job.finished_at <- Some (now t);
    job.activity <- "finished";
    t.on_change ();
    Promise.resolve resolve ());
  t.on_change ();
  id
;;

let has_running t = List.exists t.jobs ~f:Job.running

let summaries t =
  List.filter_map t.jobs ~f:(fun (job : Job.t) ->
    if job.delivered
    then None
    else
      Some { Summary.id = job.id; task = job.task; running = Job.running job })
;;

let mark_delivered t jobs =
  let fresh = List.filter jobs ~f:(fun (job : Job.t) -> not job.delivered) in
  List.iter fresh ~f:(fun (job : Job.t) -> job.delivered <- true);
  if not (List.is_empty fresh) then t.on_change ()
;;

let undelivered t =
  List.filter t.jobs ~f:(fun (job : Job.t) ->
    (not job.delivered) && not (Job.running job))
;;

let has_undelivered t = not (List.is_empty (undelivered t))

let take_undelivered t =
  let jobs = undelivered t in
  mark_delivered t jobs;
  jobs
;;

let delivery_message jobs =
  Message.user (String.concat ~sep:"\n\n" (List.map jobs ~f:Job.report))
;;

let find t id =
  match List.find t.jobs ~f:(fun (job : Job.t) -> String.equal job.id id) with
  | Some job -> Ok job
  | None ->
    Or_error.errorf
      "unknown subagent %S; known: %s"
      id
      (match t.jobs with
       | [] -> "none"
       | jobs -> String.concat ~sep:", " (List.map jobs ~f:Job.id))
;;

let await_any jobs =
  Fiber.any
    (List.map jobs ~f:(fun (job : Job.t) () -> Promise.await job.finished))
;;

let wait t ~ids ~all ~timeout ~cancel =
  let targets =
    match ids with
    | Some ids -> Or_error.all (List.map ids ~f:(find t))
    | None ->
      Ok (List.filter t.jobs ~f:(fun (job : Job.t) -> not job.delivered))
  in
  Or_error.map targets ~f:(fun targets ->
    let deadline = Option.map timeout ~f:(fun s -> now t +. s) in
    let rec loop () =
      let running = List.filter targets ~f:Job.running in
      let finished = List.filter targets ~f:(Fn.non Job.running) in
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

let cancel t id =
  Or_error.bind (find t id) ~f:(fun job ->
    if Job.running job
    then (
      Cancellation.cancel job.cancel;
      Ok ())
    else Or_error.errorf "subagent %s already finished" id)
;;

let cancel_and_wait t id ~cancel =
  Or_error.bind (find t id) ~f:(fun job ->
    Cancellation.cancel job.cancel;
    match
      Cancellation.protect cancel ~f:(fun () -> Promise.await job.finished)
    with
    | None -> Or_error.errorf "interrupted while cancelling subagent %s" id
    | Some () ->
      mark_delivered t [ job ];
      Ok job)
;;

let cancel_all ?(discard = false) t =
  List.iter t.jobs ~f:(fun job ->
    if Job.running job then Cancellation.cancel job.cancel);
  if discard then mark_delivered t t.jobs
;;

let rec wait_all t =
  match List.filter t.jobs ~f:Job.running with
  | [] -> ()
  | running ->
    await_any running;
    wait_all t
;;

let status_text t =
  match t.jobs with
  | [] -> "no subagents"
  | jobs ->
    let now = now t in
    String.concat
      ~sep:"\n"
      (List.map jobs ~f:(fun (job : Job.t) ->
         let state =
           match job.result with
           | None -> "running"
           | Some { is_error = true; _ } when job.delivered ->
             "failed, delivered"
           | Some { is_error = true; _ } -> "failed"
           | Some _ when job.delivered -> "finished, delivered"
           | Some _ -> "finished"
         in
         let elapsed =
           Option.value job.finished_at ~default:now -. job.started_at
         in
         sprintf
           "%s  %s  %.0fs  %s  (last: %s)"
           job.id
           state
           elapsed
           (short_task job.task)
           job.activity))
;;
