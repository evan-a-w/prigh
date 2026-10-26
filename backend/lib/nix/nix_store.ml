open! Core
open! Import

module Running = struct
  type t =
    { info : Nix_store_status.Running.t
    ; mutable on_progress : Nix_gc_progress.t -> unit
    }
end

type t =
  { sw : Switch.t
  ; now : unit -> float
  ; progress_interval : float
  ; ops : Nix_store_ops.t
  ; mutable running : Running.t option
  ; mutable last : Nix_gc_report.t option (** this backend's last one *)
  }

let create ~sw ~now ?(progress_interval = 0.25) ops =
  { sw; now; progress_interval; ops; running = None; last = None }
;;

let latest a b =
  match a, b with
  | None, x | x, None -> x
  | Some (a : Nix_gc_report.t), Some (b : Nix_gc_report.t) ->
    Some (if Float.(a.at >= b.at) then a else b)
;;

let running t =
  match t.running with
  | Some r -> Some r.info
  | None -> t.ops.running ()
;;

let status t =
  Or_error.map (t.ops.store ()) ~f:(fun store ->
    { Nix_store_status.store
    ; last_gc = latest t.last (t.ops.last_gc ())
    ; running = running t
    ; schedule = t.ops.schedule ()
    })
;;

let describe_by by =
  if String.equal by "schedule" then "the automatic garbage collection" else by
;;

let progress_reporter t (running : Running.t) =
  let tally = Nix_gc_output.Tally.create () in
  let last_sent = ref Float.neg_infinity in
  fun line ->
    let classified = Nix_gc_output.Line.classify line in
    Nix_gc_output.Tally.add tally classified;
    let message =
      match classified with
      | Deleting _ -> None
      | Removing_generation | Summary _ -> Some (String.strip line)
      | Other line -> Option.some_if (not (String.is_empty line)) line
    in
    let now = t.now () in
    if
      Option.is_some message || Float.(now -. !last_sent >= t.progress_interval)
    then (
      last_sent := now;
      running.on_progress
        { Nix_gc_progress.paths_deleted = tally.paths_deleted
        ; generations_deleted = tally.generations_deleted
        ; message
        })
;;

let gc t ~by generations ~on_progress =
  match running t with
  | Some (r : Nix_store_status.Running.t) ->
    Or_error.errorf
      "a Nix garbage collection is already running (started by %s): wait for \
       it to finish"
      (describe_by r.by)
  | None ->
    Or_error.bind (t.ops.check ()) ~f:(fun () ->
      let running =
        { Running.info = { by; since = t.now (); generations }; on_progress }
      in
      t.running <- Some running;
      let done_, resolve = Promise.create () in
      Fiber.fork ~sw:t.sw (fun () ->
        let result =
          match
            t.ops.collect ~by generations ~on_line:(progress_reporter t running)
          with
          | result -> result
          | exception (Eio.Cancel.Cancelled _ as exn) ->
            t.running <- None;
            Promise.resolve
              resolve
              (Or_error.error_string "the backend is shutting down");
            raise exn
          | exception exn -> Or_error.of_exn exn
        in
        t.running <- None;
        Result.iter result ~f:(fun report ->
          t.last <- latest t.last (Some report));
        Promise.resolve resolve result);
      Exn.protect
        ~f:(fun () -> Promise.await done_)
        ~finally:(fun () -> running.on_progress <- ignore))
;;
