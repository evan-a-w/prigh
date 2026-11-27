open! Core
open! Import

module Key = struct
  module T = struct
    type t =
      { host : string
      ; adapter : string
      ; member : string option
      ; login_key : string option
      ; no_sandbox : bool
      ; argv : string list
      ; env_set : (string * string) list
      ; env_unset : string list
      }
    [@@deriving sexp_of, compare, hash, equal]
  end

  include T
  include Hashable.Make_plain (T)
end

module Token_info = struct
  type t =
    { token : string
    ; expiring : now_ms:int -> bool
    ; secret_digest : string option
    }
end

module Entry = struct
  type t =
    { id : int
    ; key : Key.t
    ; mux : Harness_mux.t
    ; token_info : Token_info.t option
    ; mutable draining : bool
    ; mutable idle_cancel : Cancellation.t option
    }
end

module Boot_outcome = struct
  type t =
    | Pooled of Entry.t
    | Single_use of Harness_mux.t
end

module Slot = struct
  type t =
    | Booting of
        { id : int
        ; result : Boot_outcome.t Or_error.t Promise.t
        ; cancel : Cancellation.t
        }
    | Ready of Entry.t
end

type t =
  { pool_sw : Switch.t
  ; stop_signal : unit Promise.t
  ; stop_pool : unit Promise.u
  ; idle_timeout : Time_float.Span.t
  ; now_ms : unit -> int
  ; sleep : float -> unit
  ; active : Slot.t Key.Table.t
  ; draining : Entry.t Int.Table.t
  ; single_use : (string * Harness_mux.t) Int.Table.t
  ; mutable next_id : int
  ; mutable closed : bool
  }

let cancel_idle (entry : Entry.t) =
  Option.iter entry.idle_cancel ~f:Cancellation.cancel;
  entry.idle_cancel <- None
;;

let remove_active_entry t (entry : Entry.t) =
  match Hashtbl.find t.active entry.key with
  | Some (Ready e) when e.id = entry.id -> Hashtbl.remove t.active entry.key
  | Some _ | None -> ()
;;

let stop_entry t (entry : Entry.t) =
  cancel_idle entry;
  remove_active_entry t entry;
  Hashtbl.remove t.draining entry.id;
  Harness_mux.stop entry.mux
;;

let shutdown_entries t =
  t.closed <- true;
  let active_entries, booting_cancels =
    Hashtbl.fold
      t.active
      ~init:([], [])
      ~f:(fun ~key:_ ~data (ready, booting) ->
        match data with
        | Ready entry -> entry :: ready, booting
        | Booting { cancel; _ } -> ready, cancel :: booting)
  in
  Hashtbl.clear t.active;
  let draining_entries = Hashtbl.data t.draining in
  Hashtbl.clear t.draining;
  let single_use_muxes = Hashtbl.data t.single_use |> List.map ~f:snd in
  Hashtbl.clear t.single_use;
  List.iter booting_cancels ~f:Cancellation.cancel;
  List.iter active_entries ~f:(stop_entry t);
  List.iter draining_entries ~f:(stop_entry t);
  List.iter single_use_muxes ~f:Harness_mux.stop
;;

let create
      ~sw
      ?(idle_timeout = Time_float.Span.of_sec 600.)
      ?(now_ms = Credential.now_ms)
      ?(sleep = Eio_unix.sleep)
      ()
  =
  let sw_ready, set_sw = Promise.create () in
  let stop_signal, stop_pool = Promise.create () in
  let t_ref = ref None in
  (* Run pooled servers under a daemon supervisor's switch so warm idle
     servers do not hold the parent switch open when it finishes, and are
     cleaned up automatically if the parent switch closes without [shutdown]. *)
  Fiber.fork_daemon ~sw (fun () ->
    Switch.run (fun pool_sw ->
      Promise.resolve set_sw pool_sw;
      Exn.protect
        ~f:(fun () -> Promise.await stop_signal)
        ~finally:(fun () -> Option.iter !t_ref ~f:shutdown_entries));
    `Stop_daemon);
  let pool_sw = Promise.await sw_ready in
  let t =
    { pool_sw
    ; stop_signal
    ; stop_pool
    ; idle_timeout
    ; now_ms
    ; sleep
    ; active = Key.Table.create ()
    ; draining = Int.Table.create ()
    ; single_use = Int.Table.create ()
    ; next_id = 1
    ; closed = false
    }
  in
  t_ref := Some t;
  t
;;

let drain_entry t (entry : Entry.t) =
  if not entry.draining
  then (
    entry.draining <- true;
    cancel_idle entry;
    remove_active_entry t entry;
    if Harness_mux.active_sessions entry.mux = 0
    then stop_entry t entry
    else Hashtbl.set t.draining ~key:entry.id ~data:entry)
;;

let start_idle_timer t (entry : Entry.t) =
  cancel_idle entry;
  let cancel = Cancellation.create () in
  entry.idle_cancel <- Some cancel;
  Fiber.fork_daemon ~sw:t.pool_sw (fun () ->
    Guarded.run ~what:"harness pool idle timer" (fun () ->
      let timed_out =
        Cancellation.protect cancel ~f:(fun () ->
          t.sleep (Time_float.Span.to_sec t.idle_timeout))
      in
      match timed_out with
      | Some ()
        when (not entry.draining)
             && Harness_mux.alive entry.mux
             && Harness_mux.active_sessions entry.mux = 0 -> stop_entry t entry
      | Some () | None -> ());
    `Stop_daemon)
;;

let needs_renewal
      t
      (entry : Entry.t)
      ~(token_info : Token_info.t option)
      ~login_refused
  =
  match entry.token_info with
  | None -> false
  | Some existing ->
    let refused =
      Option.equal String.equal login_refused (Some existing.token)
    in
    let expiring = existing.expiring ~now_ms:(t.now_ms ()) in
    let secret_changed =
      match existing.secret_digest, token_info with
      | Some d1, Some { secret_digest = Some d2; _ } -> not (String.equal d1 d2)
      | _ -> false
    in
    refused || expiring || secret_changed
;;

let rec acquire
          t
          ~protocol
          ~key
          ?(poolable = true)
          ?token_info
          ?login_refused
          ?request_timeout
          ~spawn
          ()
  =
  if t.closed
  then Or_error.error_string "harness server pool is shut down"
  else if not poolable
  then spawn ~sw:t.pool_sw
  else (
    match Hashtbl.find t.active key with
    | Some (Ready entry) ->
      if not (Harness_mux.alive entry.mux)
      then (
        remove_active_entry t entry;
        acquire
          t
          ~protocol
          ~key
          ~poolable
          ?token_info
          ?login_refused
          ?request_timeout
          ~spawn
          ())
      else if needs_renewal t entry ~token_info ~login_refused
      then (
        drain_entry t entry;
        acquire
          t
          ~protocol
          ~key
          ~poolable
          ?token_info
          ?login_refused
          ?request_timeout
          ~spawn
          ())
      else (
        cancel_idle entry;
        Ok (Harness_mux.lease entry.mux))
    | Some (Booting { id = _; result; cancel = _ }) ->
      (match Promise.await result with
       | Error e -> Error e
       | Ok (Single_use _) ->
         (* The initiating caller discovered this agent/host is not poolable
            and took exclusive ownership of that process: spawn our own. *)
         spawn ~sw:t.pool_sw
       | Ok (Pooled _) ->
         acquire
           t
           ~protocol
           ~key
           ~poolable
           ?token_info
           ?login_refused
           ?request_timeout
           ~spawn
           ())
    | None ->
      let boot_id = t.next_id in
      t.next_id <- boot_id + 1;
      let result, resolve = Promise.create () in
      let boot_cancel = Cancellation.create () in
      Hashtbl.set
        t.active
        ~key
        ~data:(Booting { id = boot_id; result; cancel = boot_cancel });
      let is_still_booting () =
        match Hashtbl.find t.active key with
        | Some (Booting { id; _ }) -> id = boot_id
        | Some _ | None -> false
      in
      let clear_booting () =
        if is_still_booting () then Hashtbl.remove t.active key
      in
      let entry_ref : Entry.t option ref = ref None in
      let single_use = ref false in
      let initiator_cancelled = ref false in
      let on_empty mux =
        if !single_use
        then (
          Hashtbl.remove t.single_use boot_id;
          Harness_mux.stop mux)
        else
          Option.iter !entry_ref ~f:(fun entry ->
            if entry.draining
            then stop_entry t entry
            else if (not t.closed) && Harness_mux.alive mux
            then start_idle_timer t entry)
      in
      let on_drain _mux =
        Option.iter !entry_ref ~f:(fun entry -> drain_entry t entry)
      in
      let on_exit _mux =
        Hashtbl.remove t.single_use boot_id;
        Option.iter !entry_ref ~f:(fun entry ->
          cancel_idle entry;
          remove_active_entry t entry;
          Hashtbl.remove t.draining entry.id)
      in
      (* Boot on the pool's switch so one caller cancelling does not kill a
         shared boot that another concurrent caller may be awaiting. *)
      Fiber.fork ~sw:t.pool_sw (fun () ->
        let outcome =
          try
            match
              Cancellation.protect boot_cancel ~f:(fun () ->
                match spawn ~sw:t.pool_sw with
                | Error e -> Error e
                | Ok transport ->
                  (match
                     Harness_mux.create
                       ~sw:t.pool_sw
                       ~protocol
                       ?request_timeout
                       ~on_empty
                       ~on_drain
                       ~on_exit
                       transport
                   with
                   | exception exn ->
                     Line_transport.stop transport;
                     raise exn
                   | Error e -> Error e
                   | Ok (mux, false) ->
                     single_use := true;
                     let still = is_still_booting () in
                     clear_booting ();
                     if t.closed || (not still) || !initiator_cancelled
                     then (
                       Harness_mux.stop mux;
                       Or_error.error_string "harness server boot was cancelled")
                     else (
                       Hashtbl.set
                         t.single_use
                         ~key:boot_id
                         ~data:(key.host, mux);
                       Ok (Boot_outcome.Single_use mux))
                   | Ok (mux, true) ->
                     let still = is_still_booting () in
                     if t.closed || not still
                     then (
                       clear_booting ();
                       Harness_mux.stop mux;
                       Or_error.error_string
                         "harness server was evicted while booting")
                     else (
                       let entry : Entry.t =
                         { id = boot_id
                         ; key
                         ; mux
                         ; token_info
                         ; draining = false
                         ; idle_cancel = None
                         }
                       in
                       entry_ref := Some entry;
                       Hashtbl.set t.active ~key ~data:(Ready entry);
                       if !initiator_cancelled then start_idle_timer t entry;
                       Ok (Boot_outcome.Pooled entry))))
            with
            | Some outcome -> outcome
            | None -> Or_error.error_string "harness server boot was cancelled"
          with
          | Eio.Cancel.Cancelled _ as exn ->
            clear_booting ();
            if not (Promise.is_resolved result)
            then
              Promise.resolve
                resolve
                (Or_error.error_string "harness server boot was cancelled");
            raise exn
          | exn ->
            eprintf
              "prigh: harness pool boot failed: %s\n%!"
              (Exn.to_string exn);
            clear_booting ();
            Or_error.of_exn exn
        in
        (match outcome with
         | Error _ -> clear_booting ()
         | Ok _ -> ());
        if not (Promise.is_resolved result) then Promise.resolve resolve outcome);
      let awaited =
        try Promise.await result with
        | exn ->
          initiator_cancelled := true;
          (match Promise.peek result with
           | Some (Ok (Boot_outcome.Single_use mux)) ->
             Hashtbl.remove t.single_use boot_id;
             Harness_mux.stop mux
           | Some (Ok (Boot_outcome.Pooled entry))
             when Harness_mux.active_sessions entry.mux = 0
                  && (not entry.draining)
                  && Harness_mux.alive entry.mux -> start_idle_timer t entry
           | _ -> ());
          raise exn
      in
      (match awaited with
       | Error e -> Error e
       | Ok (Single_use mux) -> Ok (Harness_mux.lease mux)
       | Ok (Pooled entry) ->
         if Harness_mux.alive entry.mux && not entry.draining
         then (
           cancel_idle entry;
           Ok (Harness_mux.lease entry.mux))
         else
           acquire
             t
             ~protocol
             ~key
             ~poolable
             ?token_info
             ?login_refused
             ?request_timeout
             ~spawn
             ()))
;;

let evict_host t ~host =
  let active_to_stop =
    Hashtbl.fold t.active ~init:[] ~f:(fun ~key ~data acc ->
      if String.equal key.host host then (key, data) :: acc else acc)
  in
  List.iter active_to_stop ~f:(fun (key, slot) ->
    match slot with
    | Ready entry -> stop_entry t entry
    | Booting { cancel; _ } ->
      Hashtbl.remove t.active key;
      Cancellation.cancel cancel);
  let draining_to_stop =
    Hashtbl.fold t.draining ~init:[] ~f:(fun ~key:_ ~data:entry acc ->
      if String.equal entry.key.host host then entry :: acc else acc)
  in
  List.iter draining_to_stop ~f:(stop_entry t);
  let single_use_to_stop =
    Hashtbl.fold t.single_use ~init:[] ~f:(fun ~key:id ~data:(h, mux) acc ->
      if String.equal h host then (id, mux) :: acc else acc)
  in
  List.iter single_use_to_stop ~f:(fun (id, mux) ->
    Hashtbl.remove t.single_use id;
    Harness_mux.stop mux)
;;

let shutdown t =
  shutdown_entries t;
  if not (Promise.is_resolved t.stop_signal) then Promise.resolve t.stop_pool ()
;;

let active_count t =
  Hashtbl.count t.active ~f:(function
    | Ready _ -> true
    | Booting _ -> false)
;;

let draining_count t = Hashtbl.length t.draining
