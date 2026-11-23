open! Core
open! Import
include Adapter_intf

type t = (module S)

module Driver = struct
  type 'r waiter =
    { matches : 'r -> bool
    ; resolve : 'r Or_error.t Promise.u
    }

  type 'r t =
    { sw : Switch.t
    ; transport : Line_transport.t
    ; interrupt_grace : Time_float.Span.t
    ; events : Adapter_event.t Eio.Stream.t
    ; mutable waiters : 'r waiter list
    ; mutable in_turn : bool
    ; mutable turns : int (* turns begun, to tell them apart *)
    ; mutable alive : bool
    }

  let create ~sw ?(interrupt_grace = Time_float.Span.of_sec 10.) transport =
    { sw
    ; transport
    ; interrupt_grace
    ; events = Eio.Stream.create Int.max_value
    ; waiters = []
    ; in_turn = false
    ; turns = 0
    ; alive = true
    }
  ;;

  let write t line = if t.alive then Line_transport.write_line t.transport line

  let push t (event : Adapter_event.t) =
    (match event with
     | Turn_end _ -> t.in_turn <- false
     | _ -> ());
    Eio.Stream.add t.events event
  ;;

  let next_event t = Eio.Stream.take t.events

  let answer t r =
    match List.find t.waiters ~f:(fun w -> w.matches r) with
    | None -> ()
    | Some w ->
      t.waiters <- List.filter t.waiters ~f:(fun w' -> not (phys_equal w w'));
      Promise.resolve w.resolve (Ok r)
  ;;

  let exit_message ?(tail = true) ~display_name (exit : Line_transport.Exit.t) =
    let how =
      match exit.code with
      | Some code -> sprintf "%s exited (code %d)" display_name code
      | None -> sprintf "%s was stopped" display_name
    in
    match String.strip exit.stderr_tail with
    | _ when not tail -> how
    | "" -> how
    | tail -> sprintf "%s: %s" how tail
  ;;

  let request t line ~matches =
    if not t.alive
    then Or_error.error_string "the process exited"
    else (
      let p, resolve = Promise.create () in
      t.waiters <- t.waiters @ [ { matches; resolve } ];
      write t line;
      Promise.await p)
  ;;

  let begin_turn t =
    t.in_turn <- true;
    t.turns <- t.turns + 1
  ;;

  let in_turn t = t.in_turn
  let alive t = t.alive
  let stop t = Line_transport.stop t.transport

  let run t ~handle_line ~on_exit =
    Fiber.fork ~sw:t.sw (fun () ->
      let rec loop () =
        match Line_transport.read_line t.transport with
        | Some line ->
          handle_line line;
          loop ()
        | None -> ()
      in
      loop ();
      let exit = Line_transport.ended t.transport in
      t.alive <- false;
      let waiters = t.waiters in
      t.waiters <- [];
      List.iter waiters ~f:(fun w ->
        Promise.resolve
          w.resolve
          (Or_error.error_string
             (exit_message ~display_name:"the process" exit)));
      on_exit exit;
      if t.in_turn
      then
        push
          t
          (Turn_end
             { turn_id = None
             ; stop =
                 Failed
                   { kind = Crashed { stderr_tail = exit.stderr_tail }
                   ; message =
                       exit_message ~tail:false ~display_name:"the harness" exit
                   }
             }))
  ;;

  let watch_interrupt t =
    let turn = t.turns in
    Fiber.fork_daemon ~sw:t.sw (fun () ->
      Eio_unix.sleep (Time_float.Span.to_sec t.interrupt_grace);
      if t.in_turn && t.turns = turn
      then (
        push t (Turn_end { turn_id = None; stop = Interrupted });
        stop t);
      `Stop_daemon)
  ;;
end
