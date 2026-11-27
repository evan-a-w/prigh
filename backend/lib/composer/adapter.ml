open! Core
open! Import
include Adapter_intf

type t = (module S)

module Driver = struct
  type 'r waiter =
    { matches : 'r -> bool
    ; resolve : 'r Or_error.t Promise.u
    }

  (* A turn the harness runs of its own: what it said, for the notice. *)
  module Orphan = struct
    type t = { mutable text : string option }
  end

  type 'r t =
    { sw : Switch.t
    ; name : string
    ; transport : Line_transport.t
    ; interrupt_grace : Time_float.Span.t
    ; request_timeout : Time_float.Span.t
    ; events : Adapter_event.t Eio.Stream.t
    ; mutable waiters : 'r waiter list
    ; mutable in_turn : bool
    ; mutable turns : int (* turns begun, to tell them apart *)
    ; mutable alive : bool
    ; mutable stopping : bool
    ; mutable orphan : Orphan.t option
    ; idle : Eio.Condition.t (* the orphan turn ended, or the process *)
    ; mutable on_orphan_request : Adapter_event.t -> unit
    ; mutable on_autonomous : (unit -> unit) option
    ; mutable on_title : (string -> unit) option
    ; mutable exit : Line_transport.Exit.t option
    }

  let create
        ~sw
        ~name
        ?(interrupt_grace = Time_float.Span.of_sec 10.)
        ?(request_timeout = Time_float.Span.of_sec 60.)
        transport
    =
    { sw
    ; name
    ; transport
    ; interrupt_grace
    ; request_timeout
    ; events = Eio.Stream.create Int.max_value
    ; waiters = []
    ; in_turn = false
    ; turns = 0
    ; alive = true
    ; stopping = false
    ; orphan = None
    ; idle = Eio.Condition.create ()
    ; on_orphan_request = ignore
    ; on_autonomous = None
    ; on_title = None
    ; exit = None
    }
  ;;

  let alive t = t.alive && not t.stopping
  let write t line = if alive t then Line_transport.write_line t.transport line
  let on_orphan_request t f = t.on_orphan_request <- f
  let on_autonomous t f = t.on_autonomous <- Some f
  let on_title t f = t.on_title <- Some f

  (* Events that belong to no turn in particular go to whoever reads. *)
  let of_no_turn : Adapter_event.t -> bool = function
    | Thread_started _
    | Usage _
    | Notice _
    | Compacted _
    | Retrying _
    | Request_withdrawn _
    | Session_title _ -> true
    | Turn_started _
    | Text_delta _
    | Text_done _
    | Thinking_delta _
    | Thinking_done _
    | Tool_start _
    | Tool_update _
    | Tool_end _
    | Approval_request _
    | User_question _
    | Plan _
    | Turn_diff _
    | Turn_end _ -> false
  ;;

  let orphan_ended t (orphan : Orphan.t) =
    t.orphan <- None;
    Eio.Stream.add
      t.events
      (Notice
         (match orphan.text with
          | None -> sprintf "%s ran a turn of its own" t.name
          | Some text ->
            sprintf
              "%s ran a turn of its own between your messages: %s"
              t.name
              (if String.length text > 2000
               then String.prefix text 2000 ^ "..."
               else text)));
    Eio.Condition.broadcast t.idle
  ;;

  let push t (event : Adapter_event.t) =
    match event with
    | Turn_end _ when t.in_turn ->
      t.in_turn <- false;
      Eio.Stream.add t.events event
    | Session_title title when not t.in_turn ->
      (match t.on_title with
       | Some f -> f title
       | None -> Eio.Stream.add t.events event)
    | _ when t.in_turn || of_no_turn event -> Eio.Stream.add t.events event
    | Turn_end _ ->
      (* A turn of its own ended, or one that was ended for it already (an
         ignored interrupt) ended late. *)
      Option.iter t.orphan ~f:(orphan_ended t)
    | _ ->
      (match t.on_autonomous with
       | Some wake when not t.stopping ->
         t.in_turn <- true;
         t.turns <- t.turns + 1;
         Eio.Stream.add t.events event;
         wake ()
       | Some _ | None ->
         let orphan =
           match t.orphan with
           | Some o -> o
           | None ->
             let o = { Orphan.text = None } in
             t.orphan <- Some o;
             o
         in
         (match event with
          | Text_done { text; _ } when not (String.is_empty (String.strip text))
            -> orphan.text <- Some text
          | Approval_request _ | User_question _ -> t.on_orphan_request event
          | _ -> ()))
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

  let first_lines ?(max = 4) text =
    String.split_lines text
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> Fn.flip List.take max
    |> String.concat ~sep:"\n"
  ;;

  let start_failed ?(hint = fun (_ : Line_transport.Exit.t) -> None) t e =
    let check = sprintf "Check %s in Settings → Engines (/adapters)." t.name in
    match t.exit with
    | Some exit ->
      let how =
        match exit.code with
        | Some code -> sprintf "%s exited before starting (code %d)" t.name code
        | None -> sprintf "%s stopped before starting" t.name
      in
      let what = Option.value (hint exit) ~default:check in
      (match first_lines exit.stderr_tail with
       | "" -> Error.of_string (sprintf "%s. %s" how what)
       | said -> Error.of_string (sprintf "%s:\n%s\n%s" how said what))
    | None ->
      Error.of_string
        (sprintf
           "%s did not start: %s. %s"
           t.name
           (Error.to_string_hum e)
           check)
  ;;

  let request ?timeout t line ~matches =
    if not (alive t)
    then Or_error.error_string "the process exited"
    else (
      let p, resolve = Promise.create () in
      let w = { matches; resolve } in
      t.waiters <- t.waiters @ [ w ];
      let withdraw () =
        t.waiters <- List.filter t.waiters ~f:(fun w' -> not (phys_equal w w'))
      in
      write t line;
      let timeout = Option.value timeout ~default:t.request_timeout in
      match
        Fiber.first
          (fun () -> Promise.await p)
          (fun () ->
             Eio_unix.sleep (Time_float.Span.to_sec timeout);
             Or_error.errorf
               "no answer within %s"
               (Time_float.Span.to_string_hum timeout))
      with
      | result ->
        withdraw ();
        result
      | exception exn ->
        withdraw ();
        raise exn)
  ;;

  let await_idle t =
    match t.orphan with
    | None -> ()
    | Some _ ->
      Eio.Stream.add
        t.events
        (Notice
           (sprintf
              "%s is finishing a turn of its own: your message waits for it"
              t.name));
      let rec wait () =
        if Option.is_some t.orphan && t.alive
        then (
          Eio.Condition.await_no_mutex t.idle;
          wait ())
      in
      wait ()
  ;;

  let begin_turn t =
    t.in_turn <- true;
    t.turns <- t.turns + 1
  ;;

  let in_turn t = t.in_turn

  let stop t =
    if not t.stopping
    then (
      t.stopping <- true;
      (* Its turn ends now: the process may take long to go (a host away
         answers nothing). *)
      if t.in_turn
      then
        push
          t
          (Turn_end
             { turn_id = None
             ; stop =
                 Failed
                   { kind = Crashed { stderr_tail = "" }
                   ; message =
                       sprintf "%s was stopped before its turn ended" t.name
                   }
             });
      Line_transport.stop t.transport)
  ;;

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
      t.exit <- Some exit;
      t.alive <- false;
      t.orphan <- None;
      Eio.Condition.broadcast t.idle;
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
