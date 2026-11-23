open! Core
open! Import

module Exit = struct
  type t =
    { code : int option
    ; stderr_tail : string
    }
  [@@deriving sexp_of]
end

type t =
  { read_line : unit -> string option
  ; write_line : string -> unit
  ; close_stdin : unit -> unit
  ; interrupt : unit -> unit
  ; stop : unit -> unit
  ; ended : unit -> Exit.t
  ; is_ended : unit -> bool
  }

let create ~read_line ~write_line ~close_stdin ~interrupt ~stop ~ended ~is_ended
  =
  { read_line; write_line; close_stdin; interrupt; stop; ended; is_ended }
;;

let read_line t = t.read_line ()
let write_line t line = t.write_line line
let close_stdin t = t.close_stdin ()
let interrupt t = t.interrupt ()
let stop t = t.stop ()
let ended t = t.ended ()
let is_ended t = t.is_ended ()

module Peer = struct
  type transport = t

  exception Exited of int

  type t =
    { input : string option Eio.Stream.t (* what we wrote; [None]: EOF *)
    ; output : string option Eio.Stream.t (* what it wrote; [None]: done *)
    ; stderr : Buffer.t
    ; mutable interrupted : bool
    ; mutable input_closed : bool
    }

  let read_line t = Eio.Stream.take t.input

  let write_line t line =
    (* A peer writing a frame of several lines keeps them apart. *)
    List.iter (String.split_lines line) ~f:(fun l ->
      Eio.Stream.add t.output (Some l))
  ;;

  let write_stderr t s = Buffer.add_string t.stderr s

  let take_interrupt t =
    let i = t.interrupted in
    t.interrupted <- false;
    i
  ;;

  let exit _ ~code = raise (Exited code)

  let spawn ~sw f =
    let p =
      { input = Eio.Stream.create Int.max_value
      ; output = Eio.Stream.create Int.max_value
      ; stderr = Buffer.create 16
      ; interrupted = false
      ; input_closed = false
      }
    in
    let exited, set_exited = Promise.create () in
    let peer_switch = ref None in
    let close_input () =
      if not p.input_closed
      then (
        p.input_closed <- true;
        Eio.Stream.add p.input None)
    in
    Fiber.fork ~sw (fun () ->
      let code =
        match
          Switch.run (fun psw ->
            peer_switch := Some psw;
            f p)
        with
        | () -> Some 0
        | exception Exited code -> Some code
        | exception Eio.Cancel.Cancelled _ -> None
        | exception Failure _ -> None
      in
      peer_switch := None;
      Eio.Stream.add p.output None;
      Promise.resolve
        set_exited
        { Exit.code; stderr_tail = Buffer.contents p.stderr });
    let finished = ref false in
    { read_line =
        (fun () ->
          if !finished
          then None
          else (
            match Eio.Stream.take p.output with
            | Some _ as line -> line
            | None ->
              finished := true;
              None))
    ; write_line =
        (fun line ->
          if (not p.input_closed) && not (Promise.is_resolved exited)
          then Eio.Stream.add p.input (Some line))
    ; close_stdin = close_input
    ; interrupt = (fun () -> p.interrupted <- true)
    ; stop =
        (fun () ->
          close_input ();
          Option.iter !peer_switch ~f:(fun psw ->
            Switch.fail psw (Failure "stopped")))
    ; ended = (fun () -> Promise.await exited)
    ; is_ended = (fun () -> Promise.is_resolved exited)
    }
  ;;
end
