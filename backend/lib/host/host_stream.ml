open! Core
open! Import

module Exec = struct
  type t =
    stdin:Stdin_feed.t
    -> cancel:Cancellation.t
    -> on_output:(string -> unit)
    -> name:string
    -> Json.t
    -> Tool.Result.t
end

module Ended = struct
  type t =
    | Exited of
        { program : string
        ; exit : Process.Exit.t
        ; stderr_tail : string
        }
    | Not_installed of
        { program : string
        ; host : string
        ; login_shell_path : string option
        }
    | Old_host of string
    | Host_gone of string
    | Broken of string
    | Stopped
  [@@deriving sexp_of]

  let max_tail = 2000

  let to_message = function
    | Exited { program; exit; stderr_tail } ->
      let how =
        match exit with
        | Exited code -> sprintf "exited (code %d)" code
        | Signaled signal -> sprintf "was killed (%s)" (Signal.to_string signal)
        | Timed_out -> "timed out"
        | Cancelled -> "was stopped"
      in
      let tail =
        if String.length stderr_tail > max_tail
        then "..." ^ String.suffix stderr_tail max_tail
        else stderr_tail
      in
      if String.is_empty tail
      then sprintf "%s %s" program how
      else sprintf "%s %s: %s" program how tail
    | Not_installed { program; host; login_shell_path = None } ->
      sprintf "%s is not installed on %s, please install it" program host
    | Not_installed { program; host; login_shell_path = Some path } ->
      sprintf
        "%s is not on the PATH of prigh's tool host on %s (a login shell finds \
         it at %s): add %s to that PATH and restart the tool host"
        program
        host
        path
        (Filename.dirname path)
    | Old_host host ->
      sprintf
        "tool host %s runs an older prigh that cannot run agent CLIs: update \
         prigh there and restart its TUI or tool-host"
        host
    | Host_gone why -> why
    | Broken why -> "the connection to the program broke: " ^ why
    | Stopped -> "stopped"
  ;;
end

module Started = struct
  type t =
    { pid : int
    ; prigh : string
    ; mcp_socket : string option
    }
  [@@deriving sexp_of]
end

module Mcp_event = struct
  type t =
    | Line of
        { conn : int
        ; line : string
        }
    | Closed of int
  [@@deriving sexp_of]
end

module Item = struct
  type t =
    | Line of string
    | End
end

type t =
  { feed : Stdin_feed.t
  ; cancel : Cancellation.t
  ; items : Item.t Eio.Stream.t
  ; ack_every : int
  ; started : (Started.t, Ended.t) Result.t Promise.t
  ; ended : Ended.t Promise.t
  ; mutable taken : int
  ; mutable acked : int
  ; mutable stopped : bool
  ; mutable finished : bool
  }

let started t = t.started
let ended t = t.ended

let send t frame =
  Stdin_feed.write t.feed (Host_process.Down.to_line frame) ~close:false
;;

let write_line t line =
  if t.finished
  then Or_error.error_string "the program has ended"
  else send t (In line)
;;

let close_stdin t = ignore (send t Close : unit Or_error.t)

let mcp_write t ~conn line =
  if t.finished
  then Or_error.error_string "the program has ended"
  else send t (Mcp_in { conn; line })
;;

let mcp_close t ~conn = ignore (send t (Mcp_close conn) : unit Or_error.t)
let interrupt t = ignore (send t (Signal Signal.int) : unit Or_error.t)

let stop t =
  t.stopped <- true;
  Cancellation.cancel t.cancel
;;

let read_line t =
  match Eio.Stream.take t.items with
  | End ->
    Eio.Stream.add t.items End;
    None
  | Line line ->
    t.taken <- t.taken + 1;
    if t.taken - t.acked >= t.ack_every
    then (
      t.acked <- t.taken;
      ignore (send t (Ack t.taken) : unit Or_error.t));
    Some line
;;

let ended_of_result ~program ~host (result : Tool.Result.t) : Ended.t =
  match Host_process.Outcome.of_result result with
  | Some (Ran { exit; stderr_tail }) -> Exited { program; exit; stderr_tail }
  | Some (Not_found { program = _; login_shell_path }) ->
    Not_installed { program; host; login_shell_path }
  | Some (Failed message) -> Broken message
  | None ->
    let text = String.strip result.text in
    if
      String.is_prefix
        text
        ~prefix:(sprintf "unknown host tool %S" Host_process.op)
    then Old_host host
    else if String.equal text "[cancelled]"
    then Stopped
    else if String.is_prefix text ~prefix:"[the tool host"
    then Host_gone text
    else Broken text
;;

let start
      ~sw
      ~(exec : Exec.t)
      ~host
      ?(on_stderr = ignore)
      ?(on_mcp = ignore)
      (args : Host_process.Arguments.t)
  =
  let program = Filename.basename (List.hd_exn args.argv) in
  let started, started_resolver = Promise.create () in
  let ended, ended_resolver = Promise.create () in
  let t =
    { feed = Stdin_feed.create ()
    ; cancel = Cancellation.create ()
    ; items = Eio.Stream.create Int.max_value
    ; ack_every = Int.max 1 (args.window / 2)
    ; started
    ; ended
    ; taken = 0
    ; acked = 0
    ; stopped = false
    ; finished = false
    }
  in
  let broken = ref None in
  let expected = ref 1 in
  let partial = Buffer.create 256 in
  let break why =
    if Option.is_none !broken then broken := Some why;
    Cancellation.cancel t.cancel
  in
  let frame line =
    match Host_process.Up.of_line line with
    | Error e -> break (Error.to_string_hum e)
    | Ok (Started { pid; path = _; prigh; mcp_socket }) ->
      if not (Promise.is_resolved started)
      then
        Promise.resolve started_resolver (Ok { Started.pid; prigh; mcp_socket })
    | Ok (Mcp { conn; line }) -> on_mcp (Mcp_event.Line { conn; line })
    | Ok (Mcp_closed conn) -> on_mcp (Mcp_event.Closed conn)
    | Ok (Out { seq; line }) ->
      if seq <> !expected
      then break (sprintf "expected line %d but got line %d" !expected seq)
      else (
        incr expected;
        Eio.Stream.add t.items (Line line))
    | Ok (Err text) -> on_stderr text
    | Ok (Err_dropped bytes) ->
      on_stderr (sprintf "[%d bytes of stderr were not forwarded]\n" bytes)
  in
  let on_output chunk =
    if Option.is_none !broken
    then (
      Buffer.add_string partial chunk;
      let text = Buffer.contents partial in
      Buffer.clear partial;
      match String.rsplit2 text ~on:'\n' with
      | None -> Buffer.add_string partial text
      | Some (complete, rest) ->
        Buffer.add_string partial rest;
        List.iter (String.split complete ~on:'\n') ~f:(fun line ->
          if Option.is_none !broken && not (String.is_empty line)
          then frame line))
  in
  let finish (ended : Ended.t) =
    t.finished <- true;
    Stdin_feed.finish t.feed;
    if not (Promise.is_resolved started)
    then Promise.resolve started_resolver (Error ended);
    Eio.Stream.add t.items End;
    Promise.resolve ended_resolver ended
  in
  Fiber.fork ~sw (fun () ->
    match
      exec
        ~stdin:t.feed
        ~cancel:t.cancel
        ~on_output
        ~name:Host_process.op
        (Host_process.Arguments.to_json args)
    with
    | result ->
      finish
        (match !broken with
         | Some why -> Broken why
         | None when t.stopped -> Stopped
         | None -> ended_of_result ~program ~host result)
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      finish Stopped;
      raise exn
    | exception exn -> finish (Broken (Exn.to_string exn)));
  t
;;
