open! Core
open! Import

let op = "$process"
let number n = `Number (Int.to_string n)

let field json key =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal key
  | _ -> None
;;

let string_field json key =
  match field json key with
  | Some (`String s) -> Some s
  | _ -> None
;;

let int_field json key =
  match field json key with
  | Some (`Number n) -> Int.of_string_opt n
  | _ -> None
;;

let frame fields = Json.to_string (`Object fields) ^ "\n"

let parse_frame line ~f =
  match Json.parse line with
  | Error _ -> Or_error.errorf "not a frame: %s" line
  | Ok json ->
    (match string_field json "t" with
     | None -> Or_error.errorf "not a frame: %s" line
     | Some kind ->
       (match f kind json with
        | Some t -> Ok t
        | None -> Or_error.errorf "bad %s frame: %s" kind line))
;;

module Up = struct
  type t =
    | Started of
        { pid : int
        ; path : string
        ; prigh : string
        ; mcp_socket : string option
        }
    | Out of
        { seq : int
        ; line : string
        }
    | Err of string
    | Err_dropped of int
    | Mcp of
        { conn : int
        ; line : string
        }
    | Mcp_closed of int
  [@@deriving sexp_of, equal]

  let to_line = function
    | Started { pid; path; prigh; mcp_socket } ->
      frame
        ([ "t", `String "started"
         ; "pid", number pid
         ; "path", `String path
         ; "prigh", `String prigh
         ]
         @ Option.value_map mcp_socket ~default:[] ~f:(fun s ->
           [ "mcp_socket", `String s ]))
    | Mcp { conn; line } ->
      frame [ "t", `String "mcp"; "conn", number conn; "line", `String line ]
    | Mcp_closed conn ->
      frame [ "t", `String "mcp_closed"; "conn", number conn ]
    | Out { seq; line } ->
      frame [ "t", `String "out"; "s", number seq; "line", `String line ]
    | Err text -> frame [ "t", `String "err"; "text", `String text ]
    | Err_dropped bytes ->
      frame [ "t", `String "err_dropped"; "bytes", number bytes ]
  ;;

  let of_line line =
    parse_frame line ~f:(fun kind json ->
      let open Option.Let_syntax in
      match kind with
      | "started" ->
        let%bind pid = int_field json "pid" in
        let%bind path = string_field json "path" in
        let%map prigh = string_field json "prigh" in
        Started
          { pid; path; prigh; mcp_socket = string_field json "mcp_socket" }
      | "out" ->
        let%bind seq = int_field json "s" in
        let%map line = string_field json "line" in
        Out { seq; line }
      | "err" -> string_field json "text" >>| fun text -> Err text
      | "err_dropped" -> int_field json "bytes" >>| fun n -> Err_dropped n
      | "mcp" ->
        let%bind conn = int_field json "conn" in
        let%map line = string_field json "line" in
        Mcp { conn; line }
      | "mcp_closed" -> int_field json "conn" >>| fun conn -> Mcp_closed conn
      | _ -> None)
  ;;
end

let signals =
  [ "INT", Signal.int
  ; "TERM", Signal.term
  ; "KILL", Signal.kill
  ; "HUP", Signal.hup
  ]
;;

module Down = struct
  type t =
    | In of string
    | Ack of int
    | Close
    | Signal of Signal.t
    | Mcp_in of
        { conn : int
        ; line : string
        }
    | Mcp_close of int
  [@@deriving sexp_of, equal]

  let to_line = function
    | In line -> frame [ "t", `String "in"; "line", `String line ]
    | Mcp_in { conn; line } ->
      frame [ "t", `String "mcp_in"; "conn", number conn; "line", `String line ]
    | Mcp_close conn -> frame [ "t", `String "mcp_close"; "conn", number conn ]
    | Ack seq -> frame [ "t", `String "ack"; "s", number seq ]
    | Close -> frame [ "t", `String "close" ]
    | Signal signal ->
      let name =
        List.find_map signals ~f:(fun (name, s) ->
          Option.some_if (Signal.equal s signal) name)
        |> Option.value ~default:"TERM"
      in
      frame [ "t", `String "signal"; "sig", `String name ]
  ;;

  let of_line line =
    parse_frame line ~f:(fun kind json ->
      let open Option.Let_syntax in
      match kind with
      | "in" -> string_field json "line" >>| fun line -> In line
      | "ack" -> int_field json "s" >>| fun seq -> Ack seq
      | "close" -> Some Close
      | "mcp_in" ->
        let%bind conn = int_field json "conn" in
        let%map line = string_field json "line" in
        Mcp_in { conn; line }
      | "mcp_close" -> int_field json "conn" >>| fun conn -> Mcp_close conn
      | "signal" ->
        let%bind name = string_field json "sig" in
        List.Assoc.find signals ~equal:String.equal name
        >>| fun signal -> Signal signal
      | _ -> None)
  ;;
end

module Outcome = struct
  type t =
    | Ran of
        { exit : Process.Exit.t
        ; stderr_tail : string
        }
    | Not_found of
        { program : string
        ; login_shell_path : string option
        }
    | Failed of string
  [@@deriving sexp_of]

  let exit_to_json : Process.Exit.t -> Json.t = function
    | Exited code -> `Object [ "code", number code ]
    | Signaled signal -> `Object [ "signal", `String (Signal.to_string signal) ]
    | Timed_out -> `String "timed_out"
    | Cancelled -> `String "cancelled"
  ;;

  let exit_of_json : Json.t -> Process.Exit.t option = function
    | `String "timed_out" -> Some Timed_out
    | `String "cancelled" -> Some Cancelled
    | json ->
      (match int_field json "code", string_field json "signal" with
       | Some code, _ -> Some (Exited code)
       | None, Some signal ->
         Option.try_with (fun () ->
           Process.Exit.Signaled (Signal.of_string signal))
       | None, None -> None)
  ;;

  let to_result = function
    | Ran { exit; stderr_tail } ->
      Tool.Result.ok
        (Json.to_string
           (`Object
               [ "exit", exit_to_json exit; "stderr_tail", `String stderr_tail ]))
    | Not_found { program; login_shell_path } ->
      Tool.Result.error
        (Json.to_string
           (`Object
               [ "error", `String "not_found"
               ; "program", `String program
               ; ( "login_shell_path"
                 , Option.value_map login_shell_path ~default:`Null ~f:(fun p ->
                     `String p) )
               ]))
    | Failed message ->
      Tool.Result.error
        (Json.to_string
           (`Object [ "error", `String "failed"; "message", `String message ]))
  ;;

  let of_result (result : Tool.Result.t) =
    match Json.parse result.text with
    | Error _ -> None
    | Ok json ->
      if not result.is_error
      then (
        let%map.Option exit = Option.bind (field json "exit") ~f:exit_of_json in
        Ran
          { exit
          ; stderr_tail =
              Option.value (string_field json "stderr_tail") ~default:""
          })
      else (
        match string_field json "error" with
        | Some "not_found" ->
          let%map.Option program = string_field json "program" in
          Not_found
            { program; login_shell_path = string_field json "login_shell_path" }
        | Some "failed" ->
          let%map.Option message = string_field json "message" in
          Failed message
        | _ -> None)
  ;;
end

(* A line goes up JSON-escaped twice (in an [out] frame, in the host's
   output chunk), up to 4 bytes a byte for a line full of escapes: this keeps
   the frame under the RPC's 64 MiB, whose excess would drop the host. *)
let max_line = 12 * 1024 * 1024

module Arguments = struct
  type t =
    { argv : string list
    ; env : (string * string) list
    ; env_unset : string list
    ; window : int
    ; max_line : int
    ; stderr_rate : int
    ; term_grace : Time_ns.Span.t
    ; path : string option
    ; mcp : bool
    }

  let create ~argv =
    { argv
    ; env = []
    ; env_unset = []
    ; window = 64
    ; max_line
    ; stderr_rate = 64 * 1024
    ; term_grace = Time_ns.Span.of_int_ms 3000
    ; path = None
    ; mcp = false
    }
  ;;

  let to_json t =
    `Object
      ([ "argv", `Array (List.map t.argv ~f:(fun s -> `String s))
       ; "env", `Object (List.map t.env ~f:(fun (k, v) -> k, `String v))
       ; "window", number t.window
       ; "max_line", number t.max_line
       ; "stderr_rate", number t.stderr_rate
       ; "term_grace_ms", number (Time_ns.Span.to_int_ms t.term_grace)
       ; ("mcp", if t.mcp then `True else `False)
       ]
       @ Option.value_map t.path ~default:[] ~f:(fun p -> [ "path", `String p ])
       @
       if List.is_empty t.env_unset
       then []
       else
         [ "env_unset", `Array (List.map t.env_unset ~f:(fun s -> `String s)) ]
      )
  ;;

  let of_json json =
    let default = create ~argv:[] in
    let int key ~min ~max ~default =
      Int.clamp_exn (Option.value (int_field json key) ~default) ~min ~max
    in
    match field json "argv" with
    | Some (`Array (_ :: _ as argv)) ->
      let%map.Or_error argv =
        List.map argv ~f:(function
          | `String s -> Ok s
          | _ -> Or_error.error_string "argv must be strings")
        |> Or_error.all
      in
      { argv
      ; env =
          (match field json "env" with
           | Some (`Object vars) ->
             List.filter_map vars ~f:(function
               | key, `String value -> Some (key, value)
               | _ -> None)
           | _ -> [])
      ; env_unset =
          (match field json "env_unset" with
           | Some (`Array vars) ->
             List.filter_map vars ~f:(function
               | `String s -> Some s
               | _ -> None)
           | _ -> [])
      ; window = int "window" ~min:1 ~max:4096 ~default:default.window
      ; max_line =
          int "max_line" ~min:1024 ~max:max_line ~default:default.max_line
      ; stderr_rate =
          int
            "stderr_rate"
            ~min:0
            ~max:Int.max_value
            ~default:default.stderr_rate
      ; term_grace =
          Time_ns.Span.of_int_ms
            (int
               "term_grace_ms"
               ~min:0
               ~max:60_000
               ~default:(Time_ns.Span.to_int_ms default.term_grace))
      ; path = string_field json "path"
      ; mcp =
          (match field json "mcp" with
           | Some `True -> true
           | _ -> false)
      }
    | _ -> Or_error.error_string "argv must be a non-empty list of strings"
  ;;
end

let stderr_tail_capacity = 16 * 1024

(* The program's stdout as numbered lines, at most [window] ahead of the
   acks. *)
module Lines = struct
  type t =
    { window : int
    ; max_line : int
    ; send : string -> unit
    ; stopped : Cancellation.t
    ; acked_changed : Eio.Condition.t
    ; partial : Buffer.t
    ; mutable sent : int
    ; mutable acked : int
    ; mutable too_long : bool
    }

  let create ~window ~max_line ~send ~stopped =
    { window
    ; max_line
    ; send
    ; stopped
    ; acked_changed = Eio.Condition.create ()
    ; partial = Buffer.create 256
    ; sent = 0
    ; acked = 0
    ; too_long = false
    }
  ;;

  let ack t seq =
    if seq > t.acked
    then (
      t.acked <- seq;
      Eio.Condition.broadcast t.acked_changed)
  ;;

  (* False once stopped: what is left is dropped. *)
  let rec room t =
    if Cancellation.is_cancelled t.stopped
    then false
    else if t.sent - t.acked < t.window
    then true
    else (
      match
        Cancellation.protect t.stopped ~f:(fun () ->
          Eio.Condition.await_no_mutex t.acked_changed)
      with
      | None -> false
      | Some () -> room t)
  ;;

  (* One chunk per read of the program's output, cut where the window is
     full. *)
  let emit t lines =
    let pending = Buffer.create 256 in
    let flush () =
      if Buffer.length pending > 0
      then (
        t.send (Buffer.contents pending);
        Buffer.clear pending)
    in
    let rec loop = function
      | [] -> flush ()
      | line :: rest ->
        if t.sent - t.acked >= t.window then flush ();
        if room t
        then (
          t.sent <- t.sent + 1;
          Buffer.add_string
            pending
            (Up.to_line (Out { seq = t.sent; line = Utf8.sanitize line }));
          loop rest)
    in
    loop lines
  ;;

  let add t data =
    let lines = ref [] in
    let start = ref 0 in
    String.iteri data ~f:(fun i c ->
      if Char.equal c '\n'
      then (
        Buffer.add_substring t.partial data ~pos:!start ~len:(i - !start);
        lines := Buffer.contents t.partial :: !lines;
        Buffer.clear t.partial;
        start := i + 1));
    Buffer.add_substring
      t.partial
      data
      ~pos:!start
      ~len:(String.length data - !start);
    if Buffer.length t.partial > t.max_line
    then (
      t.too_long <- true;
      Cancellation.cancel t.stopped)
    else emit t (List.rev !lines)
  ;;

  (* A last line without its [\n]. *)
  let finish t =
    if Buffer.length t.partial > 0 && not t.too_long
    then (
      let line = Buffer.contents t.partial in
      Buffer.clear t.partial;
      t.sent <- t.sent + 1;
      t.send (Up.to_line (Out { seq = t.sent; line = Utf8.sanitize line })))
  ;;
end

(* Stderr forwarded up to [rate] bytes a minute, the rest counted. *)
module Stderr = struct
  type t =
    { clock : float Eio.Time.clock_ty Eio.Resource.t
    ; rate : int
    ; send : string -> unit
    ; tail : Output_tail.t
    ; mutable minute_start : float
    ; mutable forwarded : int
    ; mutable dropped : int
    ; mutable pending : string (** the start of a character cut by a read *)
    }

  let create ~clock ~rate ~send =
    { clock
    ; rate
    ; send
    ; tail = Output_tail.create ~capacity:stderr_tail_capacity ()
    ; minute_start = Eio.Time.now clock
    ; forwarded = 0
    ; dropped = 0
    ; pending = ""
    }
  ;;

  let report_dropped t =
    if t.dropped > 0
    then (
      t.send (Up.to_line (Err_dropped t.dropped));
      t.dropped <- 0)
  ;;

  let add t data =
    Output_tail.add t.tail data;
    let now = Eio.Time.now t.clock in
    if Float.(now - t.minute_start >= 60.)
    then (
      report_dropped t;
      t.minute_start <- now;
      t.forwarded <- 0);
    let text, pending = Utf8.split_incomplete_suffix (t.pending ^ data) in
    t.pending <- pending;
    (* Up to the budget, cut where a character starts, whatever the reads
       were. *)
    let room = Int.max 0 (t.rate - t.forwarded) in
    let sent, _ = Utf8.split_incomplete_suffix (String.prefix text room) in
    if not (String.is_empty sent)
    then (
      t.forwarded <- t.forwarded + String.length sent;
      t.send (Up.to_line (Err (Utf8.sanitize sent))));
    t.dropped <- t.dropped + String.length text - String.length sent
  ;;

  let tail t =
    Utf8.sanitize
      (String.strip (String.concat ~sep:"\n" (Output_tail.lines t.tail)))
  ;;
end

(* The MCP side channel: a private Unix socket whose connections (each a
   [prigh mcp-bridge] the program started) are relayed as [Mcp] frames. *)
module Mcp_listener = struct
  module Conn = struct
    type t =
      { write : string -> unit
      ; close : unit -> unit
      }
  end

  type t =
    { socket : string
    ; conns : Conn.t Int.Table.t
    }

  let create ~sw ~env ~max_line ~send =
    let dir = Filename_unix.temp_dir ~perm:0o700 "prigh-mcp" "" in
    let socket = Filename.concat dir "mcp.sock" in
    let t = { socket; conns = Int.Table.create () } in
    (* Released after the listening socket (which Eio may unlink). *)
    Switch.on_release sw (fun () ->
      (try Core_unix.unlink socket with
       | _ -> ());
      try Core_unix.rmdir dir with
      | _ -> ());
    let listening =
      Eio.Net.listen ~sw ~backlog:8 (Eio.Stdenv.net env) (`Unix socket)
    in
    let next = ref 0 in
    let serve conn id =
      let reader = Eio.Buf_read.of_flow conn ~max_size:max_line in
      (try
         let rec loop () =
           let line = Eio.Buf_read.line reader in
           send (Up.to_line (Mcp { conn = id; line = Utf8.sanitize line }));
           loop ()
         in
         loop ()
       with
       | End_of_file | Eio.Buf_read.Buffer_limit_exceeded | Eio.Io _ -> ());
      Hashtbl.remove t.conns id;
      send (Up.to_line (Mcp_closed id))
    in
    Fiber.fork_daemon ~sw (fun () ->
      let rec accept () =
        Eio.Net.accept_fork
          ~sw
          listening
          ~on_error:(fun (_ : exn) -> ())
          (fun conn (_ : Eio.Net.Sockaddr.stream) ->
             incr next;
             let id = !next in
             let quietly f =
               try f () with
               | Eio.Io _ -> ()
             in
             (* Written by a fiber of its own: the backend's frames are
                handled by one fiber, which a client slow to read must not
                hold up (its acks would wait, and so the program). *)
             let outbox = Eio.Stream.create Int.max_value in
             Hashtbl.set
               t.conns
               ~key:id
               ~data:
                 { write = (fun line -> Eio.Stream.add outbox (`Line line))
                 ; close = (fun () -> Eio.Stream.add outbox `Shutdown)
                 };
             let rec drain () =
               match Eio.Stream.take outbox with
               | `Line line ->
                 quietly (fun () -> Eio.Flow.copy_string (line ^ "\n") conn);
                 drain ()
               | `Shutdown ->
                 quietly (fun () -> Eio.Flow.shutdown conn `Send);
                 drain ()
               | `Done -> ()
             in
             Fiber.both
               (fun () ->
                  serve conn id;
                  Eio.Stream.add outbox `Done)
               drain);
        accept ()
      in
      accept ());
    t
  ;;

  let write t ~conn line =
    Option.iter (Hashtbl.find t.conns conn) ~f:(fun c -> c.write line)
  ;;

  let close t ~conn =
    Option.iter (Hashtbl.find t.conns conn) ~f:(fun c -> c.close ())
  ;;
end

(* The backend's frames, from the exec's input to the program. *)
let serve_input ~outer ~inner ~lines ~signal ~mcp =
  let partial = Buffer.create 256 in
  let handle line =
    match Down.of_line line with
    | Error _ -> ()
    | Ok (In line) ->
      ignore
        (Stdin_feed.write inner (line ^ "\n") ~close:false : unit Or_error.t)
    | Ok (Ack seq) -> Lines.ack lines seq
    | Ok Close ->
      ignore (Stdin_feed.write inner "" ~close:true : unit Or_error.t)
    | Ok (Signal s) -> signal s
    | Ok (Mcp_in { conn; line }) ->
      Option.iter mcp ~f:(fun mcp -> Mcp_listener.write mcp ~conn line)
    | Ok (Mcp_close conn) -> Option.iter mcp ~f:(Mcp_listener.close ~conn)
  in
  let rec loop () =
    match Stdin_feed.take outer with
    | Close -> ignore (Stdin_feed.write inner "" ~close:true : unit Or_error.t)
    | Data data ->
      Buffer.add_string partial data;
      let text = Buffer.contents partial in
      Buffer.clear partial;
      let complete, rest =
        match String.rsplit2 text ~on:'\n' with
        | Some (complete, rest) -> String.split complete ~on:'\n', rest
        | None -> [], text
      in
      Buffer.add_string partial rest;
      List.iter complete ~f:(fun line ->
        if not (String.is_empty line) then handle line);
      loop ()
  in
  loop ()
;;

module Placeholder = struct
  let prigh = "{{prigh}}"
  let mcp_socket = "{{mcp_socket}}"

  let fill ~self ~socket s =
    let s = String.substr_replace_all s ~pattern:prigh ~with_:self in
    match socket with
    | Some socket ->
      String.substr_replace_all s ~pattern:mcp_socket ~with_:socket
    | None -> s
  ;;
end

let resolve ~env ~cwd (args : Arguments.t) =
  let program = List.hd_exn args.argv in
  let environment = Executable.environment ~extra:args.env in
  match
    Executable.resolve
      ~environment
      ~dir:cwd
      (Option.value args.path ~default:program)
  with
  | Ok path -> Ok path
  | Error (Not_executable _) ->
    Error (Outcome.Not_found { program; login_shell_path = None })
  | Error Not_on_path ->
    Error
      (Outcome.Not_found
         { program
         ; login_shell_path = Executable.login_shell_path ~env program
         })
;;

let default_self () =
  match Core_unix.readlink "/proc/self/exe" with
  | path -> path
  | exception _ -> Sys_unix.executable_name
;;

let run ~env ~self ~outer ~cancel ~on_output ~cwd (args : Arguments.t) =
  match resolve ~env ~cwd args with
  | Error outcome -> outcome
  | Ok path ->
    let stopped = Cancellation.create () in
    let lines =
      Lines.create
        ~window:args.window
        ~max_line:args.max_line
        ~send:on_output
        ~stopped
    in
    let stderr =
      Stderr.create
        ~clock:(Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t)
        ~rate:args.stderr_rate
        ~send:on_output
    in
    let inner = Stdin_feed.create () in
    let pid = ref None in
    let signal s =
      Option.iter !pid ~f:(fun pid -> Signal_unix.send_i s (`Group pid))
    in
    Switch.run
    @@ fun sw ->
    let mcp =
      Option.some_if args.mcp ()
      |> Option.map ~f:(fun () ->
        Mcp_listener.create ~sw ~env ~max_line:args.max_line ~send:on_output)
    in
    let socket = Option.map mcp ~f:(fun mcp -> mcp.socket) in
    let fill = Placeholder.fill ~self ~socket in
    (* The socket goes only to the bridge, in its MCP config (a
       [{{mcp_socket}}] placeholder): not into the program's environment,
       which every command it runs would inherit. *)
    let extra_env =
      List.map args.env ~f:(fun (key, value) -> key, fill value)
    in
    Fiber.fork_daemon ~sw (fun () ->
      Cancellation.await cancel;
      Cancellation.cancel stopped;
      `Stop_daemon);
    Fiber.fork_daemon ~sw (fun () ->
      serve_input ~outer ~inner ~lines ~signal ~mcp;
      `Stop_daemon);
    (match
       Process.run
         ~env
         ~cwd
         ~extra_env
         ~unset_env:args.env_unset
         ~input:inner
         ~cancel:stopped
         ~term_grace:args.term_grace
         ~on_spawn:(fun p ->
           pid := Some p;
           on_output
             (Up.to_line
                (Started
                   { pid = Pid.to_int p
                   ; path
                   ; prigh = self
                   ; mcp_socket = socket
                   })))
         ~on_stdout:(Lines.add lines)
         ~on_stderr:(Stderr.add stderr)
         ~prog:path
         ~args:(List.map (List.tl_exn args.argv) ~f:fill)
         ()
     with
     | exit ->
       pid := None;
       Lines.finish lines;
       Stderr.report_dropped stderr;
       if lines.too_long
       then
         Outcome.Failed
           (sprintf
              "%s wrote a line over %d bytes, so it was stopped"
              (Filename.basename (List.hd_exn args.argv))
              args.max_line)
       else Ran { exit; stderr_tail = Stderr.tail stderr }
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn ->
       Failed
         (sprintf
            "could not start %s: %s"
            (List.hd_exn args.argv)
            (Exn.to_string exn)))
;;

let execute ~env ?self ~stdin ~cancel ~on_output ~cwd ~arguments () =
  match stdin, Arguments.of_json arguments with
  | None, _ -> Outcome.to_result (Failed (op ^ " needs its stdin"))
  | _, Error e -> Outcome.to_result (Failed (Error.to_string_hum e))
  | Some outer, Ok args ->
    let self =
      match self with
      | Some self -> self
      | None -> default_self ()
    in
    Outcome.to_result (run ~env ~self ~outer ~cancel ~on_output ~cwd args)
;;
