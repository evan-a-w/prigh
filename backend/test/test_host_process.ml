open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* A test that waits for what never comes fails rather than hangs. *)
let with_sandbox f =
  with_sandbox (fun t ->
    match
      Eio.Time.with_timeout (Eio.Stdenv.clock t.env) 30. (fun () -> Ok (f t))
    with
    | Ok () -> ()
    | Error `Timeout -> print_endline "TIMED OUT")
;;

let peer () =
  Filename.concat (Sys_unix.getcwd ()) "fake_line_peer/fake_line_peer.exe"
;;

let pid_re = Re.compile (Re.Perl.re {|"pid":[0-9]+|})
let prigh_re = Re.compile (Re.Perl.re {|"prigh":"[^"]*"|})

let mask_frame t line =
  Re.replace_string pid_re ~by:{|"pid":PID|} line
  |> Re.replace_string prigh_re ~by:{|"prigh":"$PRIGH"|}
  |> String.substr_replace_all ~pattern:(peer ()) ~with_:"$PEER"
  |> mask t
;;

(* [$process] run in this process, as the backend runs it on its own host. *)
let exec (t : Tool_test_helpers.t) : Host_stream.Exec.t =
  fun ~stdin ~cancel ~on_output ~name arguments ->
  Host_ops.execute
    ~mcp:None
    ~env:t.env
    ~stdin
    ~cancel
    ~on_output
    ~cwd:t.dir
    ~name
    ~arguments
    ()
;;

let args ?(window = 64) ?(stderr_rate = 64 * 1024) ?max_line ?term_grace argv =
  let a = Host_process.Arguments.create ~argv in
  { a with
    window
  ; stderr_rate
  ; max_line = Option.value max_line ~default:a.max_line
  ; term_grace = Option.value term_grace ~default:a.term_grace
  }
;;

(* The raw frames of one [$process], driven by hand. *)
module Raw = struct
  type t =
    { feed : Stdin_feed.t
    ; chunks : string Eio.Stream.t
    ; result : Tool.Result.t Eio.Promise.or_exn
    ; sandbox : Tool_test_helpers.t
    }

  let start sandbox ~sw arguments =
    let feed = Stdin_feed.create () in
    let chunks = Eio.Stream.create Int.max_value in
    let result =
      Eio.Fiber.fork_promise ~sw (fun () ->
        exec
          sandbox
          ~stdin:feed
          ~cancel:Cancellation.never
          ~on_output:(Eio.Stream.add chunks)
          ~name:Host_process.op
          (Host_process.Arguments.to_json arguments))
    in
    { feed; chunks; result; sandbox }
  ;;

  let print_chunk t chunk = printf "<- %s" (mask_frame t.sandbox chunk)

  (* One chunk: one read of the program's output. A test that waits for one
     that never comes fails rather than hangs. *)
  let next t =
    match
      Eio.Time.with_timeout (Eio.Stdenv.clock t.sandbox.env) 20. (fun () ->
        Ok (Eio.Stream.take t.chunks))
    with
    | Ok chunk -> print_chunk t chunk
    | Error `Timeout -> print_endline "<- (nothing within 20s)"
  ;;

  (* The chunks until the program ended, then how it ended. *)
  let rest t =
    let rec loop () =
      match
        Eio.Fiber.first
          (fun () -> `Chunk (Eio.Stream.take t.chunks))
          (fun () -> `Ended (Eio.Promise.await t.result))
      with
      | `Chunk chunk ->
        print_chunk t chunk;
        loop ()
      | `Ended _ ->
        let rec drain () =
          Option.iter (Eio.Stream.take_nonblocking t.chunks) ~f:(fun chunk ->
            print_chunk t chunk;
            drain ())
        in
        drain ()
    in
    loop ()
  ;;

  let send t frame =
    let line = Host_process.Down.to_line frame in
    printf "-> %s" line;
    Stdin_feed.write t.feed line ~close:false |> ok_exn
  ;;

  let result t =
    rest t;
    let result = Eio.Promise.await_exn t.result in
    print_s
      [%sexp
        (Host_process.Outcome.of_result result : Host_process.Outcome.t option)]
  ;;
end

let%expect_test "frames: no more than the window beyond the acks" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let p = Raw.start t ~sw (args ~window:2 [ peer (); "--flood"; "5" ]) in
  Raw.next p;
  Raw.next p;
  Raw.send p (Ack 2);
  Raw.next p;
  Raw.send p (Ack 4);
  Raw.next p;
  Raw.send p (In "hi");
  Raw.next p;
  Raw.send p (Ack 6);
  Raw.send p Close;
  Raw.result p;
  [%expect
    {|
    <- {"t":"started","pid":PID,"path":"$PEER","prigh":"$PRIGH"}
    <- {"t":"out","s":1,"line":"flood 1"}
    {"t":"out","s":2,"line":"flood 2"}
    -> {"t":"ack","s":2}
    <- {"t":"out","s":3,"line":"flood 3"}
    {"t":"out","s":4,"line":"flood 4"}
    -> {"t":"ack","s":4}
    <- {"t":"out","s":5,"line":"flood 5"}
    -> {"t":"in","line":"hi"}
    <- {"t":"out","s":6,"line":"echo hi"}
    -> {"t":"ack","s":6}
    -> {"t":"close"}
    <- {"t":"err","text":"bye\n"}
    ((Ran (exit (Exited 0)) (stderr_tail bye)))
    |}]
;;

let%expect_test "frames: stderr over its rate is counted, not sent" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let p =
    Raw.start t ~sw (args ~stderr_rate:10 [ peer (); "--stderr"; "25" ])
  in
  Raw.next p;
  Raw.send p Close;
  Raw.result p;
  [%expect
    {|
    <- {"t":"started","pid":PID,"path":"$PEER","prigh":"$PRIGH"}
    -> {"t":"close"}
    <- {"t":"err","text":"eeeeeeeeee"}
    <- {"t":"err_dropped","bytes":19}
    ((Ran (exit (Exited 0)) (stderr_tail eeeeeeeeeeeeeeeeeeeeeeeeebye)))
    |}]
;;

let%expect_test "frames: a character split across reads stays whole" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let p = Raw.start t ~sw (args [ peer (); "--split-utf8"; "--no-newline" ]) in
  Raw.next p;
  Raw.next p;
  Raw.send p Close;
  Raw.result p;
  [%expect
    {|
    <- {"t":"started","pid":PID,"path":"$PEER","prigh":"$PRIGH"}
    <- {"t":"out","s":1,"line":"é"}
    -> {"t":"close"}
    <- {"t":"err","text":"bye\n"}
    <- {"t":"out","s":2,"line":"last"}
    ((Ran (exit (Exited 0)) (stderr_tail bye)))
    |}]
;;

let%expect_test "frames: bad arguments and a missing stdin" =
  with_sandbox
  @@ fun t ->
  let run ?stdin arguments =
    let result =
      Host_ops.execute
        ~mcp:None
        ~env:t.env
        ?stdin
        ~cancel:Cancellation.never
        ~on_output:print_string
        ~cwd:t.dir
        ~name:Host_process.op
        ~arguments
        ()
    in
    print_s [%sexp (result : Tool.Result.t)]
  in
  run (`Object [ "argv", `Array [ `String "x" ] ]);
  run ~stdin:(Stdin_feed.create ()) (`Object [ "argv", `Array [] ]);
  [%expect
    {|
    ((text "{\"error\":\"failed\",\"message\":\"$process needs its stdin\"}")
     (is_error true))
    ((text
      "{\"error\":\"failed\",\"message\":\"argv must be a non-empty list of strings\"}")
     (is_error true))
    |}]
;;

let print_ended stream =
  let ended = Eio.Promise.await (Host_stream.ended stream) in
  print_s [%sexp (ended : Host_stream.Ended.t)];
  print_endline (Host_stream.Ended.to_message ended)
;;

let%expect_test "stream: lines both ways, then the end of input" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let stderr = Buffer.create 16 in
  let s =
    Host_stream.start
      ~sw
      ~exec:(exec t)
      ~host:"laptop"
      ~on_stderr:(Buffer.add_string stderr)
      (args ~window:2 [ peer (); "--flood"; "10" ])
  in
  (match Eio.Promise.await (Host_stream.started s) with
   | Ok (_ : Host_stream.Started.t) -> print_endline "started"
   | Error ended -> print_s [%sexp (ended : Host_stream.Ended.t)]);
  for _ = 1 to 10 do
    ignore (Host_stream.read_line s : string option)
  done;
  Host_stream.write_line s "a" |> ok_exn;
  Host_stream.write_line s {|{"json": "with \"quotes\"\nand a newline"}|}
  |> ok_exn;
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_s [%sexp (Host_stream.read_line s : string option)];
  Host_stream.close_stdin s;
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_ended s;
  print_s [%sexp (Buffer.contents stderr : string)];
  print_s [%sexp (Host_stream.write_line s "late" : unit Or_error.t)];
  [%expect
    {|
    started
    ("echo a")
    ("echo {\"json\": \"with \\\"quotes\\\"\\nand a newline\"}")
    ()
    ()
    (Exited (program fake_line_peer.exe) (exit (Exited 0)) (stderr_tail bye))
    fake_line_peer.exe exited (code 0): bye
    "bye\n"
    (Error "the program has ended")
    |}]
;;

let%expect_test "stream: the program exits by itself" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let s =
    Host_stream.start
      ~sw
      ~exec:(exec t)
      ~host:"laptop"
      (args [ peer (); "--exit-after"; "1" ])
  in
  Host_stream.write_line s "only" |> ok_exn;
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_ended s;
  [%expect
    {|
    ("echo only")
    ()
    (Exited (program fake_line_peer.exe) (exit (Exited 3)) (stderr_tail ""))
    fake_line_peer.exe exited (code 3)
    |}]
;;

let alive pid =
  match Signal_unix.send Signal.zero (`Pid (Pid.of_int pid)) with
  | `Ok -> true
  | `No_such_process -> false
;;

let%expect_test
    "stream: stop ends a program that ignores SIGTERM after the grace"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let s =
    Host_stream.start
      ~sw
      ~exec:(exec t)
      ~host:"laptop"
      (args
         ~term_grace:(Time_ns.Span.of_int_ms 50)
         [ peer (); "--ignore-term"; "--hang-at-eof"; "--flood"; "1" ])
  in
  let pid =
    Eio.Promise.await (Host_stream.started s)
    |> Result.ok
    |> Option.value_exn
    |> fun (s : Host_stream.Started.t) -> s.pid
  in
  print_s [%sexp (Host_stream.read_line s : string option)];
  Host_stream.stop s;
  print_ended s;
  print_s [%sexp (alive pid : bool)];
  print_s [%sexp (Host_stream.read_line s : string option)];
  [%expect
    {|
    ("flood 1")
    Stopped
    stopped
    false
    ()
    |}]
;;

let%expect_test "stream: interrupt sends SIGINT" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let s =
    Host_stream.start ~sw ~exec:(exec t) ~host:"laptop" (args [ peer () ])
  in
  ignore (Eio.Promise.await (Host_stream.started s) : _ Result.t);
  Host_stream.interrupt s;
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_ended s;
  [%expect
    {|
    ()
    (Exited (program fake_line_peer.exe) (exit (Signaled sigint))
     (stderr_tail ""))
    fake_line_peer.exe was killed (sigint)
    |}]
;;

let%expect_test "stream: a line over max_line stops the program" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let s =
    Host_stream.start
      ~sw
      ~exec:(exec t)
      ~host:"laptop"
      (args ~max_line:1024 [ peer (); "--long"; "100000" ])
  in
  print_s [%sexp (Host_stream.read_line s : string option)];
  print_ended s;
  [%expect
    {|
    ()
    (Broken "fake_line_peer.exe wrote a line over 1024 bytes, so it was stopped")
    the connection to the program broke: fake_line_peer.exe wrote a line over 1024 bytes, so it was stopped
    |}]
;;

let%expect_test "stream: not installed, or only on a login shell's PATH" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let run argv =
    let s = Host_stream.start ~sw ~exec:(exec t) ~host:"laptop" (args argv) in
    (match Eio.Promise.await (Host_stream.started s) with
     | Ok _ -> print_endline "started?"
     | Error ended ->
       print_endline (mask_sexp t [%sexp (ended : Host_stream.Ended.t)]));
    print_endline
      (mask
         t
         (Host_stream.Ended.to_message
            (Eio.Promise.await (Host_stream.ended s))))
  in
  run [ "prigh-test-no-such-cli" ];
  write t "home/.bash_profile" {|export PATH="$HOME/cli-bin:$PATH"|};
  write t "home/cli-bin/prigh-test-login-cli" "#!/bin/sh\n";
  Core_unix.chmod
    (Filename.concat t.dir "home/cli-bin/prigh-test-login-cli")
    ~perm:0o755;
  let old_home = Sys.getenv "HOME" in
  Core_unix.putenv ~key:"HOME" ~data:(Filename.concat t.dir "home");
  Exn.protect
    ~f:(fun () -> run [ "prigh-test-login-cli" ])
    ~finally:(fun () ->
      Option.iter old_home ~f:(fun data -> Core_unix.putenv ~key:"HOME" ~data));
  [%expect
    {|
    (Not_installed (program prigh-test-no-such-cli) (host laptop)
     (login_shell_path ()))
    prigh-test-no-such-cli is not installed on laptop, please install it
    (Not_installed (program prigh-test-login-cli) (host laptop)
     (login_shell_path ($DIR/home/cli-bin/prigh-test-login-cli)))
    prigh-test-login-cli is not on the PATH of prigh's tool host on laptop (a login shell finds it at $DIR/home/cli-bin/prigh-test-login-cli): add $DIR/home/cli-bin to that PATH and restart the tool host
    |}]
;;

(* A host that answers with frames and a result of our choosing. *)
let scripted ~frames ~result : Host_stream.Exec.t =
  fun ~stdin:_ ~cancel:_ ~on_output ~name:_ _ ->
  List.iter frames ~f:(fun frame -> on_output (Host_process.Up.to_line frame));
  result
;;

let%expect_test "stream: lost lines, old hosts and hosts that went away" =
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  let run ~frames result =
    let s =
      Host_stream.start
        ~sw
        ~exec:(scripted ~frames ~result)
        ~host:"laptop"
        (args [ "codex"; "app-server" ])
    in
    let rec lines () =
      match Host_stream.read_line s with
      | Some line ->
        print_endline line;
        lines ()
      | None -> ()
    in
    lines ();
    print_ended s
  in
  run
    ~frames:
      [ Started
          { pid = 1
          ; path = "/bin/codex"
          ; prigh = "/bin/prigh"
          ; mcp_socket = None
          }
      ; Out { seq = 1; line = "one" }
      ; Out { seq = 3; line = "three" }
      ]
    (Tool.Result.ok "");
  run ~frames:[] (Tool.Result.error {|unknown host tool "$process"|});
  run
    ~frames:[]
    (Tool.Result.error "[the tool host disconnected during this call]");
  run ~frames:[] (Tool.Result.error "[cancelled]");
  [%expect
    {|
    one
    (Broken "expected line 2 but got line 3")
    the connection to the program broke: expected line 2 but got line 3
    (Old_host laptop)
    tool host laptop runs an older prigh that cannot run agent CLIs: update prigh there and restart its TUI or tool-host
    (Host_gone "[the tool host disconnected during this call]")
    [the tool host disconnected during this call]
    Stopped
    stopped
    |}]
;;

let%expect_test "probe: programs, files and variables, never their contents" =
  with_sandbox
  @@ fun t ->
  write t "bin/fakecli" (sprintf "#!/bin/sh\nexec %s \"$@\"\n" (peer ()));
  Core_unix.chmod (Filename.concat t.dir "bin/fakecli") ~perm:0o755;
  write t "home/.fakecli/auth.json" {|{"secret":"never shown"}|};
  Core_unix.chmod (Filename.concat t.dir "home/.fakecli/auth.json") ~perm:0o600;
  let old_path = Sys.getenv "PATH" in
  let old_home = Sys.getenv "HOME" in
  Core_unix.putenv
    ~key:"PATH"
    ~data:(Filename.concat t.dir "bin" ^ ":" ^ Option.value old_path ~default:"");
  Core_unix.putenv ~key:"HOME" ~data:(Filename.concat t.dir "home");
  Core_unix.putenv ~key:"PRIGH_TEST_PROBE_SET" ~data:"secret value";
  let request =
    { Host_probe.Request.programs =
        [ { name = "fakecli"
          ; version = [ "--version" ]
          ; status = [ "login"; "status" ]
          }
        ; { name = "prigh-test-no-such-cli"
          ; version = [ "--version" ]
          ; status = []
          }
        ]
    ; paths = [ "~/.fakecli/auth.json"; "~/.fakecli/missing.json" ]
    ; env = [ "PRIGH_TEST_PROBE_SET"; "PRIGH_TEST_PROBE_UNSET" ]
    }
  in
  let result =
    Exn.protect
      ~f:(fun () ->
        Host_ops.execute
          ~mcp:None
          ~env:t.env
          ~cancel:Cancellation.never
          ~on_output:ignore
          ~cwd:t.dir
          ~name:Host_probe.op
          ~arguments:(Host_probe.Request.to_json request)
          ())
      ~finally:(fun () ->
        Core_unix.unsetenv "PRIGH_TEST_PROBE_SET";
        Option.iter old_path ~f:(fun data -> Core_unix.putenv ~key:"PATH" ~data);
        Option.iter old_home ~f:(fun data -> Core_unix.putenv ~key:"HOME" ~data))
  in
  print_endline (mask t result.text);
  print_endline
    (mask_sexp
       t
       [%sexp
         (Host_probe.Reply.of_result result : Host_probe.Reply.t Or_error.t)]);
  [%expect
    {|
    {"programs":[{"name":"fakecli","path":"$DIR/bin/fakecli","login_shell_path":null,"version":{"exit":0,"stdout":"fake 1.2.3\n","stderr":""},"status":{"exit":1,"stdout":"","stderr":"Not logged in\n"}},{"name":"prigh-test-no-such-cli","path":null,"login_shell_path":null,"version":null,"status":null}],"paths":[{"path":"$DIR/home/.fakecli/auth.json","exists":true,"size":24,"mode":384},{"path":"$DIR/home/.fakecli/missing.json","exists":false,"size":null,"mode":null}],"env":{"PRIGH_TEST_PROBE_SET":true,"PRIGH_TEST_PROBE_UNSET":false}}
    (Ok
     ((programs
       (((name fakecli) (path ($DIR/bin/fakecli)) (login_shell_path ())
         (version (((exit (Exited 0)) (stdout "fake 1.2.3\n") (stderr ""))))
         (status (((exit (Exited 1)) (stdout "") (stderr "Not logged in\n")))))
        ((name prigh-test-no-such-cli) (path ()) (login_shell_path ())
         (version ()) (status ()))))
      (paths
       (((path $DIR/home/.fakecli/auth.json) (exists true) (size (24))
         (mode (384)))
        ((path $DIR/home/.fakecli/missing.json) (exists false) (size ())
         (mode ()))))
      (env ((PRIGH_TEST_PROBE_SET true) (PRIGH_TEST_PROBE_UNSET false)))))
    |}]
;;

let prigh_exe () = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe"

let%expect_test "stream: the MCP side channel, through prigh mcp-bridge" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let exec : Host_stream.Exec.t =
    fun ~stdin ~cancel ~on_output ~name:_ arguments ->
    Host_process.execute
      ~env:t.env
      ~self:(prigh_exe ())
      ~stdin:(Some stdin)
      ~cancel
      ~on_output
      ~cwd:t.dir
      ~arguments
      ()
  in
  let stream = ref None in
  let on_mcp (event : Host_stream.Mcp_event.t) =
    print_s [%sexp (event : Host_stream.Mcp_event.t)];
    match event, !stream with
    | Line { conn; line = _ }, Some s ->
      Host_stream.mcp_write s ~conn {|{"pong":1}|} |> ok_exn;
      Host_stream.mcp_close s ~conn
    | (Line _ | Closed _), _ -> ()
  in
  let s =
    Host_stream.start
      ~sw
      ~exec
      ~host:"laptop"
      ~on_mcp
      { (Host_process.Arguments.create
           ~argv:
             [ "sh"
             ; "-c"
             ; {|[ "$PRIGH_MCP_SOCKET" = "$2" ] && echo same socket; echo '{"ping":1}' | "$1" mcp-bridge; echo done|}
             ; "sh"
             ; Host_process.Placeholder.prigh
             ; Host_process.Placeholder.mcp_socket
             ])
        with
        mcp = true
      }
  in
  stream := Some s;
  let started =
    Eio.Promise.await (Host_stream.started s) |> Result.ok |> Option.value_exn
  in
  let socket = Option.value_exn started.mcp_socket in
  let perm = sprintf "%o" (Core_unix.stat (Filename.dirname socket)).st_perm in
  print_s
    [%sexp
      (String.equal started.prigh (prigh_exe ()) : bool)
    , (Filename.basename socket : string)
    , (perm : string)];
  let rec lines () =
    match Host_stream.read_line s with
    | Some line ->
      print_endline line;
      lines ()
    | None -> ()
  in
  lines ();
  print_ended s;
  print_s [%sexp (Sys_unix.file_exists_exn (Filename.dirname socket) : bool)];
  [%expect
    {|
    (true mcp.sock 700)
    same socket
    (Line (conn 1) (line "{\"ping\":1}"))
    (Closed 1)
    {"pong":1}
    done
    (Exited (program sh) (exit (Exited 0)) (stderr_tail ""))
    sh exited (code 0)
    false
    |}]
;;
