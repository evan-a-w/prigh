open! Core
open! Prigh
open Prigh_test.Tool_test_helpers

(* Real tmux, behind the real WebSocket endpoint. *)

let visible s =
  String.concat_map s ~f:(function
    | '\027' -> "^["
    | '\r' -> "^M"
    | '\n' -> "^J\n"
    | c -> String.of_char c)
;;

module Fixture = struct
  type nonrec t =
    { sandbox : t
    ; sw : Eio.Switch.t
    ; socket : string (** a path *)
    ; port : int
    ; terminals : Terminals.t
    }

  let tmux t args =
    Process.run_collect
      ~env:t.sandbox.env
      ~prog:"tmux"
      ~args:("-S" :: t.socket :: args)
      ()
  ;;

  let sessions t =
    let output = tmux t [ "list-sessions"; "-F"; "#{session_name}" ] in
    if Process.Exit.is_success output.exit
    then String.split_lines output.stdout
    else []
  ;;

  (* What the shell shows, as tmux renders it. *)
  let screen t name =
    (tmux t [ "capture-pane"; "-p"; "-t"; name ]).stdout
    |> String.split_lines
    |> List.rev
    |> List.drop_while ~f:String.is_empty
    |> List.rev
  ;;

  let clock t = Eio.Stdenv.clock t.sandbox.env

  let eventually ?(timeout = 5.) t f =
    let deadline = Eio.Time.now (clock t) +. timeout in
    let rec go () =
      if f ()
      then true
      else if Float.( > ) (Eio.Time.now (clock t)) deadline
      then false
      else (
        Eio.Time.sleep (clock t) 0.02;
        go ())
    in
    go ()
  ;;

  let print_live t =
    print_s [%sexp (Terminals.live t.terminals : (string * string * int) list)]
  ;;
end

module Peer = struct
  type t =
    { ws : Websocket.t
    ; output : Buffer.t
    ; mutable closed : bool
    ; mutable texts : string list
    }

  let connect (f : Fixture.t) query =
    let flow =
      Eio.Net.connect
        ~sw:f.sw
        (Eio.Stdenv.net f.sandbox.env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, f.port))
    in
    Eio.Flow.copy_string
      (sprintf
         "GET /terminal?%s HTTP/1.1\r\n\
          Host: x\r\n\
          Connection: Upgrade\r\n\
          Upgrade: websocket\r\n\
          Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
          \r\n"
         query)
      flow;
    let reader = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
    let rec skip_headers () =
      match Eio.Buf_read.line reader with
      | "" -> ()
      | _ -> skip_headers ()
    in
    skip_headers ();
    { ws = Websocket.create ~role:`Client ~reader ~flow ()
    ; output = Buffer.create 1024
    ; closed = false
    ; texts = []
    }
  ;;

  (* Reads until [f] holds or the socket closes; [false] on a timeout. *)
  let read_until ?(timeout = 5.) (f : Fixture.t) t pred =
    match
      Eio.Time.with_timeout (Fixture.clock f) timeout (fun () ->
        let rec go () =
          if pred t || t.closed
          then Ok ()
          else (
            (match Websocket.read t.ws with
             | None -> t.closed <- true
             | Some (`Binary data) -> Buffer.add_string t.output data
             | Some (`Text text) -> t.texts <- t.texts @ [ text ]);
            go ())
        in
        go ())
    with
    | Ok () -> pred t
    | Error `Timeout -> false
  ;;

  let saw text t =
    String.is_substring (Buffer.contents t.output) ~substring:text
  ;;

  let type_ t text = Websocket.send_binary t.ws text
  let close t = Websocket.close t.ws
end

let with_fixture ?(idle_timeout = 0.3) ?(heartbeat_timeout = 3.) f =
  with_sandbox
  @@ fun sandbox ->
  Eio.Switch.run
  @@ fun sw ->
  let socket = Filename.concat sandbox.dir "tmux.sock" in
  let terminals =
    Terminals.create
      ~env:sandbox.env
      ~sw
      ~tmux:"tmux"
      ~socket:(Path socket)
      ~idle_timeout:(Time_ns.Span.of_sec idle_timeout)
      ~heartbeat_timeout:(Time_ns.Span.of_sec heartbeat_timeout)
      ~command:[ "env"; "PS1=$ "; "/bin/sh" ]
      ()
  in
  let login =
    Login_manager.create
      ~env:sandbox.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat sandbox.dir "auth.json"))
      ()
  in
  let server =
    Rpc_server.create
      ~env:sandbox.env
      ~sw
      ~token:"sekrit"
      ~login
      ~sessions_dir:(Filename.concat sandbox.dir "sessions")
      ~cwd:sandbox.dir
      ~new_agent:(fun ?session:_ ~cwd:_ () -> assert false)
      ()
  in
  let port =
    Web_server.listen
      ~env:sandbox.env
      ~sw
      ~addr:Eio.Net.Ipaddr.V4.loopback
      ~port:0
      ~root:None
      ~websockets:
        [ ( "/terminal"
          , Web_server.serve_terminal (Rpc_router.single server) terminals )
        ]
      ~on_lines:(fun ~read_line:_ ~write_line:_ -> ())
  in
  let fixture = { Fixture.sandbox; sw; socket; port; terminals } in
  Exn.protect
    ~f:(fun () -> f fixture)
    ~finally:(fun () ->
      Terminals.close_all terminals;
      ignore (Fixture.tmux fixture [ "kill-server" ] : Process.Output.t))
;;

let%expect_test "a terminal: type, share, resize, ping, then idle cleanup" =
  with_fixture
  @@ fun f ->
  let query = "token=sekrit&session=s1&cols=40&rows=8" in
  let a = Peer.connect f query in
  print_s [%sexp (Peer.read_until f a (Peer.saw "$") : bool)];
  Fixture.print_live f;
  Peer.type_ a "echo hel''lo\r";
  print_s [%sexp (Peer.read_until f a (Peer.saw "hello") : bool)];
  let name = "t1-s1" in
  print_s
    [%sexp
      (Fixture.eventually f (fun () ->
         List.equal
           String.equal
           (Fixture.screen f name)
           [ "$ echo hel''lo"; "hello"; "$" ])
       : bool)];
  [%expect
    {|
    true
    ((s1 t1-s1 1))
    true
    true
    |}];
  (* A second viewer gets the screen, then the same live output. *)
  let b = Peer.connect f query in
  print_s [%sexp (Peer.read_until f b (Peer.saw "\027[3;3H") : bool)];
  print_endline (visible (Buffer.contents b.output));
  Fixture.print_live f;
  Peer.type_ b "echo again\r";
  print_s
    [%sexp
      (Peer.read_until f a (Peer.saw "again\r\n")
       && Peer.read_until f b (Peer.saw "again\r\n")
       : bool)];
  [%expect
    {|
    true
    $ echo hel''lo^M^J
    hello^M^J
    $^M^J
    ^M^J
    ^M^J
    ^M^J
    ^M^J
    ^[[0m^[[3;3H
    ((s1 t1-s1 2))
    true
    |}];
  Websocket.send_text a.ws {|{"type":"resize","cols":50,"rows":12}|};
  Websocket.send_text a.ws {|{"type":"ping"}|};
  print_s
    [%sexp
      (Peer.read_until f a (fun p ->
         List.mem p.texts {|{"type":"pong"}|} ~equal:String.equal)
       : bool)];
  print_s
    [%sexp
      (Fixture.eventually f (fun () ->
         String.equal
           (String.strip
              (Fixture.tmux
                 f
                 [ "display-message"
                 ; "-p"
                 ; "-t"
                 ; name
                 ; "#{window_width}x#{window_height}"
                 ])
                .stdout)
           "50x12")
       : bool)];
  [%expect
    {|
    true
    true
    |}];
  (* Closing one socket keeps the terminal; closing the last starts the idle
     timer, which kills it. *)
  Peer.close a;
  print_s
    [%sexp
      (Fixture.eventually f (fun () ->
         List.equal
           [%equal: string * string * int]
           (Terminals.live f.terminals)
           [ "s1", name, 1 ])
       : bool)];
  Peer.close b;
  print_s
    [%sexp
      (Fixture.eventually f (fun () ->
         List.is_empty (Terminals.live f.terminals)
         && List.is_empty (Fixture.sessions f))
       : bool)];
  [%expect
    {|
    true
    true
    |}]
;;

let%expect_test "reconnecting within the idle timeout reattaches" =
  with_fixture ~idle_timeout:2.
  @@ fun f ->
  let query = "token=sekrit&session=s2&cols=30&rows=4" in
  let a = Peer.connect f query in
  ignore (Peer.read_until f a (Peer.saw "$") : bool);
  Peer.type_ a "X=kept\r";
  ignore (Peer.read_until f a (Peer.saw "\r\n$") : bool);
  Peer.close a;
  let b = Peer.connect f query in
  ignore (Peer.read_until f b (Peer.saw "\027[0m") : bool);
  Peer.type_ b "echo $X\r";
  print_s [%sexp (Peer.read_until f b (Peer.saw "\r\nkept") : bool)];
  Fixture.print_live f;
  [%expect
    {|
    true
    ((s2 t1-s2 1))
    |}]
;;

let%expect_test "a silent socket is dropped after the heartbeat timeout" =
  with_fixture ~heartbeat_timeout:0.3 ~idle_timeout:60.
  @@ fun f ->
  let a = Peer.connect f "token=sekrit&session=s3" in
  print_s [%sexp (Peer.read_until f a (fun p -> p.closed) : bool)];
  Fixture.print_live f;
  [%expect
    {|
    true
    ((s3 t1-s3 0))
    |}]
;;

let%expect_test "the shell exiting ends the terminal and tells the client" =
  with_fixture
  @@ fun f ->
  let a = Peer.connect f "token=sekrit&session=s4" in
  ignore (Peer.read_until f a (Peer.saw "$") : bool);
  Peer.type_ a "exit\r";
  print_s [%sexp (Peer.read_until f a (fun p -> p.closed) : bool)];
  print_s [%sexp (a.texts : string list)];
  Fixture.print_live f;
  print_s [%sexp (Fixture.sessions f : string list)];
  (* The next socket for the key starts a fresh shell. *)
  let b = Peer.connect f "token=sekrit&session=s4" in
  ignore (Peer.read_until f b (Peer.saw "$") : bool);
  Fixture.print_live f;
  [%expect
    {|
    true
    ("{\"type\":\"exit\"}")
    ()
    ()
    ((s4 t2-s4 1))
    |}]
;;

let%expect_test "the shell dies with its control client" =
  with_fixture ~idle_timeout:60.
  @@ fun f ->
  let a = Peer.connect f "token=sekrit&session=s5" in
  ignore (Peer.read_until f a (Peer.saw "$") : bool);
  (* What happens to the client when the backend dies: it goes away and
     tmux destroys the unattached session. *)
  let clients =
    (Fixture.tmux f [ "list-clients"; "-F"; "#{client_pid}" ]).stdout
    |> String.split_lines
  in
  print_s [%sexp (List.length clients : int)];
  List.iter clients ~f:(fun pid ->
    Signal_unix.send_i Signal.kill (`Pid (Pid.of_string pid)));
  print_s [%sexp (Peer.read_until f a (fun p -> p.closed) : bool)];
  print_s [%sexp (a.texts : string list)];
  print_s
    [%sexp
      (Fixture.eventually f (fun () -> List.is_empty (Fixture.sessions f))
       : bool)];
  Fixture.print_live f;
  [%expect
    {|
    1
    true
    ("{\"type\":\"exit\"}")
    true
    ()
    |}]
;;

let%expect_test "bad token, unknown session and start failures" =
  with_fixture
  @@ fun f ->
  let a = Peer.connect f "token=nope&session=s6" in
  ignore (Peer.read_until f a (fun p -> p.closed) : bool);
  print_s [%sexp (a.texts : string list)];
  (* No session: the key is "default" and the shell starts in the server's
     directory. *)
  let b = Peer.connect f "token=sekrit" in
  ignore (Peer.read_until f b (Peer.saw "$") : bool);
  Peer.type_ b "pwd\r";
  print_s [%sexp (Peer.read_until f b (Peer.saw f.sandbox.dir) : bool)];
  Fixture.print_live f;
  [%expect
    {|
    ("{\"type\":\"error\",\"message\":\"unauthorised: bad or missing token\"}")
    true
    ((default t1-default 1))
    |}];
  let missing =
    Terminal.create
      ~env:f.sandbox.env
      ~sw:f.sw
      ~tmux:"/nonexistent/tmux"
      ~socket:(Path f.socket)
      ~name:"x"
      ~cwd:f.sandbox.dir
      ~cols:80
      ~rows:24
      ()
  in
  print_s [%sexp (Or_error.ignore_m missing : unit Or_error.t)];
  [%expect
    {| (Error "/nonexistent/tmux not found: install tmux or set PRIGH_TMUX") |}]
;;

module Worker = Prigh_test.Test_tool_host.Worker

(* Reads what a stdio worker writes until [pred]: binary frames go to
   [screen], other lines are printed and kept in [lines]. *)
let read_worker_until w ~screen ~lines pred =
  let rec go () =
    if not (pred ())
    then (
      match Worker.read_line w with
      | None -> print_endline "(eof)"
      | Some line ->
        let json = Jsonaf.of_string line in
        (match Jsonaf.member "kind" json, Jsonaf.member "data" json with
         | Some (`String "binary"), Some (`String data) ->
           Buffer.add_string screen (Base64.decode_exn data)
         | _ ->
           print_endline line;
           lines := line :: !lines);
        go ())
  in
  go ()
;;

let%expect_test "stdio worker: terminals over JSON lines" =
  with_fixture
  @@ fun f ->
  let w = Worker.start f.sandbox ~sw:f.sw ~terminals:(lazy f.terminals) in
  let screen = Buffer.create 1024 in
  let lines = ref [] in
  let read_until pred = read_worker_until w ~screen ~lines pred in
  let saw text () =
    String.is_substring (Buffer.contents screen) ~substring:text
  in
  let said text () =
    List.exists !lines ~f:(String.is_substring ~substring:text)
  in
  let frame term_id kind data =
    Worker.send
      w
      (Jsonaf.to_string
         (`Object
             [ "type", `String "terminal_frame"
             ; "term_id", `String term_id
             ; "kind", `String kind
             ; ( "data"
               , `String
                   (if String.equal kind "binary"
                    then Base64.encode_string data
                    else data) )
             ]))
  in
  Worker.send
    w
    (sprintf
       {|{"type":"terminal_open","term_id":"v1","key":"k1","cwd":"%s","cols":40,"rows":8}|}
       f.sandbox.dir);
  read_until (saw "$");
  frame "v1" "binary" "echo hel''lo\r";
  read_until (saw "hello\r\n");
  frame "v1" "text" {|{"type":"ping"}|};
  read_until (said "pong");
  Fixture.print_live f;
  [%expect
    {|
    {"type":"terminal_frame","term_id":"v1","kind":"text","data":"{\"type\":\"pong\"}"}
    ((k1 t1-k1 1))
    |}];
  (* The frontend closing a viewer gets no reply; the terminal lives on. *)
  Worker.send w {|{"type":"terminal_close","term_id":"v1"}|};
  print_s
    [%sexp
      (Fixture.eventually f (fun () ->
         List.equal
           [%equal: string * string * int]
           (Terminals.live f.terminals)
           [ "k1", "t1-k1", 0 ])
       : bool)];
  [%expect {| true |}];
  (* The shell ending is reported, then the viewer is closed. *)
  Worker.send
    w
    (sprintf
       {|{"type":"terminal_open","term_id":"v2","key":"k1","cwd":"%s"}|}
       f.sandbox.dir);
  Buffer.clear screen;
  read_until (saw "$");
  frame "v2" "binary" "exit\r";
  read_until (said "terminal_closed");
  Worker.close w;
  read_until (fun () -> false);
  [%expect
    {|
    {"type":"terminal_frame","term_id":"v2","kind":"text","data":"{\"type\":\"exit\"}"}
    {"type":"terminal_closed","term_id":"v2"}
    (worker finished)
    (eof)
    |}]
;;

(* The browser's terminal for a session whose tools run on a network tool
   host: the backend relays it, the host runs the shell. *)
let%expect_test "a terminal on a network tool host, relayed end to end" =
  with_sandbox
  @@ fun sandbox ->
  Eio.Switch.run
  @@ fun sw ->
  let terminals ~socket =
    Terminals.create
      ~env:sandbox.env
      ~sw
      ~tmux:"tmux"
      ~socket:(Path (Filename.concat sandbox.dir socket))
      ~idle_timeout:(Time_ns.Span.of_sec 60.)
      ~command:[ "env"; "PS1=$ "; "/bin/sh" ]
      ()
  in
  let backend_terminals = terminals ~socket:"backend.sock" in
  let host_terminals = terminals ~socket:"host.sock" in
  let host_dir = Filename.concat sandbox.dir "host" in
  Core_unix.mkdir_p host_dir;
  let agent, h =
    Prigh_test.Test_rpc.make_server
      ~token:"sekrit"
      sandbox
      ~sw
      ~provider:(Faux_provider.create [])
  in
  let port =
    Web_server.listen
      ~env:sandbox.env
      ~sw
      ~addr:Eio.Net.Ipaddr.V4.loopback
      ~port:0
      ~root:None
      ~websockets:
        [ ( "/terminal"
          , Web_server.serve_terminal
              (Rpc_router.single h.server)
              backend_terminals )
        ]
      ~on_lines:(Rpc_server.serve_lines h.server)
  in
  let f =
    { Fixture.sandbox
    ; sw
    ; socket = Filename.concat sandbox.dir "host.sock"
    ; port
    ; terminals = host_terminals
    }
  in
  let call meth params =
    ignore
      (Rpc_server.handle
         h.server
         h.client
         (Jsonaf.of_string
            (sprintf {|{"id": 1, "method": "%s", "params": %s}|} meth params))
       : Jsonaf.t)
  in
  let session = Session.id (Agent.session agent) in
  let query = sprintf "token=sekrit&session=%s&cols=40&rows=8" session in
  let print_live () =
    print_endline
      (mask
         sandbox
         (Sexp.to_string
            [%sexp
              { host =
                  (Terminals.live host_terminals : (string * string * int) list)
              ; backend =
                  (Terminals.live backend_terminals
                   : (string * string * int) list)
              }]))
  in
  Exn.protect
    ~finally:(fun () ->
      Terminals.close_all host_terminals;
      Terminals.close_all backend_terminals;
      ignore (Fixture.tmux f [ "kill-server" ] : Process.Output.t))
    ~f:(fun () ->
      (* The host runs in [host_sw]; ending it drops the connection. *)
      let c =
        Eio.Switch.run (fun host_sw ->
          let host =
            Prigh_test.Test_tool_host.Host.start
              sandbox
              ~sw:host_sw
              ~terminals:(lazy host_terminals)
              ~port
              ~token:(Some "sekrit")
              ~cwd:host_dir
          in
          Prigh_test.Test_tool_host.Host.wait_logs sandbox host 1;
          call "hello" {|{"token": "sekrit"}|};
          call "set_active_host" {|{"host": "client-2"}|};
          let a = Peer.connect f query in
          ignore (Peer.read_until f a (Peer.saw "$") : bool);
          Peer.type_ a "pwd\r";
          print_s [%sexp (Peer.read_until f a (Peer.saw host_dir) : bool)];
          print_live ();
          [%expect
            {|
            connected to 127.0.0.1:PORT as client-2
            true
            ((host((<id> t1-<id> 1)))(backend()))
            |}];
          Websocket.send_text a.ws {|{"type":"resize","cols":50,"rows":12}|};
          Websocket.send_text a.ws {|{"type":"ping"}|};
          print_s
            [%sexp
              (Peer.read_until f a (fun p ->
                 List.mem p.texts {|{"type":"pong"}|} ~equal:String.equal)
               : bool)];
          let name = sprintf "t1-%s" session in
          print_s
            [%sexp
              (Fixture.eventually f (fun () ->
                 String.equal
                   (String.strip
                      (Fixture.tmux
                         f
                         [ "display-message"
                         ; "-p"
                         ; "-t"
                         ; name
                         ; "#{window_width}x#{window_height}"
                         ])
                        .stdout)
                   "50x12")
               : bool)];
          [%expect
            {|
            true
            true
            |}];
          (* Closing the browser detaches it on the host. *)
          Peer.close a;
          print_s
            [%sexp
              (Fixture.eventually f (fun () ->
                 List.equal
                   [%equal: string * string * int]
                   (Terminals.live host_terminals)
                   [ session, name, 0 ])
               : bool)];
          (* The shell exiting on the host closes the browser's socket. *)
          let b = Peer.connect f query in
          ignore (Peer.read_until f b (Peer.saw "$") : bool);
          Peer.type_ b "exit\r";
          print_s [%sexp (Peer.read_until f b (fun p -> p.closed) : bool)];
          print_s [%sexp (b.texts : string list)];
          [%expect
            {|
            true
            true
            ("{\"type\":\"exit\"}")
            |}];
          let c = Peer.connect f query in
          ignore (Peer.read_until f c (Peer.saw "$") : bool);
          print_live ();
          [%expect {| ((host((<id> t2-<id> 1)))(backend())) |}];
          c)
      in
      (* The host is gone: its browser sockets close with an error, and new
         ones are refused. *)
      print_s [%sexp (Peer.read_until f c (fun p -> p.closed) : bool)];
      print_s [%sexp (c.texts : string list)];
      let d = Peer.connect f query in
      ignore (Peer.read_until f d (fun p -> p.closed) : bool);
      print_s [%sexp (d.texts : string list)];
      [%expect
        {|
        true
        ("{\"type\":\"error\",\"message\":\"the tool host disconnected\"}")
        ("{\"type\":\"error\",\"message\":\"no terminal: the tool host \\\"client-2\\\" is not connected\"}")
        |}])
;;
