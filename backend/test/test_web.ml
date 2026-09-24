open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let%expect_test "websocket: accept key (RFC 6455 §1.3)" =
  print_endline (Websocket.accept_key "dGhlIHNhbXBsZSBub25jZQ==");
  [%expect {| s3pPLMBiTxaQ9kYGzzhZRbK+xOo= |}]
;;

let reader_of_string s =
  Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:(128 * 1024 * 1024)
;;

let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c))

let%expect_test "websocket: frames round-trip at every length encoding" =
  let check ?mask (frame : Websocket.Frame.t) =
    let bytes = Websocket.encode ?mask frame in
    let decoded = Websocket.read_frame (reader_of_string bytes) in
    printf
      "%s len=%d header=%s roundtrip=%b\n"
      (Sexp.to_string [%sexp (frame.opcode : Websocket.Opcode.t)])
      (String.length frame.payload)
      (hex
         (String.prefix
            bytes
            (String.length bytes - String.length frame.payload)))
      (Websocket.Frame.equal frame decoded)
  in
  check { fin = true; opcode = Text; payload = "Hello" };
  check
    ~mask:"\x37\xfa\x21\x3d"
    { fin = true; opcode = Text; payload = "Hello" };
  check { fin = false; opcode = Binary; payload = String.make 126 'x' };
  check { fin = true; opcode = Continuation; payload = String.make 65535 'y' };
  check
    ~mask:"abcd"
    { fin = true; opcode = Text; payload = String.make 70000 'z' };
  check { fin = true; opcode = Ping; payload = "" };
  check { fin = true; opcode = Close; payload = "\x03\xe8" };
  [%expect
    {|
    Text len=5 header=8105 roundtrip=true
    Text len=5 header=818537fa213d roundtrip=true
    Binary len=126 header=027e007e roundtrip=true
    Continuation len=65535 header=807effff roundtrip=true
    Text len=70000 header=81ff000000000001117061626364 roundtrip=true
    Ping len=0 header=8900 roundtrip=true
    Close len=2 header=8802 roundtrip=true
    |}];
  (* The RFC's masked "Hello" example decodes to the plain text. *)
  let decoded =
    Websocket.read_frame
      (reader_of_string "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58")
  in
  print_s [%sexp (decoded : Websocket.Frame.t)];
  [%expect {| ((fin true) (opcode Text) (payload Hello)) |}]
;;

let%expect_test
    "websocket: fragmented messages, interleaved control frames, errors"
  =
  let show s =
    let reader = Websocket.Message_reader.create (reader_of_string s) in
    let rec go () =
      match Websocket.Message_reader.next reader with
      | exception End_of_file -> print_endline "eof"
      | exception Websocket.Protocol_error e -> printf "protocol error: %s\n" e
      | message ->
        print_s [%sexp (message : Websocket.Message.t)];
        go ()
    in
    go ()
  in
  let frame ?(fin = true) opcode payload =
    Websocket.encode { fin; opcode; payload }
  in
  show
    (frame ~fin:false Text "Hel"
     ^ frame Ping "keepalive"
     ^ frame ~fin:false Continuation "lo "
     ^ frame Continuation "world"
     ^ frame Pong ""
     ^ frame Close "\x03\xe8bye"
     ^ frame Close "");
  [%expect
    {|
    (Ping keepalive)
    (Text "Hello world")
    (Pong "")
    (Close (1000))
    (Close ())
    eof
    |}];
  show (frame Continuation "orphan");
  [%expect {| protocol error: continuation frame without a start |}];
  show (frame ~fin:false Text "a" ^ frame Text "b");
  [%expect
    {| protocol error: data frame while a fragmented message is pending |}];
  show "\x83\x00";
  [%expect {| protocol error: unknown opcode 3 |}];
  show "\x81\x7f\xff\xff\xff\xff\xff\xff\xff\xff";
  [%expect {| protocol error: frame too large |}];
  show "\x81\x05Hel";
  [%expect {| eof |}]
;;

let%expect_test "web server: path safety and content types" =
  List.iter
    [ "/"
    ; "/index.html"
    ; "/js/main.bc.js"
    ; "/../etc/passwd"
    ; "/.git/config"
    ; "/a//b/"
    ]
    ~f:(fun path ->
      printf
        "%S -> %s\n"
        path
        (Sexp.to_string
           [%sexp (Web_server.For_testing.safe_relative path : string option)]));
  List.iter [ "index.html"; "main.bc.js"; "style.css"; "x.wasm" ] ~f:(fun f ->
    printf "%s: %s\n" f (Web_server.For_testing.content_type f));
  [%expect
    {|
    "/" -> ("")
    "/index.html" -> (index.html)
    "/js/main.bc.js" -> (js/main.bc.js)
    "/../etc/passwd" -> ()
    "/.git/config" -> ()
    "/a//b/" -> (a/b)
    index.html: text/html; charset=utf-8
    main.bc.js: text/javascript; charset=utf-8
    style.css: text/css; charset=utf-8
    x.wasm: application/octet-stream
    |}]
;;

(* A client on a real loopback socket: raw HTTP for the static files, then a
   masked WebSocket conversation with the RPC server. *)
module Loopback = struct
  let connect ~env ~sw ~port =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  ;;

  let http ~env ~sw ~port request =
    let flow = connect ~env ~sw ~port in
    Eio.Flow.copy_string request flow;
    Eio.Flow.shutdown flow `Send;
    let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    let status = Eio.Buf_read.line reader in
    let rec headers acc =
      match Eio.Buf_read.line reader with
      | "" -> List.rev acc
      | line -> headers (line :: acc)
    in
    let headers = headers [] in
    let body = Eio.Buf_read.take_all reader in
    printf "%s\n" status;
    List.iter (List.sort headers ~compare:String.compare) ~f:(fun h ->
      if not (String.is_prefix h ~prefix:"Sec-WebSocket-Accept")
      then printf "  %s\n" h);
    if not (String.is_empty body) then printf "  body: %S\n" body
  ;;
end

let%expect_test "web server: static files, 404s and the RPC over a WebSocket" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let root = Filename.concat t.dir "site" in
  write t "site/index.html" "<html>hi</html>";
  write t "site/main.bc.js" "console.log(1)";
  write t "site/.secret" "no";
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~token:"sekrit"
      ~login
      ~sessions_dir
      ~new_agent
      ~default_agent:(new_agent ~cwd:t.dir ())
      ()
  in
  let port =
    Web_server.listen
      ~env:t.env
      ~sw
      ~addr:Eio.Net.Ipaddr.V4.loopback
      ~port:0
      ~root:(Some root)
      ~on_websocket:(Web_server.serve_rpc server)
      ~on_lines:(Rpc_server.serve_lines server)
  in
  let http = Loopback.http ~env:t.env ~sw ~port in
  http "GET / HTTP/1.1\r\nHost: x\r\n\r\n";
  http "HEAD /main.bc.js HTTP/1.1\r\nHost: x\r\n\r\n";
  http "GET /main.bc.js?v=1 HTTP/1.1\r\nHost: x\r\n\r\n";
  http "GET /.secret HTTP/1.1\r\n\r\n";
  http "GET /../auth.json HTTP/1.1\r\n\r\n";
  http "GET /missing HTTP/1.1\r\n\r\n";
  http "POST / HTTP/1.1\r\n\r\n";
  http "GET /ws HTTP/1.1\r\nUpgrade: websocket\r\n\r\n";
  [%expect
    {|
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 15
      Content-Type: text/html; charset=utf-8
      body: "<html>hi</html>"
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 0
      Content-Type: text/javascript; charset=utf-8
    HTTP/1.1 200 OK
      Cache-Control: no-cache
      Connection: close
      Content-Length: 14
      Content-Type: text/javascript; charset=utf-8
      body: "console.log(1)"
    HTTP/1.1 404 Not Found
      Cache-Control: no-cache
      Connection: close
      Content-Length: 10
      body: "not found\n"
    HTTP/1.1 404 Not Found
      Cache-Control: no-cache
      Connection: close
      Content-Length: 10
      body: "not found\n"
    HTTP/1.1 404 Not Found
      Cache-Control: no-cache
      Connection: close
      Content-Length: 10
      body: "not found\n"
    HTTP/1.1 405 Method Not Allowed
      Cache-Control: no-cache
      Connection: close
      Content-Length: 19
      body: "method not allowed\n"
    HTTP/1.1 400 Bad Request
      Cache-Control: no-cache
      Connection: close
      Content-Length: 26
      body: "missing Sec-WebSocket-Key\n"
    |}];
  (* The upgrade, then JSON lines as masked text frames. *)
  let flow = Loopback.connect ~env:t.env ~sw ~port in
  Eio.Flow.copy_string
    "GET /ws HTTP/1.1\r\n\
     Host: x\r\n\
     Connection: Upgrade\r\n\
     Upgrade: websocket\r\n\
     Sec-WebSocket-Version: 13\r\n\
     Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
     \r\n"
    flow;
  let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line -> headers (line :: acc)
  in
  List.iter (headers []) ~f:print_endline;
  [%expect
    {|
    HTTP/1.1 101 Switching Protocols
    Upgrade: websocket
    Connection: Upgrade
    Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
    |}];
  let ws = Websocket.create ~role:`Client ~reader ~flow () in
  let call request =
    Websocket.send_text ws request;
    match Websocket.read_text ws with
    | Some reply -> print_endline (mask t reply)
    | None -> print_endline "(closed)"
  in
  call {|{"id": 1, "method": "ping", "params": {}}|};
  call
    {|{"id": 2, "method": "hello", "params": {"name": "browser", "token": "wrong"}}|};
  call
    {|{"id": 3, "method": "hello", "params": {"name": "browser", "token": "sekrit"}}|};
  call {|{"id": 4, "method": "list_paths", "params": {"prefix": "site"}}|};
  call "not json";
  [%expect
    {|
    {"type":"response","id":1,"ok":false,"error":"unauthorised: send hello with the token first"}
    {"type":"response","id":2,"ok":false,"error":"unauthorised: bad or missing token"}
    {"type":"response","id":3,"ok":true,"result":{"client_id":"client-1","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}}
    {"type":"response","id":4,"ok":true,"result":["site/","site/.secret","site/index.html","site/main.bc.js"]}
    {"type":"response","id":null,"ok":false,"error":"invalid JSON: json: unexpected string: 'not'"}
    |}];
  (* A ping is answered without disturbing the conversation; close is echoed. *)
  Eio.Flow.copy_string
    (Websocket.encode
       ~mask:"mask"
       { fin = true; opcode = Ping; payload = "hb" })
    flow;
  let messages = Websocket.Message_reader.create reader in
  print_s [%sexp (Websocket.Message_reader.next messages : Websocket.Message.t)];
  Websocket.close ws;
  print_s [%sexp (Websocket.Message_reader.next messages : Websocket.Message.t)];
  [%expect
    {|
    (Pong hb)
    (Close (1000))
    |}]
;;

(* One port, two kinds of client: a terminal frontend (plain JSON lines, the
   TUI's -connect) and a browser (WebSocket). They attach to the same session,
   a prompt from one streams to both, and the terminal, which can run tools,
   shows up in [hosts] for either of them to pick with /host. *)
let%expect_test "web port: a JSON-lines terminal and a WebSocket browser share \
                 a session"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [ Faux_provider.Reply.text "hi both" ])
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~sessions_dir
      ~new_agent
      ~default_agent:(new_agent ~cwd:t.dir ())
      ()
  in
  let port =
    Web_server.listen
      ~env:t.env
      ~sw
      ~addr:Eio.Net.Ipaddr.V4.loopback
      ~port:0
      ~root:None
      ~on_websocket:(Web_server.serve_rpc server)
      ~on_lines:(Rpc_server.serve_lines server)
  in
  let terminal =
    let flow = Loopback.connect ~env:t.env ~sw ~port in
    let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    ( (fun line -> Eio.Flow.copy_string (line ^ "\n") flow)
    , fun () ->
        match Eio.Buf_read.line reader with
        | exception (End_of_file | Eio.Io _) -> None
        | line -> Some line )
  in
  let browser =
    let flow = Loopback.connect ~env:t.env ~sw ~port in
    Eio.Flow.copy_string
      "GET /ws HTTP/1.1\r\n\
       Host: x\r\n\
       Connection: Upgrade\r\n\
       Upgrade: websocket\r\n\
       Sec-WebSocket-Version: 13\r\n\
       Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
       \r\n"
      flow;
    let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
    let rec skip_headers () =
      match Eio.Buf_read.line reader with
      | "" -> ()
      | _ -> skip_headers ()
    in
    skip_headers ();
    let ws = Websocket.create ~role:`Client ~reader ~flow () in
    Websocket.send_text ws, fun () -> Websocket.read_text ws
  in
  let summarise line =
    match Json.of_string line with
    | `Object fields ->
      let field name =
        match List.Assoc.find fields ~equal:String.equal name with
        | Some (`String s) -> s
        | Some json -> Json.to_string json
        | None -> "-"
      in
      let hosts json =
        match Jsonaf.member "hosts" json with
        | Some (`Array hosts) ->
          List.map hosts ~f:(fun h ->
            match Jsonaf.member "name" h with
            | Some (`String n) -> n
            | _ -> "?")
          |> String.concat ~sep:","
        | _ -> "-"
      in
      (match field "type" with
       | "response" ->
         let result =
           Option.value (List.Assoc.find fields ~equal:String.equal "result") ~default:`Null
         in
         let client_id =
           match Jsonaf.member "client_id" result with
           | Some (`String id) -> " client_id=" ^ id
           | _ -> ""
         in
         let state =
           match Jsonaf.member "state" result with
           | Some state ->
             sprintf
               " active_host=%s hosts=%s"
               (match Jsonaf.member "active_host" state with
                | Some (`String h) -> h
                | _ -> "?")
               (hosts state)
           | None -> ""
         in
         sprintf "response %s ok=%s%s%s" (field "id") (field "ok") client_id state
       | "event" ->
         (match field "event" with
          | "state" ->
            let state = Option.value (Jsonaf.member "state" (`Object fields)) ~default:`Null in
            sprintf
              "event state active_host=%s hosts=%s"
              (match Jsonaf.member "active_host" state with
               | Some (`String h) -> h
               | _ -> "?")
              (hosts state)
          | ev -> "event " ^ ev)
       | other -> other)
    | _ -> line
  in
  let send (write, _) line = write line in
  let last_line = ref "" in
  let rec drain_until name ((_, read) as c) ~substring =
    match read () with
    | None -> printf "%s: (closed)\n" name
    | Some line ->
      last_line := line;
      printf "%s: %s\n" name (summarise line);
      if not (String.is_substring line ~substring) then drain_until name c ~substring
  in
  let response id = sprintf "\"id\":%S" id in
  let client_id () =
    match Json.of_string !last_line |> Jsonaf.member "result" with
    | Some result ->
      (match Jsonaf.member "client_id" result with
       | Some (`String id) -> id
       | _ -> "?")
    | None -> "?"
  in
  let event name = sprintf "\"event\":%S" name in
  (* The terminal can run tools, so it takes over as the session's host as
     it attaches (the state event precedes the hello response). *)
  send
    terminal
    {|{"id": "t1", "method": "hello", "params": {"name": "laptop", "cwd": "/tmp", "tools": true}}|};
  drain_until "terminal" terminal ~substring:(response "t1");
  let terminal_id = client_id () in
  send browser {|{"id": "b1", "method": "hello", "params": {"name": "browser"}}|};
  drain_until "browser" browser ~substring:(response "b1");
  [%expect {| |}];
  send browser {|{"id": "b2", "method": "prompt", "params": {"text": "hello"}}|};
  (* The run's first host call is the $instructions lookup on the terminal,
     which answers as the real tool host would. *)
  drain_until "terminal" terminal ~substring:(event "tool_exec");
  (match !last_line |> Json.of_string |> Jsonaf.member "exec_id" with
   | Some (`String exec_id) ->
     send
       terminal
       (sprintf
          {|{"id": "t-exec", "method": "tool_exec_result", "params": {"exec_id": "%s", "text": "[]"}}|}
          exec_id)
   | _ -> print_endline "no exec_id");
  drain_until "browser" browser ~substring:(event "agent_end");
  drain_until "terminal" terminal ~substring:(event "agent_end");
  [%expect {| |}];
  (* /host from the browser moves tools back to the backend and then to the
     terminal again; both clients see each switch. *)
  send
    browser
    {|{"id": "b3", "method": "set_active_host", "params": {"host": "backend"}}|};
  drain_until "browser" browser ~substring:(response "b3");
  drain_until "terminal" terminal ~substring:(event "state");
  send
    terminal
    (sprintf
       {|{"id": "t2", "method": "set_active_host", "params": {"host": "%s"}}|}
       terminal_id);
  drain_until "terminal" terminal ~substring:(response "t2");
  drain_until "browser" browser ~substring:(event "state");
  [%expect {| |}]
;;
