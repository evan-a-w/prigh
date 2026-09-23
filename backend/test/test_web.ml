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
