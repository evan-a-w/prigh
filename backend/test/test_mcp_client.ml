open! Core
open! Prigh
open Mcp_test_helpers
module Server = Fake_http_server
module Json = Jsonaf

let connect ~env ~sw ?timeout server =
  match Mcp_client.connect ~env ~sw ?timeout server with
  | Ok client -> client
  | Error e -> raise_s [%message "connect failed" (e : Error.t)]
;;

let print_tools ~dir client =
  match Mcp_client.tools client with
  | Error e -> print_s_masked ~dir [%sexp (e : Error.t)]
  | Ok tools ->
    List.iter tools ~f:(fun (tool : Mcp_client.Tool.t) ->
      print_s
        [%sexp (tool.name : string), { read_only : bool = tool.read_only }])
;;

let call ?(cancel = Cancellation.never) ~dir client tool arguments =
  let result =
    Mcp_client.call
      client
      ~cancel
      ~tool
      ~arguments:(ok_exn (Json.parse arguments))
  in
  print_result ~dir result
;;

let%expect_test "stdio: handshake, paginated tools (cached), calls, close" =
  run
  @@ fun ~env ~sw ~dir ->
  let client = connect ~env ~sw (stdio_server ~dir ()) in
  print_tools ~dir client;
  [%expect
    {|
    (echo ((read_only true)))
    (image ((read_only false)))
    (error ((read_only false)))
    (slow ((read_only false)))
    (structured ((read_only false)))
    (ping_client ((read_only false)))
    (change_tools ((read_only false)))
    |}];
  (match Mcp_client.tools client with
   | Ok (echo :: _) -> print_s [%sexp (echo : Mcp_client.Tool.t)]
   | _ -> print_endline "no tools");
  [%expect
    {|
    ((name echo) (description "Echoes its text")
     (input_schema
      (Object
       ((type (String object))
        (properties (Object ((text (Object ((type (String string)))))))))))
     (read_only true))
    |}];
  call ~dir client "echo" {|{"text":"hello"}|};
  [%expect
    {|
    hello
    (echoed)
    |}];
  call ~dir client "image" "{}";
  [%expect
    {|
    here is a picture
    {"type":"resource","resource":{"uri":"file:///notes.txt","text":"notes"}}
    (image ((mime_type image/png) (data iVBORw0KGgo=)))
    |}];
  call ~dir client "error" "{}";
  [%expect {| ERROR: something broke |}];
  call ~dir client "structured" "{}";
  [%expect {| {"answer":42} |}];
  call ~dir client "nope" "{}";
  [%expect {| ERROR: Unknown tool: nope (MCP error -32602) |}];
  print_s [%sexp (Mcp_client.failure client : string option)];
  [%expect {| () |}];
  Mcp_client.close client;
  print_s [%sexp (Mcp_client.failure client : string option)];
  call ~dir client "echo" {|{"text":"after"}|};
  [%expect
    {|
    ("the connection was closed")
    ERROR: the connection was closed
    |}];
  (* The tools were listed once (two pages) and then cached. *)
  print_log ~dir;
  [%expect
    {|
    start
    initialize {"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"prigh","version":"$VERSION"}}
    notifications/initialized
    tools/list cursor=none
    tools/list cursor=page2
    tools/call echo
    tools/call image
    tools/call error
    tools/call structured
    tools/call nope
    stdin closed
    |}]
;;

let%expect_test "stdio: the server's requests are answered" =
  run
  @@ fun ~env ~sw ~dir ->
  let client = connect ~env ~sw (stdio_server ~dir ()) in
  call ~dir client "ping_client" "{}";
  [%expect
    {|
    {"jsonrpc":"2.0","id":"s1","result":{}}
    {"jsonrpc":"2.0","id":"s2","error":{"code":-32601,"message":"method not found: sampling/createMessage"}}
    |}];
  Mcp_client.close client
;;

let%expect_test "stdio: list_changed invalidates the cached tools" =
  run
  @@ fun ~env ~sw ~dir ->
  let client = connect ~env ~sw (stdio_server ~dir ()) in
  let names () =
    List.map (ok_exn (Mcp_client.tools client)) ~f:(fun tool -> tool.name)
    |> String.concat ~sep:" "
    |> print_endline
  in
  names ();
  names ();
  call ~dir client "change_tools" "{}";
  names ();
  names ();
  [%expect
    {|
    echo image error slow structured ping_client change_tools
    echo image error slow structured ping_client change_tools
    changed
    echo image error slow structured ping_client change_tools added
    echo image error slow structured ping_client change_tools added
    |}];
  List.filter (log ~dir) ~f:(String.is_prefix ~prefix:"tools/")
  |> List.iter ~f:print_endline;
  [%expect
    {|
    tools/list cursor=none
    tools/list cursor=page2
    tools/call change_tools
    tools/list cursor=none
    tools/list cursor=page2
    |}];
  Mcp_client.close client
;;

let%expect_test "stdio: cancelling a call tells the server" =
  run
  @@ fun ~env ~sw ~dir ->
  let client = connect ~env ~sw (stdio_server ~dir ()) in
  let cancel = Cancellation.create () in
  Eio.Fiber.both
    (fun () -> call ~cancel ~dir client "slow" "{}")
    (fun () ->
       wait_until ~env (fun () ->
         List.mem (log ~dir) "tools/call slow" ~equal:String.equal);
       Cancellation.cancel cancel);
  [%expect {| ERROR: [cancelled] |}];
  (* The server handles messages in order, so it has seen the cancellation
     by the time it answers the next call. *)
  call ~dir client "echo" {|{"text":"still alive"}|};
  [%expect
    {|
    still alive
    (echoed)
    |}];
  List.filter (log ~dir) ~f:(String.is_prefix ~prefix:"cancelled")
  |> List.iter ~f:print_endline;
  [%expect {| cancelled {"requestId":2,"reason":"cancelled by the user"} |}];
  Mcp_client.close client
;;

let%expect_test "stdio: a server that dies fails its calls and the client" =
  run
  @@ fun ~env ~sw ~dir ->
  let client = connect ~env ~sw (stdio_server ~dir ()) in
  call ~dir client "crash" "{}";
  [%expect {| ERROR: the server exited (code 3): fake: crashing on purpose |}];
  print_s [%sexp (Mcp_client.failure client : string option)];
  [%expect {| ("the server exited (code 3): fake: crashing on purpose") |}];
  print_tools ~dir client;
  [%expect {| "the server exited (code 3): fake: crashing on purpose" |}];
  Mcp_client.close client
;;

let print_connect_error ~env ~sw ~dir ?timeout server =
  match Mcp_client.connect ~env ~sw ?timeout server with
  | Ok client ->
    Mcp_client.close client;
    print_endline "connected"
  | Error e -> print_endline (mask ~dir (Error.to_string_hum e))
;;

let%expect_test "stdio: startup failures" =
  run
  @@ fun ~env ~sw ~dir ->
  print_connect_error
    ~env
    ~sw
    ~dir
    (stdio_server ~dir ~args:[ "--exit-with-stderr" ] ());
  [%expect
    {|
    the server exited (code 1): fake: could not read the config
    fake: FAKE_TOKEN is not set
    |}];
  let with_command command =
    { (stdio_server ~dir ()) with
      transport = Stdio { command; args = []; env = [] }
    }
  in
  print_connect_error ~env ~sw ~dir (with_command "no-such-mcp-server");
  [%expect
    {| the command "no-such-mcp-server" was not found on PATH; install it, or fix "command" in $DIR/.mcp.json |}];
  print_connect_error ~env ~sw ~dir (with_command "./bin/server");
  [%expect
    {| $DIR/bin/server does not exist or is not executable; fix "command" in $DIR/.mcp.json |}];
  (* Relative commands run from the server's directory, PATH comes from its
     env. *)
  Core_unix.mkdir_p (Filename.concat dir "bin");
  Core_unix.symlink
    ~target:fake_server
    ~link_name:(Filename.concat dir "bin/server");
  print_connect_error ~env ~sw ~dir (with_command "./bin/server");
  print_connect_error
    ~env
    ~sw
    ~dir
    { (stdio_server ~dir ()) with
      transport =
        Stdio
          { command = "server"
          ; args = []
          ; env = [ "PATH", Filename.concat dir "bin" ]
          }
    };
  [%expect
    {|
    connected
    connected
    |}];
  print_connect_error
    ~env
    ~sw
    ~dir
    ~timeout:(Time_ns.Span.of_int_ms 100)
    (stdio_server ~dir ~args:[ "--hang" ] ());
  [%expect {| the server did not answer initialize within 100ms |}]
;;

let mcp_handler ?(status = 200) (request : Server.Request.t) : Server.Reply.t =
  let json = ok_exn (Json.parse request.body) in
  let id = Option.value (Json.member "id" json) ~default:`Null in
  let result result =
    Json.to_string
      (`Object [ "jsonrpc", `String "2.0"; "id", id; "result", result ])
  in
  let sse_event json = sprintf "event: message\ndata: %s\n\n" json in
  match Option.bind (Json.member "method" json) ~f:Json.string with
  | _ when status <> 200 -> Server.Reply.simple status "missing bearer token"
  | Some "initialize" ->
    Server.Reply.simple
      ~headers:
        [ "Content-Type", "application/json"; "Mcp-Session-Id", "session-1" ]
      200
      (result (`Object [ "protocolVersion", `String "2025-06-18" ]))
  | Some "tools/list" ->
    { status = 200
    ; headers = [ "Content-Type", "text/event-stream" ]
    ; chunks =
        [ ( 0.
          , sse_event {|{"jsonrpc":"2.0","id":"p1","method":"ping"}|}
            ^ sse_event
                {|{"jsonrpc":"2.0","method":"notifications/message","params":{}}|}
          )
        ; ( 0.1
          , sse_event
              (result
                 (`Object
                     [ ( "tools"
                       , `Array
                           [ `Object
                               [ "name", `String "search"
                               ; ( "annotations"
                                 , `Object [ "readOnlyHint", `True ] )
                               ]
                           ] )
                     ])) )
        ]
    }
  | Some "tools/call" ->
    { status = 200
    ; headers = [ "Content-Type", "text/event-stream; charset=utf-8" ]
    ; chunks =
        [ ( 0.
          , sse_event
              (result
                 (`Object
                     [ ( "content"
                       , `Array
                           [ `Object
                               [ "type", `String "text"
                               ; "text", `String "found"
                               ]
                           ] )
                     ])) )
        ]
    }
  | Some _ | None -> Server.Reply.simple 202 ""
;;

let http_server ~url ~dir =
  { Mcp_config.Server.name = "remote"
  ; source = Filename.concat dir ".mcp.json"
  ; project = false
  ; dir
  ; transport = Http { url; headers = [ "Authorization", "Bearer secret" ] }
  ; approval = ""
  }
;;

let%expect_test "http: JSON and SSE replies, the session id, server requests" =
  run
  @@ fun ~env ~sw ~dir ->
  let server = Server.start ~sw ~env ~handler:mcp_handler in
  let client =
    connect ~env ~sw (http_server ~url:(Server.url server "/mcp") ~dir)
  in
  print_tools ~dir client;
  call ~dir client "search" {|{"q":"x"}|};
  [%expect
    {|
    (search ((read_only true)))
    found
    |}];
  List.iter (Server.requests server) ~f:(fun request ->
    let json = ok_exn (Json.parse request.body) in
    let header = Server.Request.header request in
    print_s
      [%sexp
        { message =
            ((match Json.member "method" json with
              | Some method_ -> Json.to_string method_
              | None -> Json.to_string json)
             : string)
        ; request_line : string = request.request_line
        ; content_type : string option = header "content-type"
        ; accept : string option = header "accept"
        ; authorization : string option = header "authorization"
        ; protocol_version : string option = header "mcp-protocol-version"
        ; session_id : string option = header "mcp-session-id"
        }]);
  [%expect
    {|
    ((message "\"initialize\"") (request_line "POST /mcp HTTP/1.1")
     (content_type (application/json))
     (accept ("application/json, text/event-stream"))
     (authorization ("Bearer secret")) (protocol_version ()) (session_id ()))
    ((message "\"notifications/initialized\"")
     (request_line "POST /mcp HTTP/1.1") (content_type (application/json))
     (accept ("application/json, text/event-stream"))
     (authorization ("Bearer secret")) (protocol_version (2025-06-18))
     (session_id (session-1)))
    ((message "\"tools/list\"") (request_line "POST /mcp HTTP/1.1")
     (content_type (application/json))
     (accept ("application/json, text/event-stream"))
     (authorization ("Bearer secret")) (protocol_version (2025-06-18))
     (session_id (session-1)))
    ((message "{\"jsonrpc\":\"2.0\",\"id\":\"p1\",\"result\":{}}")
     (request_line "POST /mcp HTTP/1.1") (content_type (application/json))
     (accept ("application/json, text/event-stream"))
     (authorization ("Bearer secret")) (protocol_version (2025-06-18))
     (session_id (session-1)))
    ((message "\"tools/call\"") (request_line "POST /mcp HTTP/1.1")
     (content_type (application/json))
     (accept ("application/json, text/event-stream"))
     (authorization ("Bearer secret")) (protocol_version (2025-06-18))
     (session_id (session-1)))
    |}];
  Mcp_client.close client
;;

let%expect_test "http: errors" =
  run
  @@ fun ~env ~sw ~dir ->
  let print_error ~url e =
    print_endline
      (mask ~dir (Error.to_string_hum e)
       |> String.substr_replace_all ~pattern:url ~with_:"$URL")
  in
  let unauthorized = Server.start ~sw ~env ~handler:(mcp_handler ~status:401) in
  let url = Server.url unauthorized "/mcp" in
  (match Mcp_client.connect ~env ~sw (http_server ~url ~dir) with
   | Ok _ -> print_endline "connected"
   | Error e -> print_error ~url e);
  [%expect
    {| HTTP 401 from $URL: missing bearer token; if it needs credentials, add an Authorization header to the server's "headers" in $DIR/.mcp.json |}];
  (* The server forgets the session: the client is dead until reconnected. *)
  let forgetful =
    Server.start ~sw ~env ~handler:(fun request ->
      if String.is_substring request.body ~substring:"tools/list"
      then Server.Reply.simple 404 "unknown session"
      else mcp_handler request)
  in
  let url = Server.url forgetful "/mcp" in
  let client = connect ~env ~sw (http_server ~url ~dir) in
  (match Mcp_client.tools client with
   | Ok _ -> print_endline "listed"
   | Error e -> print_error ~url e);
  print_s [%sexp (Mcp_client.failure client : string option)];
  [%expect
    {|
    HTTP 404 from $URL: unknown session
    ("the server ended the session (HTTP 404)")
    |}]
;;
