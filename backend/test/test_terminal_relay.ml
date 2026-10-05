open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The server side of a terminal relayed to a tool-host client, with the
   host played by hand and the browser by a [Terminal_channel.Fed]. *)

let settle () =
  for _ = 1 to 200 do
    Eio.Fiber.yield ()
  done
;;

let print_target t server ~session =
  let target =
    match Rpc_server.terminal_target server ~session with
    | `Backend cwd -> sprintf "Backend %s" cwd
    | `Host (host, cwd) -> sprintf "Host %s %s" host cwd
    | `Unavailable reason -> sprintf "Unavailable %S" reason
  in
  print_endline (mask t target)
;;

let request (h : Test_rpc.H.t) t client meth params =
  print_endline
    (mask
       t
       (Json.to_string
          (Rpc_server.handle
             h.server
             client
             (`Object
                 [ "id", `String meth
                 ; "method", `String meth
                 ; "params", params
                 ]))))
;;

let print_terminal_events t sent =
  Queue.iter sent ~f:(fun json ->
    match Json.member "event" json with
    | Some (`String name) when String.is_prefix name ~prefix:"terminal_" ->
      print_endline (mask t (Json.to_string json))
    | _ -> ());
  Queue.clear sent
;;

module Browser = struct
  type t =
    { fed : Terminal_channel.Fed.t
    ; received : Terminal_channel.Frame.t Queue.t
    ; mutable finished : bool
    }

  (* Opens a relayed terminal in its own fiber. *)
  let open_ ~sw (h : Test_rpc.H.t) ~host ~key ~cwd =
    let received = Queue.create () in
    let fed = Terminal_channel.Fed.create ~send:(Queue.enqueue received) in
    let t = { fed; received; finished = false } in
    Eio.Fiber.fork ~sw (fun () ->
      Rpc_server.relay_terminal
        h.server
        ~host
        ~key
        ~cwd
        ~cols:80
        ~rows:24
        (Terminal_channel.Fed.channel fed);
      t.finished <- true);
    settle ();
    t
  ;;

  let print t =
    Queue.iter t.received ~f:(fun frame ->
      print_s [%sexp (frame : Terminal_channel.Frame.t)]);
    Queue.clear t.received;
    printf "relay finished: %b\n" t.finished
  ;;
end

let%expect_test "terminal relay: target, frames both ways, closes, host loss" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent, h =
    Test_rpc.make_server t ~sw ~provider:(Faux_provider.create [])
  in
  let session = Session.id (Agent.session agent) in
  print_target t h.server ~session:None;
  print_target t h.server ~session:(Some "unknown");
  print_target t h.server ~session:(Some session);
  [%expect
    {|
    Backend $DIR
    Backend $DIR
    Backend $DIR
    |}];
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  ignore
    (Rpc_server.handle
       h.server
       laptop
       (Json.of_string
          (sprintf
             {|{"id": 1, "method": "hello", "params": {"name": "laptop", "tools": true, "cwd": "/home/me/proj", "session": "%s"}}|}
             session))
     : Json.t);
  print_target t h.server ~session:(Some session);
  [%expect {| Host client-2 /home/me/proj |}];
  Queue.clear sent;
  let browser =
    Browser.open_ ~sw h ~host:"client-2" ~key:session ~cwd:"/home/me/proj"
  in
  Terminal_channel.Fed.push browser.fed (`Binary "echo hi\r");
  Terminal_channel.Fed.push browser.fed (`Text {|{"type":"ping"}|});
  settle ();
  print_terminal_events t sent;
  [%expect
    {|
    {"type":"event","event":"terminal_open","host":"client-2","term_id":"term-1","key":"<id>","cwd":"/home/me/proj","cols":80,"rows":24}
    {"type":"event","event":"terminal_frame","host":"client-2","term_id":"term-1","kind":"binary","data":"ZWNobyBoaQ0="}
    {"type":"event","event":"terminal_frame","host":"client-2","term_id":"term-1","kind":"text","data":"{\"type\":\"ping\"}"}
    |}];
  (* The host answers; only it may write to its terminals. *)
  let frame client fields =
    request h t client "terminal_frame" (`Object fields)
  in
  frame
    laptop
    [ "term_id", `String "term-1"
    ; "kind", `String "binary"
    ; "data", `String (Base64.encode_string "hi\r\n")
    ];
  frame
    laptop
    [ "term_id", `String "term-1"
    ; "kind", `String "text"
    ; "data", `String {|{"type":"pong"}|}
    ];
  frame
    h.client
    [ "term_id", `String "term-1"; "kind", `String "text"; "data", `String "x" ];
  frame
    laptop
    [ "term_id", `String "term-9"; "kind", `String "text"; "data", `String "x" ];
  frame
    laptop
    [ "term_id", `String "term-1"
    ; "kind", `String "binary"
    ; "data", `String "!!"
    ];
  frame laptop [ "term_id", `String "term-1"; "kind", `String "other" ];
  request
    h
    t
    h.client
    "terminal_closed"
    (`Object [ "term_id", `String "term-1" ]);
  settle ();
  Browser.print browser;
  [%expect
    {|
    {"type":"response","id":"terminal_frame","ok":true,"result":{}}
    {"type":"response","id":"terminal_frame","ok":true,"result":{}}
    {"type":"response","id":"terminal_frame","ok":false,"error":"no terminal \"term-1\""}
    {"type":"response","id":"terminal_frame","ok":false,"error":"no terminal \"term-9\""}
    {"type":"response","id":"terminal_frame","ok":false,"error":"bad base64 in \"data\": Wrong padding"}
    {"type":"response","id":"terminal_frame","ok":false,"error":"a frame needs \"kind\" (\"binary\" or \"text\") and a string \"data\""}
    {"type":"response","id":"terminal_closed","ok":false,"error":"no terminal \"term-1\""}
    (Binary "hi\r\n")
    (Text "{\"type\":\"pong\"}")
    relay finished: false
    |}];
  (* The host finishing with the viewer ends the relay (the web server then
     closes the socket); the host hears nothing more. *)
  request h t laptop "terminal_closed" (`Object [ "term_id", `String "term-1" ]);
  settle ();
  Browser.print browser;
  print_terminal_events t sent;
  request h t laptop "terminal_closed" (`Object [ "term_id", `String "term-1" ]);
  [%expect
    {|
    {"type":"response","id":"terminal_closed","ok":true,"result":{}}
    relay finished: true
    {"type":"response","id":"terminal_closed","ok":false,"error":"no terminal \"term-1\""}
    |}];
  (* The browser going away tells the host. *)
  let browser = Browser.open_ ~sw h ~host:"client-2" ~key:session ~cwd:"/x" in
  Terminal_channel.Fed.close browser.fed;
  settle ();
  Browser.print browser;
  print_terminal_events t sent;
  [%expect
    {|
    relay finished: true
    {"type":"event","event":"terminal_open","host":"client-2","term_id":"term-2","key":"<id>","cwd":"/x","cols":80,"rows":24}
    {"type":"event","event":"terminal_close","host":"client-2","term_id":"term-2"}
    |}];
  (* The host going away closes the browser side with an error; the session
     keeps it as its active host, so new terminals are refused. *)
  let browser = Browser.open_ ~sw h ~host:"client-2" ~key:session ~cwd:"/x" in
  Rpc_server.disconnect h.server laptop;
  settle ();
  Browser.print browser;
  print_target t h.server ~session:(Some session);
  let late = Browser.open_ ~sw h ~host:"client-2" ~key:session ~cwd:"/x" in
  Browser.print late;
  [%expect
    {|
    (Text "{\"type\":\"error\",\"message\":\"the tool host disconnected\"}")
    relay finished: true
    Unavailable "the tool host \"client-2\" is not connected"
    (Text "{\"type\":\"error\",\"message\":\"the tool host disconnected\"}")
    relay finished: true
    |}]
;;
