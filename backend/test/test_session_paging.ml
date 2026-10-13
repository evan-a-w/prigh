open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let%expect_test "file cache: read once while unchanged, least recently used out"
  =
  with_sandbox
  @@ fun t ->
  let cache = File_cache.create ~max_bytes:25 () in
  let file name = Filename.concat t.dir name in
  let write name text = Out_channel.write_all (file name) ~data:text in
  let read path =
    printf "reading %s\n" (Filename.basename path);
    Ok (In_channel.read_all path)
  in
  let get name =
    match File_cache.find_or_read cache (file name) ~read with
    | Ok text -> printf "%s: %S\n" name text
    | Error e -> print_s [%sexp (e : Error.t)]
  in
  let cached () =
    print_s
      [%sexp
        (List.map (File_cache.paths cache) ~f:Filename.basename : string list)]
  in
  write "a" "0123456789";
  write "b" "0123456789";
  get "a";
  get "a";
  get "b";
  cached ();
  [%expect
    {|
    reading a
    a: "0123456789"
    a: "0123456789"
    reading b
    b: "0123456789"
    (a b)
    |}];
  (* A change of size (or of modification time) is a change. *)
  Out_channel.with_file (file "a") ~append:true ~f:(fun oc ->
    Out_channel.output_string oc "!");
  get "a";
  [%expect
    {|
    reading a
    a: "0123456789!"
    |}];
  (* Over 25 bytes: the least recently used goes. *)
  write "c" "0123456789";
  get "c";
  cached ();
  [%expect
    {|
    reading c
    c: "0123456789"
    (a c)
    |}];
  Core_unix.unlink (file "c");
  print_s [%sexp (File_cache.find cache (file "c") : string option)];
  cached ();
  [%expect
    {|
    ()
    (a)
    |}];
  File_cache.set cache (file "b") "set by hand";
  print_s [%sexp (File_cache.find cache (file "b") : string option)];
  [%expect {| ("set by hand") |}]
;;

module Client = struct
  type t =
    { client : Rpc_server.Client.t
    ; server : Rpc_server.t
    }

  let request (c : t) meth params =
    Rpc_server.handle
      c.server
      c.client
      (`Object
          [ "id", `String "r"
          ; "method", `String meth
          ; "params", `Object params
          ])
  ;;

  let result json =
    match Json.member "ok" json, Json.member "result" json with
    | Some `True, Some result -> Ok result
    | _ ->
      (match Json.member "error" json with
       | Some (`String e) -> Error e
       | _ -> Error (Json.to_string json))
  ;;

  let call (c : t) meth params =
    match result (request c meth params) with
    | Ok json -> json
    | Error e -> raise_s [%message "call failed" meth e]
  ;;
end

let server (t : Tool_test_helpers.t) ~sw =
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
      ~home:t.dir
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let c = { Client.client; server } in
  ignore (Client.request c "hello" [] : Json.t);
  c
;;

let numbered n =
  String.concat
    (List.map (List.range 1 (n + 1)) ~f:(sprintf "test %04d ... ok\n"))
;;

let call id = Content.Tool_call { id; name = "bash"; arguments = "{}" }

let assistant ?(calls = []) text =
  Message.Assistant
    { content = Text text :: calls
    ; stop_reason = (if List.is_empty calls then End_turn else Tool_use)
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let result ?(name = "bash") id text =
  Message.Tool_result
    { tool_call_id = id; tool_name = name; text; is_error = false; images = [] }
;;

(* Ten turns, each running the tests; the last also a subagent, whose log
   is written as an earlier backend would have. *)
let saved (t : Tool_test_helpers.t) =
  let session =
    Session.create ~dir:(Filename.concat t.dir "sessions") ~cwd:t.dir ()
  in
  let append m =
    ignore (Session.append_message session ~at:1. m : Session.Entry.t)
  in
  for turn = 1 to 10 do
    append (Message.user (sprintf "turn %d" turn));
    let id = sprintf "c%d" turn in
    let calls = if turn = 10 then [ call id; call "s1" ] else [ call id ] in
    append (assistant ~calls (sprintf "running %d" turn));
    append (result id (numbered (if turn = 10 then 1000 else 3)));
    if turn = 10 then append (result ~name:"subagent" "s1" "found it");
    append (assistant (sprintf "done %d" turn))
  done;
  let dir = Session.data_dir (Session.path session) in
  Core_unix.mkdir_p dir;
  let line json = Json.to_string json in
  let message m =
    line
      (`Array
          [ `String "Message"
          ; `Object
              [ "id", `String "a1"
              ; "at", `Number "2"
              ; "turns", `Number "1"
              ; "tool_calls", `Number "1"
              ; "message", Message.jsonaf_of_t m
              ]
          ])
  in
  Out_channel.write_lines
    (Filename.concat dir "subagents.jsonl")
    [ {|["Run",{"run":1}]|}
    ; {|["Start",{"id":"a1","call_id":"s1","task":"look","model":"m","at":1,"run":1}]|}
    ; message (assistant ~calls:[ call "g1" ] "grepping")
    ; message (result ~name:"grep" "g1" (numbered 2000))
    ; message (assistant "found it")
    ; {|["End",{"id":"a1","at":3,"run":1,"turns":1,"tool_calls":1,"result":{"text":"found it","is_error":false}}]|}
    ];
  session
;;

let text json =
  match Json.member "text" json, Json.member "content" json with
  | Some (`String s), _ -> s
  | _, Some (`Array (c :: _)) ->
    (match Json.member "text" c with
     | Some (`String s) -> s
     | _ -> "?")
  | _ -> "?"
;;

let summary text =
  match String.split_lines text with
  | [] -> "(empty)"
  | [ line ] -> line
  | lines ->
    sprintf
      "%s ... %s (%d lines)"
      (List.hd_exn lines)
      (List.last_exn lines)
      (List.length lines)
;;

let show_messages messages =
  List.iter messages ~f:(fun m ->
    let elided =
      match Json.member "elided" m with
      | Some e -> " elided " ^ Json.to_string e
      | None -> ""
    in
    printf
      "  %s: %s%s\n"
      (Json.string_exn (Json.member_exn "role" m))
      (summary (text m))
      elided)
;;

let show_page json =
  printf
    "older %s, before %s\n"
    (Json.to_string (Json.member_exn "older" json))
    (match Json.member_exn "before" json with
     | `Null -> "null"
     | `String _ -> "<id>"
     | json -> Json.to_string json);
  show_messages (Json.list_exn (Json.member_exn "messages" json))
;;

let live (c : Client.t) path =
  Json.list_exn (Client.call c "list_sessions" [])
  |> List.find ~f:(fun s ->
    match Json.member "path" s with
    | Some (`String p) -> String.equal p path
    | _ -> false)
  |> Option.bind ~f:(Json.member "live")
  |> Option.value ~default:`Null
  |> Json.to_string
;;

let%expect_test
    "get_messages_page: the newest first, older on demand, outputs whole on \
     demand"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let session = saved t in
  let path = Session.path session in
  let c = server t ~sw in
  (* Another session's messages, without opening it. *)
  show_page
    (Client.call
       c
       "get_messages_page"
       [ "session", `String path; "limit", `Number "4" ]);
  printf "live: %s\n" (live c path);
  [%expect
    {|
    older 37, before <id>
      assistant: running 10
      tool_result: test 0001 ... ok ... test 1000 ... ok (40 lines) elided {"at":340,"lines":1000,"bytes":17000}
      tool_result: found it
      assistant: done 10
    live: false
    |}];
  ignore (Client.call c "switch_session" [ "path", `String path ] : Json.t);
  let page = Client.call c "get_messages_page" [ "limit", `Number "4" ] in
  let before = Json.member_exn "before" page in
  show_page
    (Client.call
       c
       "get_messages_page"
       [ "limit", `Number "4"; "before", before ]);
  [%expect
    {|
    older 33, before <id>
      assistant: running 9
      tool_result: test 0001 ... ok ... test 0003 ... ok (3 lines)
      assistant: done 9
      user: turn 10
    |}];
  (* The whole output, of the session's call or of a subagent's. *)
  let output params =
    match Client.result (Client.request c "get_tool_output" params) with
    | Ok json ->
      printf
        "%s, is_error %s\n"
        (summary (text json))
        (Json.to_string (Json.member_exn "is_error" json))
    | Error e -> print_endline e
  in
  output [ "call_id", `String "c10" ];
  output [ "call_id", `String "g1"; "agent_id", `String "a1" ];
  output [ "call_id", `String "g1" ];
  output [ "call_id", `String "g1"; "agent_id", `String "a2" ];
  [%expect
    {|
    test 0001 ... ok ... test 1000 ... ok (1000 lines), is_error false
    test 0001 ... ok ... test 2000 ... ok (2000 lines), is_error false
    no output of tool call "g1" in this session: pass the "session" (and, for a subagent's call, "agent_id") it belongs to
    unknown subagent "a2": list_subagents shows this session's
    |}];
  (* A subagent's transcript, its outputs cut too. *)
  Client.call c "get_subagent" [ "id", `String "s1"; "elide", `True ]
  |> Json.member_exn "messages"
  |> Json.list_exn
  |> show_messages;
  [%expect
    {|
      assistant: grepping
      tool_result: test 0001 ... ok ... test 2000 ... ok (40 lines) elided {"at":340,"lines":2000,"bytes":34000}
      assistant: found it
    |}];
  (match
     Client.result
       (Client.request
          c
          "get_messages_page"
          [ "session", `String "nope"; "limit", `Number "4" ])
   with
   | Ok _ -> ()
   | Error e -> print_endline e);
  [%expect {| no session "nope" |}]
;;

let%expect_test "get_tool_output: before a compaction, of another session" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let session = saved t in
  let path = Session.path session in
  let c = server t ~sw in
  let output params =
    match Client.result (Client.request c "get_tool_output" params) with
    | Ok json -> print_endline (summary (text json))
    | Error e -> print_endline e
  in
  (* Read without opening it; c's own session has no such call. *)
  output [ "call_id", `String "c2"; "session", `String path ];
  output [ "call_id", `String "c2" ];
  [%expect
    {|
    test 0001 ... ok ... test 0003 ... ok (3 lines)
    no output of tool call "c2" in this session: pass the "session" (and, for a subagent's call, "agent_id") it belongs to
    |}];
  (* Compacted down to the last turn: the earlier outputs are still there. *)
  let kept_from =
    List.find_map_exn (Session.entries session) ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message (User { text = "turn 10"; _ }) -> Some e.id
      | _ -> None)
  in
  ignore
    (Session.append_compaction session ~summary:"earlier turns" ~kept_from
     : Session.Entry.t);
  ignore (Client.call c "switch_session" [ "path", `String path ] : Json.t);
  output [ "call_id", `String "c2" ];
  output [ "call_id", `String "nope" ];
  [%expect
    {|
    test 0001 ... ok ... test 0003 ... ok (3 lines)
    no output of tool call "nope" in this session: pass the "session" (and, for a subagent's call, "agent_id") it belongs to
    |}]
;;

let%expect_test "sessions read again are cached until their file changes" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let session = saved t in
  let path = Session.path session in
  (* A time [utimes] can set exactly. *)
  let stamp = Float.round_down (Core_unix.stat path).st_mtime in
  Core_unix.utimes path ~access:stamp ~modif:stamp;
  let c = server t ~sw in
  let newest () =
    let page =
      Client.call
        c
        "get_messages_page"
        [ "session", `String path; "limit", `Number "1" ]
    in
    show_messages (Json.list_exn (Json.member_exn "messages" page))
  in
  newest ();
  [%expect {| assistant: done 10 |}];
  (* Changed behind the cache's back, keeping its size and time: still the
     cached session. *)
  let contents = In_channel.read_all path in
  Out_channel.write_all
    path
    ~data:
      (String.substr_replace_all contents ~pattern:"done 10" ~with_:"DONE 10");
  Core_unix.utimes path ~access:stamp ~modif:stamp;
  newest ();
  [%expect {| assistant: done 10 |}];
  (* Another time is another file. *)
  Core_unix.utimes path ~access:(stamp +. 1.) ~modif:(stamp +. 1.);
  newest ();
  [%expect {| assistant: DONE 10 |}];
  (* Opened, then left (and so evicted): kept for next time, until another
     process appends to it. *)
  ignore (Client.call c "switch_session" [ "path", `String path ] : Json.t);
  ignore (Client.call c "new_session" [] : Json.t);
  printf "live: %s\n" (live c path);
  newest ();
  [%expect
    {|
    live: false
      assistant: DONE 10
    |}];
  let other = Or_error.ok_exn (Session.load path) in
  ignore
    (Session.append_message other (Message.user "from elsewhere")
     : Session.Entry.t);
  newest ();
  ignore (Client.call c "switch_session" [ "path", `String path ] : Json.t);
  show_messages
    (Json.list_exn
       (Json.member_exn
          "messages"
          (Client.call c "get_messages_page" [ "limit", `Number "1" ])));
  [%expect
    {|
    user: from elsewhere
    user: from elsewhere
    |}]
;;
