open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let assistant text =
  Message.Assistant
    { content =
        [ Text text
        ; Content.Tool_call { id = "c"; name = "bash"; arguments = "{}" }
        ]
    ; stop_reason = End_turn
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let tool_result text =
  Message.Tool_result
    { tool_call_id = "c"
    ; tool_name = "bash"
    ; text
    ; is_error = false
    ; images = []
    }
;;

let sessions_dir (t : Tool_test_helpers.t) = Filename.concat t.dir "sessions"

(* A session saved with [messages], last active [at]. *)
let saved (t : Tool_test_helpers.t) ?name ~at messages =
  let session = Session.create ~dir:(sessions_dir t) ~cwd:t.dir () in
  Option.iter name ~f:(fun name ->
    ignore (Session.set_name session ~name : Session.Entry.t));
  List.iter messages ~f:(fun m ->
    ignore (Session.append_message session ~at m : Session.Entry.t));
  session
;;

let render (snippet : Session_search.Snippet.t) =
  let buf = Buffer.create 100 in
  let pos =
    List.fold snippet.ranges ~init:0 ~f:(fun pos (start, len) ->
      Buffer.add_string buf (String.sub snippet.text ~pos ~len:(start - pos));
      Buffer.add_string buf ("[" ^ String.sub snippet.text ~pos:start ~len ^ "]");
      start + len)
  in
  Buffer.add_string buf (String.drop_prefix snippet.text pos);
  Buffer.contents buf
;;

let search ?(limit = 50) index (t : Tool_test_helpers.t) ~labels query =
  let sessions = Session.list ~dir:(sessions_dir t) in
  let loaded = ref 0 in
  let hits =
    Session_search.search
      index
      ~sessions
      ~load:(fun (s : Session.Summary.t) ->
        incr loaded;
        Result.ok (Session.load s.path))
      ~query
      ~limit
  in
  printf "%S (read %d)\n" query !loaded;
  List.iter hits ~f:(fun (hit : Session_search.Hit.t) ->
    printf
      "  %-8s %3d  %s\n"
      (List.Assoc.find labels hit.id ~equal:String.equal
       |> Option.value ~default:"?")
      hit.score
      (Option.value_map hit.snippet ~default:"-" ~f:render))
;;

let%expect_test "words" =
  print_s
    [%sexp
      (Session_search.words "Fix fix/ws-reconnect: café_au_lait 8080, x"
       : string list)];
  [%expect {| (fix fix ws reconnect "caf\195\169" au lait 8080 x) |}]
;;

let%expect_test "search: ranking, snippets, typos, updates" =
  with_sandbox
  @@ fun t ->
  let port =
    saved
      t
      ~name:"Make the port configurable"
      ~at:100.
      [ Message.user
          "prigh serve should take the listen port from the config file"
      ; assistant
          "I'll add a --port flag and read `port` from config.json.\n\n\
           The default stays 8080."
      ; tool_result "error: port 8080 already in use (xylophone)"
      ; Message.user "also make the websocket path configurable"
      ; assistant "Done: `ws_path` in the config, defaulting to /ws."
      ]
  in
  let compaction =
    saved
      t
      ~at:200.
      [ Message.user "why does the compaction summary lose the plan?"
      ; assistant
          (String.concat
             [ "Looking at it, there are three places where the context gets \
                cut down before the request is sent, and the first two are \
                fine. "
             ; "The summary prompt drops the plan section when the context is \
                long; I'll keep the plan verbatim in the compaction instead of \
                summarising it, which costs a few hundred tokens at most."
             ])
      ]
  in
  let phone =
    saved
      t
      ~name:"Phone layout"
      ~at:300.
      [ Message.user "on a 320px phone the session list overflows sideways"
      ; assistant "The sidebar's min-width forces the overflow; café menus too."
      ]
  in
  let labels =
    [ Session.id port, "port"
    ; Session.id compaction, "compact"
    ; Session.id phone, "phone"
    ]
  in
  let index = Session_search.create () in
  let search ?limit = search ?limit index t ~labels in
  List.iter
    [ "port"
    ; "config port"
    ; "configurable websocket"
    ; "compaktion"
    ; "plna"
    ; "flow"
    ; "fl"
    ; "plan compaction"
    ; "sideways overflow"
    ; "cafe"
    ; "café"
    ; "xylophone"
    ; "8080"
    ; "verbatim"
    ; "hundred verbatim"
    ; "looking"
    ; "zzz port"
    ; "  "
    ]
    ~f:search;
  [%expect
    {|
    "port" (read 3)
      port      40  …take the listen [port] from the config file
    "config port" (read 0)
      port      72  …take the listen [port] from the [config] file
    "configurable websocket" (read 0)
      port      60  also make the [websocket] path [configurable]
    "compaktion" (read 0)
      compact   12  why does the [compaction] summary lose the plan?
    "plna" (read 0)
      compact   12  …summary lose the [plan]?
    "flow" (read 0)
      phone     18  …session list over[flow]s sideways
    "fl" (read 0)
      port      16  I'll add a --port [fl]ag and read `port` from config.json. The default stays 8080.
    "plan compaction" (read 0)
      compact   60  why does the [compaction] summary lose the [plan]?
    "sideways overflow" (read 0)
      phone     54  …the session list [overflow]s [sideways]
    "cafe" (read 0)
      phone      8  …the overflow; [caf�]� menus too.
    "caf\195\169" (read 0)
      phone     20  …the overflow; [café] menus too.
    "xylophone" (read 0)
    "8080" (read 0)
      port      20  …The default stays [8080].
    "verbatim" (read 0)
      compact   20  …I'll keep the plan [verbatim] in the compaction instead of summarising it, which…
    "hundred verbatim" (read 0)
      compact   40  …I'll keep the plan [verbatim] in the compaction instead of summarising it, which…
    "looking" (read 0)
      compact   20  [Looking] at it, there are three places where the context gets cut down before the…
    "zzz port" (read 0)
    "  " (read 0)
    |}];
  (* Only what changed is read again. *)
  ignore
    (Session.append_message
       compaction
       ~at:400.
       (Message.user "now deploy it to kubernetes")
     : Session.Entry.t);
  search "kubernetes";
  ignore (Session.set_name phone ~name:"Mobile sidebar" : Session.Entry.t);
  search "mobile";
  search "phone";
  Session.delete_files (Session.path port);
  search "port";
  search ~limit:1 "the";
  [%expect
    {|
    "kubernetes" (read 1)
      compact   20  now deploy it to [kubernetes]
    "mobile" (read 1)
      phone     40  -
    "phone" (read 0)
      phone     30  on a 320px [phone] the session list overflows sideways
    "port" (read 0)
    "the" (read 0)
      compact   30  why does [the] compaction summary lose [the] plan?
    |}]
;;

let server (t : Tool_test_helpers.t) ~sw =
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let sessions_dir = sessions_dir t in
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
  Rpc_server.create
    ~env:t.env
    ~sw
    ~home:t.dir
    ~login
    ~sessions_dir
    ~cwd:t.dir
    ~new_agent
    ()
;;

let%expect_test "search_sessions: the RPC, live sessions" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let saved_one =
    saved
      t
      ~at:100.
      [ Message.user "make the port configurable"
      ; assistant "Added a `port` field to the config."
      ]
  in
  let server = server t ~sw in
  let client = Rpc_server.connect server ~send:ignore in
  let call meth params =
    let response =
      Rpc_server.handle
        server
        client
        (`Object
            [ "id", `String "r"
            ; "method", `String meth
            ; "params", `Object params
            ])
    in
    match Json.member "result" response with
    | Some result -> print_endline (mask t (Json.to_string result))
    | None -> print_endline (mask t (Json.to_string response))
  in
  let quiet meth params =
    ignore
      (Rpc_server.handle
         server
         client
         (`Object
             [ "id", `String "r"
             ; "method", `String meth
             ; "params", `Object params
             ])
       : Json.t)
  in
  quiet "hello" [];
  call "search_sessions" [ "query", `String "config port" ];
  call "search_sessions" [ "query", `String "" ];
  call "search_sessions" [ "query", `Number "1" ];
  call "search_sessions" [ "query", `String "port"; "limit", `String "x" ];
  (* Renamed while open: the live session's name counts. *)
  quiet "switch_session" [ "path", `String (Session.path saved_one) ];
  quiet "set_session_name" [ "name", `String "Listen address" ];
  call "search_sessions" [ "query", `String "listen" ];
  [%expect
    {|
    {"query":"config port","hits":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","score":54,"snippet":{"text":"make the port configurable","ranges":[[9,4],[14,6]]}}]}
    {"query":"","hits":[]}
    {"type":"response","id":"r","ok":false,"error":"param \"query\" must be a string"}
    {"type":"response","id":"r","ok":false,"error":"param \"limit\" must be an integer"}
    {"query":"listen","hits":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","score":40,"snippet":null}]}
    |}]
;;
