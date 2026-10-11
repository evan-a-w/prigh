open! Core
open! Prigh
open Tool_test_helpers
open Test_background_subagents
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates
module Json = Jsonaf

let ms_re = Re.compile (Re.Perl.re {|"(\w+_at_ms|at|bytes)":[0-9]+|})

let show t json =
  print_endline
    (Re.replace ms_re json ~f:(fun g -> sprintf {|"%s":<n>|} (Re.Group.get g 1))
     |> mask t)
;;

let subagents_json agent =
  List.map (Agent.subagents agent) ~f:(fun s ->
    Json.to_string (Rpc_json.subagent_summary s))
;;

(* What [list_subagents] and [get_subagent] answer. *)
let print_subagents t agent ~get =
  print_endline "--- list_subagents";
  List.iter (subagents_json agent) ~f:(show t);
  List.iter get ~f:(fun key ->
    printf "--- get_subagent %s\n" key;
    match Agent.subagent agent key with
    | None -> print_endline "none"
    | Some found -> show t (Json.to_string (Rpc_json.subagent found)))
;;

let log_file session =
  Filename.concat (Session.data_dir (Session.path session)) "subagents.jsonl"
;;

(* The saved log, one line per record: its kind and subagent. *)
let print_log_file session =
  print_endline "--- subagents.jsonl";
  List.iter
    (In_channel.read_lines (log_file session))
    ~f:(fun line ->
      match Json.of_string line with
      | `Array [ `String kind; fields ] ->
        let field name =
          match Json.member name fields with
          | Some (`String s) -> " " ^ s
          | Some (`Number n) -> sprintf " %s=%s" name n
          | _ -> ""
        in
        printf "%s%s%s\n" kind (field "id") (field "run")
      | _ -> print_endline "?")
;;

let create_agent
      (t : Tool_test_helpers.t)
      ~sw
      ~provider
      ?session
      ?(cwd = t.dir)
      ()
  =
  Agent.create
    ~env:t.env
    ~sw
    ~provider
    ~tools:(Tools.all @ subagent_tools ~provider)
    ~sessions_dir:(Filename.concat t.dir "sessions")
    ~home:t.dir
    ?session
    ~cwd
    ()
;;

let reload agent =
  Or_error.ok_exn (Session.load (Session.path (Agent.session agent)))
;;

let%expect_test "the log is saved with the session and reloaded with it" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Routed_provider.create
      ~main:
        [ spawn "outer"
        ; Reply.text "started"
        ; Reply.text "noted"
        ; spawn ~id:"c9" "again"
        ; Reply.text "started again"
        ; Reply.text "noted again"
        ]
      [ "outer", [ spawn ~id:"n1" "inner"; Reply.text "outer done" ]
      ; "inner", [ Reply.text "inner done" ]
      ; "again", [ Reply.text "again done" ]
      ]
  in
  let agent = create_agent t ~sw ~provider () in
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  let before = subagents_json agent in
  print_log_file (Agent.session agent);
  [%expect
    {|
    --- subagents.jsonl
    Start a1 run=1
    Message a1
    Message a1
    Start a1/n1 run=1
    Message a1/n1
    Message a1/n1
    End a1/n1 run=1
    Message a1
    Message a1
    End a1 run=1
    Run run=2
    |}];
  (* The agent goes away (eviction, a restart); a new one over the session
     file lists the same. *)
  let agent = create_agent t ~sw ~provider ~session:(reload agent) () in
  print_s [%sexp (List.equal String.equal before (subagents_json agent) : bool)];
  print_subagents t agent ~get:[ "a1"; "n1"; "a1/n1"; "nope" ];
  [%expect
    {|
    true
    --- list_subagents
    {"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":4,"stale":true,"result":{"text":"outer done\n[subagent: 2 turns, 40 in / 18 out tokens, $0.0000]","is_error":false}}
    {"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}
    --- get_subagent a1
    {"subagent":{"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":4,"stale":true,"result":{"text":"outer done\n[subagent: 2 turns, 40 in / 18 out tokens, $0.0000]","is_error":false}},"messages":[{"role":"user","text":"outer","at":<n>},{"role":"assistant","content":[{"type":"tool_call","id":"n1","name":"subagent","arguments":"{\"task\":\"inner\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash","at":<n>},{"role":"tool_result","tool_call_id":"n1","tool_name":"subagent","text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false,"at":<n>},{"role":"assistant","content":[{"type":"text","text":"outer done"}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash","at":<n>}]}
    --- get_subagent n1
    {"subagent":{"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}},"messages":[{"role":"user","text":"inner","at":<n>},{"role":"assistant","content":[{"type":"text","text":"inner done"}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash","at":<n>}]}
    --- get_subagent a1/n1
    {"subagent":{"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}},"messages":[{"role":"user","text":"inner","at":<n>},{"role":"assistant","content":[{"type":"text","text":"inner done"}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash","at":<n>}]}
    --- get_subagent nope
    none
    |}];
  (* A new run makes them stale, and a new subagent gets a fresh id. *)
  Or_error.ok_exn (Agent.prompt agent "more");
  Agent.wait_idle agent;
  print_subagents t agent ~get:[];
  [%expect
    {|
    --- list_subagents
    {"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":4,"stale":true,"result":{"text":"outer done\n[subagent: 2 turns, 40 in / 18 out tokens, $0.0000]","is_error":false}}
    {"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}
    {"id":"a2","call_id":"c9","parent":null,"task":"again","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"again done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}
    |}];
  (* In place, the log follows the session. *)
  let path = Session.path (Agent.session agent) in
  Agent.new_session agent;
  print_subagents t agent ~get:[];
  Or_error.ok_exn (Agent.switch_session agent ~path);
  print_subagents t agent ~get:[];
  [%expect
    {|
    --- list_subagents
    --- list_subagents
    {"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":4,"stale":true,"result":{"text":"outer done\n[subagent: 2 turns, 40 in / 18 out tokens, $0.0000]","is_error":false}}
    {"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}
    {"id":"a2","call_id":"c9","parent":null,"task":"again","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"again done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}
    |}]
;;

let%expect_test "subagents running when the backend stops load as interrupted" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let gates = Gates.create () in
  Gates.hold gates "inner";
  let provider =
    Routed_provider.create
      ~before:(Gates.before gates)
      ~main:[ spawn "outer"; Reply.text "started"; Reply.text "noted" ]
      [ "outer", [ spawn ~id:"n1" "inner"; Reply.text "outer done" ]
      ; "inner", [ Reply.text "inner done" ]
      ]
  in
  let agent = create_agent t ~sw ~provider () in
  Or_error.ok_exn (Agent.prompt agent "go");
  let rec wait_nested () =
    if
      not
        (List.exists (Agent.subagents agent) ~f:(fun s ->
           String.equal s.id "a1/n1"))
    then (
      Eio.Fiber.yield ();
      wait_nested ())
  in
  wait_nested ();
  wait_turn agent;
  let session = reload agent in
  (* The backend died while writing a line. *)
  Out_channel.with_file (log_file session) ~append:true ~f:(fun oc ->
    Out_channel.output_string oc {|["Message",{"id":"a1","at":|});
  (* Rewound to before the subagent call: its id must still not be reused. *)
  let prompt =
    List.find_exn (Session.entries session) ~f:(fun e ->
      match e.payload with
      | Message (User _) -> true
      | _ -> false)
  in
  Or_error.ok_exn (Session.rewind session ~to_:prompt.id);
  let provider2 =
    Routed_provider.create
      ~main:[ spawn ~id:"c5" "later"; Reply.text "started"; Reply.text "noted" ]
      [ "later", [ Reply.text "later done" ] ]
  in
  let restarted = create_agent t ~sw ~provider:provider2 ~session () in
  print_subagents t restarted ~get:[ "a1/n1" ];
  [%expect
    {|
    --- list_subagents
    {"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"failed","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":false,"result":{"text":"[interrupted] the backend stopped before it finished","is_error":true}}
    {"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"failed","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":0,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":1,"stale":false,"result":{"text":"[interrupted] the backend stopped before it finished","is_error":true}}
    --- get_subagent a1/n1
    {"subagent":{"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"failed","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":0,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":1,"stale":false,"result":{"text":"[interrupted] the backend stopped before it finished","is_error":true}},"messages":[{"role":"user","text":"inner","at":<n>}]}
    |}];
  Or_error.ok_exn (Agent.prompt restarted "again");
  Agent.wait_idle restarted;
  print_subagents t restarted ~get:[];
  [%expect
    {|
    --- list_subagents
    {"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"failed","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"[interrupted] the backend stopped before it finished","is_error":true}}
    {"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"failed","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":0,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":1,"stale":true,"result":{"text":"[interrupted] the backend stopped before it finished","is_error":true}}
    {"id":"a2","call_id":"c5","parent":null,"task":"later","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"later done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}
    |}];
  Gates.release gates "inner";
  Agent.wait_idle agent
;;

let%expect_test
    "over RPC: an evicted session comes back with its subagents; deleting it \
     removes the log"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Routed_provider.create
      ~main:[ spawn "outer"; Reply.text "started"; Reply.text "noted" ]
      [ "outer", [ spawn ~id:"n1" "inner"; Reply.text "outer done" ]
      ; "inner", [ Reply.text "inner done" ]
      ]
  in
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home:t.dir
      ~login
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~cwd:t.dir
      ~new_agent:(fun ?session ~cwd () ->
        create_agent t ~sw ~provider ?session ~cwd ())
      ()
  in
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
    | Some result -> result
    | None -> response
  in
  let (_ : Json.t) = call "hello" [] in
  let (_ : Json.t) = call "prompt" [ "text", `String "go" ] in
  let agent = Rpc_server.agent_of_client server client in
  Agent.wait_idle agent;
  let session = Agent.session agent in
  let id = Session.id session in
  let (_ : Json.t) = call "new_session" [] in
  let (_ : Json.t) = call "switch_session" [ "path", `String id ] in
  print_s
    [%sexp
      "evicted and loaded again"
    , (not (phys_equal agent (Rpc_server.agent_of_client server client)) : bool)];
  show t (Json.to_string (call "list_subagents" []));
  show t (Json.to_string (call "get_subagent" [ "id", `String "n1" ]));
  [%expect
    {|
    ("evicted and loaded again" true)
    [{"id":"a1","call_id":"c1","parent":null,"task":"outer","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":2,"tool_calls":1,"current_tool":null,"current_tool_started_at_ms":null,"message_count":4,"stale":true,"result":{"text":"outer done\n[subagent: 2 turns, 40 in / 18 out tokens, $0.0000]","is_error":false}},{"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}}]
    {"subagent":{"id":"a1/n1","call_id":"n1","parent":"a1","task":"inner","model":"deepseek-flash","state":"complete","started_at_ms":<n>,"updated_at_ms":<n>,"ended_at_ms":<n>,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":true,"result":{"text":"inner done\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]","is_error":false}},"messages":[{"role":"user","text":"inner","at":<n>},{"role":"assistant","content":[{"type":"text","text":"inner done"}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash","at":<n>}]}
    |}];
  let (_ : Json.t) = call "new_session" [] in
  let data = Session.data_dir (Session.path session) in
  print_s
    [%sexp "log saved", (Sys_unix.file_exists_exn (log_file session) : bool)];
  show
    t
    (Json.to_string
       (call
          "delete_sessions"
          [ "paths", `Array [ `String (Session.path session) ] ]));
  print_s
    [%sexp
      "after delete"
    , (Sys_unix.file_exists_exn (Session.path session) : bool)
    , (Sys_unix.file_exists_exn data : bool)];
  [%expect
    {|
    ("log saved" true)
    {"deleted":[{"path":"$DIR/sessions/<stamp>_<id>.jsonl","id":"<id>","bytes":<n>}],"refused":[]}
    ("after delete" false false)
    |}]
;;
