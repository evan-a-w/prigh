open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Gates = Routed_provider.Gates
module Json = Jsonaf

let day = 86400.

(* A server over [t]'s sessions, its config in [t.dir]; clients collect what
   they are sent. *)
module Server = struct
  type t =
    { server : Rpc_server.t
    ; sessions_dir : string
    }

  let create ?default_session (t : Tool_test_helpers.t) ~sw ~provider =
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
        ~provider
        ~tools:Tools.all
        ~sessions_dir
        ~home:t.dir
        ?session
        ~cwd
        ()
    in
    let default_agent =
      Option.map default_session ~f:(fun path ->
        let session = Or_error.ok_exn (Session.load path) in
        new_agent ~session ~cwd:(Session.cwd session) ())
    in
    { server =
        Rpc_server.create
          ~env:t.env
          ~sw
          ~home:t.dir
          ~login
          ~sessions_dir
          ~cwd:t.dir
          ~new_agent
          ?default_agent
          ()
    ; sessions_dir
    }
  ;;
end

module Client = struct
  type t =
    { name : string
    ; client : Rpc_server.Client.t
    ; server : Rpc_server.t
    ; sent : Json.t Queue.t
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

  let bytes_re = Re.compile (Re.Perl.re {|"bytes":[0-9]+|})

  (* Sizes depend on the sandbox path; they are checked separately. *)
  let show t json =
    print_endline
      (Re.replace_string
         bytes_re
         ~by:{|"bytes":<n>|}
         (mask t (Json.to_string json)))
  ;;

  (* Prints the outcome of a call. *)
  let call t (c : t) meth params = show t (request c meth params)

  let connect ?session (server : Server.t) name =
    let sent = Queue.create () in
    let client = Rpc_server.connect server.server ~send:(Queue.enqueue sent) in
    let c = { name; client; server = server.server; sent } in
    let params =
      Option.value_map session ~default:[] ~f:(fun s ->
        [ "session", `String s ])
    in
    ignore (request c "hello" params : Json.t);
    Queue.clear sent;
    c
  ;;

  let agent (c : t) = Rpc_server.agent_of_client c.server c.client

  (* How many [sessions_changed] events [c] got since the last call. *)
  let sessions_changed (c : t) =
    let n =
      Queue.count c.sent ~f:(fun json ->
        match Json.member "event" json with
        | Some (`String "sessions_changed") -> true
        | _ -> false)
    in
    Queue.clear c.sent;
    printf "%s: %d sessions_changed\n" c.name n
  ;;
end

(* One line per session: the name (else the first prompt) and how it is
   listed. *)
let print_sessions (c : Client.t) =
  match Client.result (Client.request c "list_sessions" []) with
  | Error e -> print_endline e
  | Ok sessions ->
    List.iter
      (Option.value (Json.list sessions) ~default:[])
      ~f:(fun s ->
        let field name = Json.member name s in
        let str name =
          match field name with
          | Some (`String s) -> s
          | _ -> "-"
        in
        let flag name =
          match field name with
          | Some `True -> Some name
          | _ -> None
        in
        let number name =
          match field name with
          | Some (`Number n) -> n
          | _ -> "-"
        in
        printf
          "%-10s %s background=%s clients=%s\n"
          (match field "name" with
           | Some (`String n) -> n
           | _ -> str "first_prompt")
          (String.concat
             ~sep:" "
             (List.filter_opt [ flag "archived"; flag "running"; flag "live" ]))
          (number "background")
          (number "clients"))
;;

(* A saved session named [name], last active [days_ago] days ago. *)
let saved (t : Tool_test_helpers.t) ?(days_ago = 0.) name =
  let session =
    Session.create ~dir:(Filename.concat t.dir "sessions") ~cwd:t.dir ()
  in
  ignore (Session.set_name session ~name : Session.Entry.t);
  let at = Core_unix.gettimeofday () -. (days_ago *. day) in
  ignore
    (Session.append_message session ~at (Message.user ("about " ^ name))
     : Session.Entry.t);
  (* The listing is in this order, most recent first. *)
  Core_unix.utimes (Session.path session) ~access:at ~modif:at;
  session
;;

let path session = [ "path", `String (Session.path session) ]

let%expect_test
    "archiving: by age and by hand, kept on disk, undone by activity"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let (_ : Session.t) = saved t "ancient" ~days_ago:30. in
  let old = saved t "old" ~days_ago:10. in
  let recent = saved t "recent" ~days_ago:0.1 in
  let gates = Gates.create () in
  let provider =
    Routed_provider.create
      ~before:(Gates.before gates)
      ~main:[ Reply.text "welcome back"; Reply.text "still here" ]
      []
  in
  let server = Server.create t ~sw ~provider in
  let a = Client.connect server "a" in
  let b = Client.connect server "b" ~session:(Session.id old) in
  print_sessions a;
  [%expect
    {|
    recent      background=0 clients=-
    old        archived live background=0 clients=1
    ancient    archived background=0 clients=-
    |}];
  Client.call t a "set_archived" (path recent @ [ "archived", `True ]);
  Client.call
    t
    a
    "set_archived"
    [ "path", `String (Session.id old); "archived", `False ];
  Client.call t a "set_archived" (path recent);
  Client.call t a "set_archived" [ "path", `String "nope"; "archived", `True ];
  print_sessions a;
  Client.sessions_changed a;
  Client.sessions_changed b;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":false,"error":"missing param \"archived\""}
    {"type":"response","id":"r","ok":false,"error":"no such session \"nope\""}
    old        live background=0 clients=1
    recent     archived background=0 clients=-
    ancient    archived background=0 clients=-
    a: 2 sessions_changed
    b: 2 sessions_changed
    |}];
  (* A fresh server over the same files sees the same; the threshold is
     the config's, reread on every listing. *)
  let server2 = Server.create t ~sw ~provider in
  let c = Client.connect server2 "c" in
  print_sessions c;
  Client.call
    t
    c
    "set_config"
    [ "config", `Object [ "archive_after_days", `Number "0" ] ];
  print_sessions c;
  Client.call
    t
    c
    "set_config"
    [ "config", `Object [ "archive_after_days", `String "7" ] ];
  [%expect
    {|
    old         background=0 clients=-
    recent     archived background=0 clients=-
    ancient    archived background=0 clients=-
    {"type":"response","id":"r","ok":true,"result":{"scoped_models":[],"approval_mode":"auto","permissions":{"allow":[],"ask":[],"deny":[]},"default_model":null,"default_thinking":null,"fallback_models":[],"default_cwd":null,"max_budget_usd":null,"goal_max_continuations":20,"resume_on_limit_reset":true,"archive_after_days":0,"notify":"bell"}}
    old         background=0 clients=-
    recent     archived background=0 clients=-
    ancient     background=0 clients=-
    {"type":"response","id":"r","ok":false,"error":"config.archive_after_days must be a number of days (0: never)"}
    |}];
  (* A new message unarchives; a running session cannot be archived. *)
  Client.call t c "switch_session" (path recent);
  Gates.hold gates "main";
  Client.call t c "prompt" [ "text", `String "back again" ];
  Client.call t c "set_archived" (path recent @ [ "archived", `True ]);
  print_sessions c;
  Gates.release gates "main";
  Agent.wait_idle (Client.agent c);
  print_sessions c;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":"recent","session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"on","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[],"jobs":[],"budget_usd":null,"goal":null,"resume_at_ms":null,"approval_mode":"auto","plan_mode":false,"plan":null,"session_rules":[]}}}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":false,"error":"session \"recent\" is running: wait for it or /abort it before archiving"}
    recent     running live background=0 clients=1
    old         background=0 clients=-
    ancient     background=0 clients=-
    recent     live background=0 clients=1
    old         background=0 clients=-
    ancient     background=0 clients=-
    |}]
;;

let%expect_test
    "a server starting loads the sessions waiting to resume, to resume on time"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let now = Core_unix.gettimeofday () in
  let waiting = saved t "waiting" in
  Session.set_resume_at waiting (Some (now +. 3600.));
  let lapsed = saved t "lapsed" ~days_ago:0.1 in
  Session.set_resume_at lapsed (Some (now -. 60.));
  let (_ : Session.t) = saved t "plain" ~days_ago:0.2 in
  (* Listed in the order of their messages. *)
  List.iter
    [ waiting, now; lapsed, now -. (0.1 *. day) ]
    ~f:(fun (s, at) -> Core_unix.utimes (Session.path s) ~access:at ~modif:at);
  let provider = Faux_provider.create [] in
  let server = Server.create t ~sw ~provider in
  Rpc_server.restore_resumes server.server;
  let a = Client.connect server "a" in
  print_sessions a;
  (match Client.result (Client.request a "switch_session" (path waiting)) with
   | Ok result ->
     (match Json.member "state" result with
      | Some state ->
        printf
          "resume_at_ms set: %b\n"
          (match Json.member "resume_at_ms" state with
           | Some (`Number _) -> true
           | _ -> false)
      | None -> print_endline "no state")
   | Error e -> print_endline e);
  Client.call t a "cancel_resume" [];
  print_endline
    (Option.value_map
       (Session.resume_at
          (Or_error.ok_exn (Session.load (Session.path waiting))))
       ~default:"no resume recorded"
       ~f:(fun _ -> "still recorded"));
  [%expect
    {|
    waiting    live background=0 clients=0
    lapsed      background=0 clients=-
    plain       background=0 clients=-
    resume_at_ms set: true
    {"type":"response","id":"r","ok":true,"result":{}}
    no resume recorded
    |}]
;;

let gated = "while [ ! -f gate ]; do sleep 0.02; done; echo done"

let%expect_test "a session is active while its background work runs" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Faux_provider.create [ Reply.text "hello"; Reply.text "the job is done" ]
  in
  let server = Server.create t ~sw ~provider in
  let a = Client.connect server "a" in
  let b = Client.connect server "b" in
  Client.call t a "prompt" [ "text", `String "start" ];
  let agent = Client.agent a in
  Agent.wait_idle agent;
  Client.call t a "set_session_name" [ "name", `String "worker" ];
  Client.sessions_changed a;
  Client.sessions_changed b;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    a: 4 sessions_changed
    b: 4 sessions_changed
    |}];
  Client.call t a "shell" [ "command", `String gated; "background", `True ];
  Client.call t a "new_session" [];
  print_sessions b;
  Client.call
    t
    b
    "delete_sessions"
    [ "paths", `Array [ `String (Session.path (Agent.session agent)) ] ];
  Client.sessions_changed b;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"job_id":"j1"}}
    {"type":"response","id":"r","ok":true,"result":{"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"on","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[],"jobs":[],"budget_usd":null,"goal":null,"resume_at_ms":null,"approval_mode":"auto","plan_mode":false,"plan":null,"session_rules":[]}}}
    worker     running live background=1 clients=0
    {"type":"response","id":"r","ok":true,"result":{"deleted":[],"refused":[{"path":"$DIR/sessions/<stamp>_<id>.jsonl","error":"session \"worker\" is running: wait for it or /abort it first"}]}}
    b: 1 sessions_changed
    |}];
  write t "gate" "";
  Agent.wait_idle agent;
  print_sessions b;
  Client.sessions_changed a;
  Client.sessions_changed b;
  [%expect
    {|
    worker      background=0 clients=-
    a: 3 sessions_changed
    b: 2 sessions_changed
    |}]
;;

let%expect_test "deleting sessions: refusals, files, delete_archived" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let default = saved t "default" ~days_ago:0.4 in
  let mine = saved t "mine" ~days_ago:0.3 in
  let shared = saved t "shared" ~days_ago:0.2 in
  let plain = saved t "plain" ~days_ago:0.1 in
  let (_ : Session.t) = saved t "stale1" ~days_ago:8. in
  let (_ : Session.t) = saved t "stale2" ~days_ago:9. in
  let data = Session.data_dir (Session.path plain) in
  Core_unix.mkdir_p data;
  Out_channel.write_all (Filename.concat data "out.txt") ~data:"12345";
  let log_size =
    Int64.to_int_exn (Core_unix.stat (Session.path plain)).st_size
  in
  let server =
    Server.create
      t
      ~sw
      ~provider:(Faux_provider.create [])
      ~default_session:(Session.path default)
  in
  let a = Client.connect server "a" ~session:(Session.id mine) in
  let b = Client.connect server "b" ~session:(Session.id shared) in
  let c = Client.connect server "c" in
  print_sessions a;
  [%expect
    {|
    plain       background=0 clients=-
    shared     live background=0 clients=1
    mine       live background=0 clients=1
    default    live background=0 clients=1
    stale1     archived background=0 clients=-
    stale2     archived background=0 clients=-
    |}];
  (match
     Client.result
       (Client.request
          a
          "delete_sessions"
          [ ( "paths"
            , `Array
                (List.map
                   [ Session.path mine
                   ; Session.path shared
                   ; Session.path default
                   ; Session.id plain
                   ; "nope"
                   ]
                   ~f:(fun p -> `String p)) )
          ])
   with
   | Error e -> print_endline e
   | Ok result ->
     Client.show t result;
     (match Json.member "deleted" result with
      | Some (`Array [ deleted ]) ->
        (match Json.member "bytes" deleted with
         | Some (`Number n) ->
           printf "bytes: log + %d\n" (Int.of_string n - log_size)
         | _ -> ())
      | _ -> ()));
  print_s
    [%sexp
      (Sys_unix.file_exists_exn (Session.path plain) : bool)
    , (Sys_unix.file_exists_exn data : bool)];
  Client.sessions_changed a;
  Client.sessions_changed b;
  Client.sessions_changed c;
  [%expect
    {|
    {"deleted":[{"path":"$DIR/sessions/<stamp>_<id>.jsonl","id":"<id>","bytes":<n>}],"refused":[{"path":"$DIR/sessions/<stamp>_<id>.jsonl","error":"session \"mine\" is the session you are in: switch to another one first"},{"path":"$DIR/sessions/<stamp>_<id>.jsonl","error":"session \"shared\" is open in 1 other frontend(s): close it there first"},{"path":"$DIR/sessions/<stamp>_<id>.jsonl","error":"session \"default\" is the backend's default session (-session): restart the backend without -session to delete it"},{"path":"nope","error":"no such session \"nope\""}]}
    bytes: log + 5
    (false false)
    a: 1 sessions_changed
    b: 1 sessions_changed
    c: 1 sessions_changed
    |}];
  Client.call t c "delete_session" (path shared);
  Client.call t c "delete_session" (path plain);
  Client.call t c "set_archived" (path shared @ [ "archived", `True ]);
  Client.call t c "delete_archived" [];
  print_sessions c;
  [%expect
    {|
    {"type":"response","id":"r","ok":false,"error":"session \"shared\" is open in 1 other frontend(s): close it there first"}
    {"type":"response","id":"r","ok":false,"error":"no such session \"$DIR/sessions/<stamp>_<id>.jsonl\""}
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{"deleted":[{"path":"$DIR/sessions/<stamp>_<id>.jsonl","id":"<id>","bytes":<n>},{"path":"$DIR/sessions/<stamp>_<id>.jsonl","id":"<id>","bytes":<n>}],"refused":[{"path":"$DIR/sessions/<stamp>_<id>.jsonl","error":"session \"shared\" is open in 1 other frontend(s): close it there first"}]}}
    shared     archived live background=0 clients=1
    mine       live background=0 clients=1
    default    live background=0 clients=1
    |}]
;;

let%expect_test "opening a session with unreadable lines tells the client" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let damaged = saved t "damaged" in
  Out_channel.with_file (Session.path damaged) ~append:true ~f:(fun oc ->
    Out_channel.output_string oc {|["Entry",{"id":"torn","par|});
  let fine = saved t "fine" in
  let server = Server.create t ~sw ~provider:(Faux_provider.create []) in
  let a = Client.connect server "a" in
  print_sessions a;
  let open_ session =
    Rpc_server.respond
      server.server
      a.client
      (`Object
          [ "id", `String "r"
          ; "method", `String "switch_session"
          ; "params", `Object (path session)
          ]);
    Queue.iter a.sent ~f:(fun json ->
      match Json.member "type" json with
      | Some (`String "response") ->
        print_endline
          (match Client.result json with
           | Ok _ -> "response: ok"
           | Error e -> "response: " ^ e)
      | _ ->
        (match Json.member "event" json with
         | Some (`String "notice") -> Client.show t json
         | _ -> ()));
    Queue.clear a.sent
  in
  open_ damaged;
  open_ fine;
  open_ damaged;
  [%expect
    {|
    fine        background=0 clients=-
    damaged     background=0 clients=-
    response: ok
    {"type":"event","event":"notice","text":"parts of this session could not be read and are left out:\nline 4: skipped an incomplete last line (prigh probably stopped while writing it)"}
    response: ok
    response: ok
    {"type":"event","event":"notice","text":"parts of this session could not be read and are left out:\nline 4: skipped an incomplete last line (prigh probably stopped while writing it)"}
    |}]
;;
