open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let make_server t ~sw ~backend_host ?before ?(children = []) replies =
  let provider = Routed_provider.create ?before ~main:replies children in
  let sessions_dir = t.dir ^/ "sessions" in
  let new_agent ?session ~cwd () =
    let agent_ref = ref None in
    let subagent =
      Tool_subagent.create
        ~provider
        ~current_model:(fun () ->
          (Agent.state (Option.value_exn !agent_ref)).model)
        ~current_thinking:(fun () -> Thinking.Off)
        ~home:t.dir
    in
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:(Tools.all @ (subagent :: Tool_subagent.control_tools))
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host
        ~cwd
        ()
    in
    agent_ref := Some agent;
    agent
  in
  Rpc_server.create
    ~env:t.env
    ~sw
    ~backend_host
    ~login:
      (Login_manager.create
         ~env:t.env
         ~sw
         ~getenv:(fun _ -> None)
         ~store:(Auth_store.create ~path:(t.dir ^/ "auth.json"))
         ())
    ~sessions_dir
    ~cwd:t.dir
    ~new_agent
    ()
;;

let call t server client ?(params = "{}") meth =
  let request =
    Json.of_string
      (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params)
  in
  print_endline
    (mask t (Json.to_string (Rpc_server.handle server client request)))
;;

let show_state t agent =
  let state = Agent.state agent in
  print_endline
    (mask_sexp
       t
       [%message
         ""
           ~active_host:(state.active_host : string)
           ~cwd:(state.cwd : string)
           ~git_branch:(state.git_branch : string option)
           ~hosts:(List.map state.hosts ~f:(fun h -> h.id) : string list)])
;;

let show_target t server ~session =
  print_endline
    (mask_sexp
       t
       (match Rpc_server.terminal_target server ~session with
        | `Backend cwd -> [%message "Backend" cwd]
        | `Host (id, cwd) -> [%message "Host" id cwd]
        | `Unavailable reason -> [%message "Unavailable" reason]))
;;

let new_tool_results agent ~seen =
  let results =
    List.filter_map (Agent.messages agent) ~f:(function
      | Message.Tool_result r -> Some r
      | _ -> None)
  in
  List.iter (List.drop results !seen) ~f:(fun r ->
    printf "tool_result %s: %S\n" r.tool_name r.text);
  seen := List.length results
;;

let member_string json name =
  match Json.member name json with
  | Some (`String s) -> s
  | _ -> ""
;;

(* Answers the host's [tool_exec]s pushed to [sent]: [$instructions] with no
   files, anything else with [reply name], printing each request. Stops once
   [agent] is idle. *)
let act_as_host t server host sent agent ~reply =
  let rec go () =
    match Queue.dequeue sent with
    | Some json when String.equal (member_string json "event") "tool_exec" ->
      let name = member_string json "name" in
      print_endline (mask t ("exec: " ^ Json.to_string json));
      let text =
        if String.equal name Host_ops.instructions_op then "[]" else reply name
      in
      ignore
        (Rpc_server.handle
           server
           host
           (Json.of_string
              (sprintf
                 {|{"id": 0, "method": "tool_exec_result", "params": {"exec_id": "%s", "text": "%s"}}|}
                 (member_string json "exec_id")
                 text))
         : Json.t);
      go ()
    | Some _ -> go ()
    | None ->
      if Agent.is_running agent || Agent.has_running_subagents agent
      then (
        Eio.Fiber.yield ();
        go ())
  in
  go ()
;;

let git_repo t = write t ".git/HEAD" "ref: refs/heads/main\n"

let%expect_test "backend host disabled" =
  with_sandbox
  @@ fun t ->
  git_repo t;
  Eio.Switch.run
  @@ fun sw ->
  let server =
    make_server
      t
      ~sw
      ~backend_host:false
      [ Reply.tool_call
          ~id:"c1"
          ~name:"bash"
          ~arguments:{|{"command":"pwd"}|}
          ()
      ; Reply.text "no host"
      ; Reply.tool_call
          ~id:"c2"
          ~name:"bash"
          ~arguments:{|{"command":"pwd"}|}
          ()
      ; Reply.text "on the laptop"
      ; Reply.tool_call
          ~id:"c3"
          ~name:"subagent"
          ~arguments:{|{"task":"look around"}|}
          ()
      ; Reply.text "delegated"
      ; Reply.text "noted the report"
      ; Reply.tool_call
          ~id:"c4"
          ~name:"bash"
          ~arguments:{|{"command":"pwd"}|}
          ()
      ; Reply.text "host gone"
      ]
      ~children:
        [ ( "look around"
          , [ Reply.tool_call ~id:"s1" ~name:"ls" ~arguments:"{}" ()
            ; Reply.text "sub done"
            ] )
        ]
  in
  let client = Rpc_server.connect server ~send:ignore in
  let agent = Rpc_server.agent_of_client server client in
  let seen = ref 0 in
  show_state t agent;
  call t server client ~params:{|{"host": "backend"}|} "set_active_host";
  call t server client ~params:{|{"host": "nobody"}|} "set_active_host";
  call t server client ~params:{|{"prefix": ""}|} "list_paths";
  call t server client ~params:{|{"prefix": "/"}|} "list_dirs";
  call t server client ~params:{|{"path": "/"}|} "set_cwd";
  call t server client ~params:{|{"command": "ls"}|} "shell";
  show_target t server ~session:None;
  show_target t server ~session:(Some (Session.id (Agent.session agent)));
  call t server client ~params:{|{"text": "go"}|} "prompt";
  Agent.wait_idle agent;
  new_tool_results agent ~seen;
  print_s
    [%sexp
      (Session.system_prompt (Agent.session agent) |> Option.is_some : bool)];
  [%expect
    {|
    ((active_host "") (cwd $DIR) (git_branch ()) (hosts ()))
    {"type":"response","id":"r","ok":false,"error":"the backend tool host is disabled"}
    {"type":"response","id":"r","ok":false,"error":"unknown tool host \"nobody\""}
    {"type":"response","id":"r","ok":false,"error":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host"}
    {"type":"response","id":"r","ok":false,"error":"tool host is not connected"}
    {"type":"response","id":"r","ok":false,"error":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host"}
    {"type":"response","id":"r","ok":true,"result":{"text":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host","is_error":true}}
    (Unavailable "no live session and the backend tool host is disabled")
    (Unavailable "no tool host connected")
    {"type":"response","id":"r","ok":true,"result":{}}
    tool_result bash: "no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host"
    true
    |}];
  (* A tool host connecting (attached to a session of its own) is adopted. *)
  let sent = Queue.create () in
  let laptop = Rpc_server.connect server ~send:(Queue.enqueue sent) in
  call
    t
    server
    laptop
    ~params:{|{"name": "laptop", "tools": true, "cwd": "/home/me/proj"}|}
    "hello";
  show_state t agent;
  show_target t server ~session:(Some (Session.id (Agent.session agent)));
  Queue.clear sent;
  call t server client ~params:{|{"text": "again"}|} "prompt";
  act_as_host t server laptop sent agent ~reply:(fun name ->
    name ^ " ran on laptop");
  new_tool_results agent ~seen;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"client_id":"client-2","namespace":null,"user":null,"superuser":false,"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"/home/me/proj","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"client-2","hosts":[{"id":"client-2","name":"laptop","cwd":"/home/me/proj","session_id":"<id>","session_name":null}],"subagents":[],"jobs":[]}}}
    ((active_host client-2) (cwd /home/me/proj) (git_branch ())
     (hosts (client-2)))
    (Host client-2 /home/me/proj)
    {"type":"response","id":"r","ok":true,"result":{}}
    exec: {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/c2-0","call_id":"c2","name":"bash","arguments":{"command":"pwd"},"cwd":"/home/me/proj"}
    tool_result bash: "bash ran on laptop"
    |}];
  (* The subagent's tools (and its instructions lookup) also go to the host,
     even after the turn that started it has ended. *)
  call t server client ~params:{|{"text": "delegate"}|} "prompt";
  act_as_host t server laptop sent agent ~reply:(fun name ->
    name ^ " ran on laptop");
  new_tool_results agent ~seen;
  List.iter (Agent.messages agent) ~f:(function
    | User { text; _ } when String.is_prefix text ~prefix:"[subagent" ->
      printf "delivered: %S\n" text
    | _ -> ());
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    exec: {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/c3-1","call_id":"c3","name":"$instructions","arguments":{"home":"$DIR"},"cwd":"/home/me/proj"}
    exec: {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/s1-2","call_id":"s1","name":"ls","arguments":{},"cwd":"/home/me/proj"}
    tool_result subagent: "started agent a1 (look around); its result will be delivered to you when it finishes; use subagent_wait to block on it"
    delivered: "[subagent a1 finished] look around\nsub done\n[subagent: 2 turns, 30 in / 13 out tokens, $0.0000]"
    |}];
  (* With the host gone there is nothing to run on. *)
  Rpc_server.disconnect server laptop;
  show_state t agent;
  call t server client ~params:{|{"text": "and again"}|} "prompt";
  Agent.wait_idle agent;
  new_tool_results agent ~seen;
  show_target t server ~session:(Some (Session.id (Agent.session agent)));
  [%expect
    {|
    ((active_host client-2) (cwd /home/me/proj) (git_branch ()) (hosts ()))
    {"type":"response","id":"r","ok":true,"result":{}}
    tool_result bash: "no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host"
    (Unavailable "the tool host \"client-2\" is not connected")
    |}]
;;

let%expect_test "backend host enabled: git branch and terminal targets" =
  with_sandbox
  @@ fun t ->
  git_repo t;
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~backend_host:true [] in
  let client = Rpc_server.connect server ~send:ignore in
  let agent = Rpc_server.agent_of_client server client in
  let session = Some (Session.id (Agent.session agent)) in
  show_state t agent;
  show_target t server ~session:None;
  show_target t server ~session;
  let laptop = Rpc_server.connect server ~send:ignore in
  ignore
    (Rpc_server.handle
       server
       laptop
       (Json.of_string
          (sprintf
             {|{"id": 1, "method": "hello", "params": {"tools": true, "cwd": "/home/me", "session": "%s"}}|}
             (Option.value_exn session)))
     : Json.t);
  show_target t server ~session;
  call t server client ~params:{|{"host": "backend"}|} "set_active_host";
  show_target t server ~session;
  [%expect
    {|
    ((active_host backend) (cwd $DIR) (git_branch (main)) (hosts (backend)))
    (Backend $DIR)
    (Backend $DIR)
    (Host client-2 /home/me)
    {"type":"response","id":"r","ok":true,"result":{}}
    (Backend $DIR)
    |}]
;;

let delivered agent =
  List.iter (Agent.messages agent) ~f:(function
    | User { text; _ } when String.is_prefix text ~prefix:"[subagent" ->
      printf "delivered: %S\n" text
    | _ -> ())
;;

let live_sessions server client =
  let request =
    Json.of_string {|{"id": "r", "method": "list_sessions", "params": {}}|}
  in
  match Json.member "result" (Rpc_server.handle server client request) with
  | Some (`Array sessions) ->
    List.iter sessions ~f:(fun s ->
      printf
        "session %S live=%s\n"
        (member_string s "first_prompt")
        (match Json.member "live" s with
         | Some `True -> "true"
         | _ -> "false"))
  | _ -> print_endline "no sessions"
;;

let rec wait_turn agent =
  if Agent.is_running agent
  then (
    Eio.Fiber.yield ();
    wait_turn agent)
;;

let%expect_test "subagents outlive their turn and a session switch; cancel RPC" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let gates = Routed_provider.Gates.create () in
  Routed_provider.Gates.hold gates "stuck";
  Routed_provider.Gates.hold gates "slow";
  let server =
    make_server
      t
      ~sw
      ~backend_host:true
      ~before:(Routed_provider.Gates.before gates)
      [ Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"stuck"}|}
          ()
      ; Reply.text "waiting"
      ; Reply.text "noted the cancel"
      ; Reply.tool_call
          ~id:"c2"
          ~name:"subagent"
          ~arguments:{|{"task":"slow"}|}
          ()
      ; Reply.text "spawned slow"
      ; Reply.text "slow arrived"
      ]
      ~children:
        [ "stuck", [ Reply.text "never" ]
        ; "slow", [ Reply.text "slow report" ]
        ]
  in
  let client = Rpc_server.connect server ~send:ignore in
  let agent = Rpc_server.agent_of_client server client in
  call t server client ~params:{|{"text": "go"}|} "prompt";
  wait_turn agent;
  call t server client ~params:{|{"agent_id": "a9"}|} "cancel_subagent";
  call t server client ~params:{|{"agent_id": "a1"}|} "cancel_subagent";
  Agent.wait_idle agent;
  delivered agent;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":false,"error":"unknown subagent \"a9\"; known: a1"}
    {"type":"response","id":"r","ok":true,"result":{}}
    delivered: "[subagent a1 failed] stuck\n[cancelled]\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]"
    |}];
  (* The client moves to a new session; the old one keeps its agent running
     (live, with no client) and receives the report. *)
  call t server client ~params:{|{"text": "again"}|} "prompt";
  wait_turn agent;
  call t server client "new_session";
  live_sessions server client;
  Routed_provider.Gates.release gates "slow";
  Agent.wait_idle agent;
  delivered agent;
  live_sessions server client;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    session "go" live=true
    delivered: "[subagent a1 failed] stuck\n[cancelled]\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]"
    delivered: "[subagent a2 finished] slow\nslow report\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]"
    session "go" live=false
    |}]
;;

let%expect_test "backend host disabled: no access to the backend's other files" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server = make_server t ~sw ~backend_host:false [] in
  let client = Rpc_server.connect server ~send:ignore in
  (* A saved session of this server, and a file it must not touch. *)
  let saved = Session.create ~dir:(t.dir ^/ "sessions") ~cwd:t.dir () in
  ignore (Session.set_name saved ~name:"saved" : Session.Entry.t);
  let elsewhere = Session.create ~dir:(t.dir ^/ "elsewhere") ~cwd:t.dir () in
  ignore (Session.set_name elsewhere ~name:"elsewhere" : Session.Entry.t);
  let secret = t.dir ^/ "secret.json" in
  Out_channel.write_all secret ~data:"{}";
  let path_param path = sprintf {|{"path": "%s"}|} path in
  call t server client ~params:(path_param (Session.path elsewhere)) "import";
  call
    t
    server
    client
    ~params:(path_param (Session.path elsewhere))
    "switch_session";
  call t server client ~params:(path_param secret) "delete_session";
  call
    t
    server
    client
    ~params:(path_param (t.dir ^/ "sessions/../secret.json"))
    "delete_session";
  print_s [%sexp (Sys_unix.file_exists_exn secret : bool)];
  call
    t
    server
    client
    ~params:(path_param (Session.path saved))
    "delete_session";
  print_s [%sexp (Sys_unix.file_exists_exn (Session.path saved) : bool)];
  [%expect
    {|
    {"type":"response","id":"r","ok":false,"error":"\"$DIR/elsewhere/<stamp>_<id>.jsonl\" is not in the sessions directory"}
    {"type":"response","id":"r","ok":false,"error":"\"$DIR/elsewhere/<stamp>_<id>.jsonl\" is not in the sessions directory"}
    {"type":"response","id":"r","ok":false,"error":"\"$DIR/secret.json\" is not in the sessions directory"}
    {"type":"response","id":"r","ok":false,"error":"\"$DIR/sessions/../secret.json\" is not in the sessions directory"}
    true
    {"type":"response","id":"r","ok":true,"result":{}}
    false
    |}];
  (* Exports are written by the tool host, as its user. *)
  let sent = Queue.create () in
  let laptop = Rpc_server.connect server ~send:(Queue.enqueue sent) in
  call
    t
    server
    laptop
    ~params:{|{"name": "laptop", "tools": true, "cwd": "/home/me/proj"}|}
    "hello";
  Eio.Fiber.both
    (fun () ->
       call
         t
         server
         client
         ~params:{|{"format": "markdown", "path": "out.md"}|}
         "export")
    (fun () ->
       let rec answer () =
         match Queue.dequeue sent with
         | Some json when String.equal (member_string json "event") "tool_exec"
           ->
           print_endline
             (mask
                t
                (sprintf
                   "exec %s %s"
                   (member_string json "name")
                   (Option.value_map
                      (Json.member "arguments" json)
                      ~default:""
                      ~f:(fun args -> member_string args "path"))));
           ignore
             (Rpc_server.handle
                server
                laptop
                (Json.of_string
                   (sprintf
                      {|{"id": 0, "method": "tool_exec_result", "params": {"exec_id": "%s", "text": "wrote"}}|}
                      (member_string json "exec_id")))
              : Json.t)
         | Some _ -> answer ()
         | None ->
           Eio.Fiber.yield ();
           answer ()
       in
       answer ());
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"client_id":"client-2","namespace":null,"user":null,"superuser":false,"state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"/home/me/proj","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"client-2","hosts":[{"id":"client-2","name":"laptop","cwd":"/home/me/proj","session_id":"<id>","session_name":null}],"subagents":[],"jobs":[]}}}
    exec write out.md
    {"type":"response","id":"r","ok":true,"result":{"path":"/home/me/proj/out.md"}}
    |}]
;;
