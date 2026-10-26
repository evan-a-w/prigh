open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* A tool host whose home is not the backend's (the Docker deployment: the
   backend runs as [prigh], each user's tool host as that user; or a remote
   backend and a laptop): [~] in paths meant for the host is the host's. *)

let member_string json name =
  match Json.member name json with
  | Some (`String s) -> s
  | _ -> ""
;;

let with_home home f =
  let old = Sys.getenv "HOME" in
  Core_unix.putenv ~key:"HOME" ~data:home;
  Exn.protect ~f ~finally:(fun () ->
    Option.iter old ~f:(fun data -> Core_unix.putenv ~key:"HOME" ~data))
;;

(* Answers the host's [tool_exec]s: the [$] operations really run, with the
   host's [HOME]; tools just say where they ran. *)
let serve_host t server host ~sw ~home stream =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec go () =
      let json = Eio.Stream.take stream in
      if String.equal (member_string json "event") "tool_exec"
      then (
        let name = member_string json "name" in
        let cwd = member_string json "cwd" in
        let arguments =
          Option.value (Json.member "arguments" json) ~default:(`Object [])
        in
        print_endline
          (mask
             t
             (sprintf "host: %s %s in %s" name (Json.to_string arguments) cwd));
        let result : Tool.Result.t =
          if String.equal name Host_ops.instructions_op
          then Tool.Result.ok "[]"
          else if String.is_prefix name ~prefix:"$"
          then
            with_home home (fun () ->
              Host_ops.execute
                ~mcp:None
                ~env:t.env
                ~cancel:Cancellation.never
                ~on_output:ignore
                ~cwd
                ~name
                ~arguments
                ())
          else Tool.Result.ok ("ran in " ^ cwd)
        in
        ignore
          (Rpc_server.handle
             server
             host
             (`Object
                 [ "id", `Number "0"
                 ; "method", `String "tool_exec_result"
                 ; ( "params"
                   , `Object
                       [ "exec_id", `String (member_string json "exec_id")
                       ; "text", `String result.text
                       ; ("is_error", if result.is_error then `True else `False)
                       ] )
                 ])
           : Json.t));
      go ()
    in
    go ())
;;

let make_server t ~sw replies ~children =
  let provider = Routed_provider.create ~main:replies children in
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
        ()
    in
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:
          (Tools.all
           @ (subagent :: Tool_subagent.control_tools)
           @ [ Tool_worktree.create () ])
        ~sessions_dir
        ~home:t.dir
        ?session
        ~backend_host:false
        ~cwd
        ()
    in
    agent_ref := Some agent;
    agent
  in
  Rpc_server.create
    ~env:t.env
    ~sw
    ~backend_host:false
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

let show_cwd t agent =
  print_endline (mask t ("cwd: " ^ (Agent.state agent).cwd))
;;

(* [says_home]: the host's [hello] gives its home; an older host's does not
   (the backend asks it once). *)
let scenario ~says_home =
  with_sandbox
  @@ fun t ->
  List.iter
    [ "backend-home/sub"; "laptop-home/sub"; "laptop-home/x"; "work" ]
    ~f:(fun dir -> Core_unix.mkdir_p (t.dir ^/ dir));
  with_home (t.dir ^/ "backend-home")
  @@ fun () ->
  Eio.Switch.run
  @@ fun sw ->
  let server =
    make_server
      t
      ~sw
      [ Reply.tool_call
          ~id:"w1"
          ~name:"worktree"
          ~arguments:{|{"action":"list","cwd":"~/sub"}|}
          ()
      ; Reply.text "listed"
      ; Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"look","cwd":"~/sub"}|}
          ()
      ; Reply.text "delegated"
      ; Reply.text "noted"
      ]
      ~children:
        [ ( "look"
          , [ Reply.tool_call ~id:"s1" ~name:"ls" ~arguments:"{}" ()
            ; Reply.text "sub done"
            ] )
        ]
  in
  let client = Rpc_server.connect server ~send:ignore in
  let agent = Rpc_server.agent_of_client server client in
  let sent = Eio.Stream.create Int.max_value in
  let laptop = Rpc_server.connect server ~send:(Eio.Stream.add sent) in
  serve_host t server laptop ~sw ~home:(t.dir ^/ "laptop-home") sent;
  ignore
    (Rpc_server.handle
       server
       laptop
       (Json.of_string
          (sprintf
             {|{"id": 1, "method": "hello", "params": {"name": "laptop", "tools": true, "host_id": "laptop", "cwd": "%s/work"%s}}|}
             t.dir
             (if says_home
              then sprintf {|, "home": "%s/laptop-home"|} t.dir
              else "")))
     : Json.t);
  show_cwd t agent;
  print_endline
    (mask
       t
       (Json.to_string
          (Option.value_exn
             (Json.member "hosts" (Rpc_json.state (Agent.state agent))))));
  print_endline "--- /cd ~, /cd ~/sub, Tab after /cd ~/s";
  call t server client ~params:{|{"path": "~"}|} "set_cwd";
  show_cwd t agent;
  call t server client ~params:{|{"path": "~/sub"}|} "set_cwd";
  show_cwd t agent;
  call t server client ~params:{|{"prefix": "~/s"}|} "list_dirs";
  print_endline "--- /host laptop with the directory ~/x";
  call
    t
    server
    client
    ~params:{|{"host": "laptop", "cwd": "~/x"}|}
    "set_active_host";
  show_cwd t agent;
  print_endline "--- a switch into the worktree ~/sub, then back";
  call t server client ~params:{|{"path": "~/sub", "worktree": true}|} "set_cwd";
  show_cwd t agent;
  call t server client "worktree_back";
  show_cwd t agent;
  print_endline "--- the review of ~/sub";
  call t server client ~params:{|{"dir": "~/sub"}|} "git_diff";
  print_endline "--- the worktree tool with cwd ~/sub";
  call t server client ~params:{|{"text": "list"}|} "prompt";
  Agent.wait_idle agent;
  print_endline "--- a subagent with cwd ~/sub";
  call t server client ~params:{|{"text": "go"}|} "prompt";
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent do
    Eio.Fiber.yield ()
  done
;;

let%expect_test "~ is the tool host's home" =
  scenario ~says_home:true;
  [%expect
    {|
    cwd: $DIR/work
    [{"id":"laptop","name":"laptop","cwd":"$DIR/work","session_id":"<id>","session_name":null,"home":"$DIR/laptop-home"}]
    --- /cd ~, /cd ~/sub, Tab after /cd ~/s
    host: $resolve_dir {"path":"~"} in $DIR/work
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home","created":false}}
    cwd: $DIR/laptop-home
    host: $resolve_dir {"path":"~/sub"} in $DIR/laptop-home
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/sub","created":false}}
    cwd: $DIR/laptop-home/sub
    host: $list_dirs {"prefix":"~/s"} in $DIR/laptop-home/sub
    {"type":"response","id":"r","ok":true,"result":["~/sub/"]}
    --- /host laptop with the directory ~/x
    host: $resolve_dir {"path":"~/x"} in $DIR/laptop-home/sub
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/x","created":false}}
    cwd: $DIR/laptop-home/x
    --- a switch into the worktree ~/sub, then back
    host: $resolve_dir {"path":"~/sub"} in $DIR/laptop-home/x
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/sub","created":false}}
    cwd: $DIR/laptop-home/sub
    host: $resolve_dir {"path":"$DIR/laptop-home/x"} in $DIR/laptop-home/sub
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/x","created":false}}
    cwd: $DIR/laptop-home/x
    --- the review of ~/sub
    host: $git_diff {"dir":"~/sub","default_base":true} in $DIR/laptop-home/x
    {"type":"response","id":"r","ok":false,"error":"not a git repository: $DIR/laptop-home/sub (name a worktree of the repository: /worktree lists them)"}
    --- the worktree tool with cwd ~/sub
    {"type":"response","id":"r","ok":true,"result":{}}
    host: $instructions {"home":"$DIR","with_nix":true} in $DIR/laptop-home/x
    host: $worktree_list {"dir":"~/.prigh/worktrees"} in $DIR/laptop-home/sub
    --- a subagent with cwd ~/sub
    {"type":"response","id":"r","ok":true,"result":{}}
    host: $instructions {"home":"$DIR","with_nix":true} in $DIR/laptop-home/sub
    host: ls {} in $DIR/laptop-home/sub
    |}]
;;

let%expect_test "~ is the tool host's home: an older host that does not say" =
  scenario ~says_home:false;
  [%expect
    {|
    cwd: $DIR/work
    [{"id":"laptop","name":"laptop","cwd":"$DIR/work","session_id":"<id>","session_name":null}]
    --- /cd ~, /cd ~/sub, Tab after /cd ~/s
    host: $resolve_dir {"path":"~"} in $DIR/work
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home","created":false}}
    cwd: $DIR/laptop-home
    host: $resolve_dir {"path":"~/sub"} in $DIR/laptop-home
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/sub","created":false}}
    cwd: $DIR/laptop-home/sub
    host: $list_dirs {"prefix":"~/s"} in $DIR/laptop-home/sub
    {"type":"response","id":"r","ok":true,"result":["~/sub/"]}
    --- /host laptop with the directory ~/x
    host: $resolve_dir {"path":"~/x"} in $DIR/laptop-home/sub
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/x","created":false}}
    cwd: $DIR/laptop-home/x
    --- a switch into the worktree ~/sub, then back
    host: $resolve_dir {"path":"~/sub"} in $DIR/laptop-home/x
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/sub","created":false}}
    cwd: $DIR/laptop-home/sub
    host: $resolve_dir {"path":"$DIR/laptop-home/x"} in $DIR/laptop-home/sub
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/laptop-home/x","created":false}}
    cwd: $DIR/laptop-home/x
    --- the review of ~/sub
    host: $git_diff {"dir":"~/sub","default_base":true} in $DIR/laptop-home/x
    {"type":"response","id":"r","ok":false,"error":"not a git repository: $DIR/laptop-home/sub (name a worktree of the repository: /worktree lists them)"}
    --- the worktree tool with cwd ~/sub
    {"type":"response","id":"r","ok":true,"result":{}}
    host: $instructions {"home":"$DIR","with_nix":true} in $DIR/laptop-home/x
    host: $resolve_dir {"path":"~"} in $DIR/laptop-home/x
    host: $worktree_list {"dir":"~/.prigh/worktrees"} in $DIR/laptop-home/sub
    --- a subagent with cwd ~/sub
    {"type":"response","id":"r","ok":true,"result":{}}
    host: $instructions {"home":"$DIR","with_nix":true} in $DIR/laptop-home/sub
    host: ls {} in $DIR/laptop-home/sub
    |}]
;;
