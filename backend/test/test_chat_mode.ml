open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Server = Fake_http_server

(* An agent whose requests are recorded by key ([main] or a subagent's
   task) and whose tool results are printed. Its tools run on the backend
   unless [backend_host] is false. *)
let port_re = Re.compile (Re.Perl.re {|127\.0\.0\.1:[0-9]+|})

let with_chat_agent
      ?(backend_host = true)
      ?(children = [])
      ?(before = fun _ -> ())
      replies
      f
  =
  with_sandbox
  @@ fun t ->
  write t "AGENTS.md" "Project rule: use tabs.";
  Eio.Switch.run
  @@ fun sw ->
  let server =
    Server.start ~sw ~env:t.env ~handler:(fun _ ->
      Server.Reply.simple
        200
        "<html><body><h1>Served</h1><p>by the test server.</p></body></html>"
        ~headers:[ "Content-Type", "text/html" ])
  in
  let replies = replies ~url:(Server.url server "/page") in
  let requests = Queue.create () in
  let agent_ref = ref None in
  let provider =
    Routed_provider.create
      ~before:(fun ~key:_ -> before (Option.value_exn !agent_ref))
      ~on_request:(fun ~key request -> Queue.enqueue requests (key, request))
      ~main:replies
      children
  in
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
         @ Tool_jobs.tools
         @ Tools.interaction)
      ~sessions_dir:(t.dir ^/ "sessions")
      ~home:t.dir
      ~backend_host
      ~cwd:t.dir
      ()
  in
  agent_ref := Some agent;
  Agent.subscribe agent ~f:(function
    | Tool_exec { name; _ } -> printf "host asked to run %s\n" name
    | Loop (Message_end (Tool_result r)) ->
      printf
        "%s: %s\n"
        r.tool_name
        (Re.replace_string port_re ~by:"127.0.0.1:PORT" (mask t r.text))
    | Loop (Message_end (Assistant a)) ->
      printf "assistant: %s\n" (Message.Assistant.text a)
    | _ -> ());
  f t agent requests
;;

let fixed replies ~url:_ = replies

let prompt agent text =
  match Agent.prompt agent text with
  | Ok () -> Agent.wait_idle agent
  | Error e -> printf "prompt refused: %s\n" (Error.to_string_hum e)
;;

let tool_names (request : Provider.Request.t) =
  List.map request.tools ~f:(fun (s : Tool_spec.t) -> s.name)
;;

let last_request requests ~key =
  Queue.to_list requests
  |> List.filter_map ~f:(fun (k, r) -> Option.some_if (String.equal k key) r)
  |> List.last_exn
;;

let show_mode agent =
  let s = Agent.state agent in
  print_s
    [%sexp
      { chat_mode : bool = s.chat_mode
      ; plan_mode : bool = s.plan_mode
      ; no_host : string option = s.no_host
      }]
;;

let%expect_test "the tools and the system prompt of a chat" =
  with_chat_agent
    (fixed [ Reply.text "coding"; Reply.text "chatting"; Reply.text "fresh" ])
  @@ fun t agent requests ->
  prompt agent "hi";
  let coding = last_request requests ~key:"main" in
  Agent.set_mode agent ~chat:true;
  show_mode agent;
  prompt agent "hi again";
  let chat = last_request requests ~key:"main" in
  print_s [%sexp (tool_names chat : string list)];
  print_s
    [%sexp
      (List.filter (tool_names coding) ~f:(fun n ->
         not (List.mem (tool_names chat) n ~equal:String.equal))
       : string list)];
  (* The session keeps the prompt it started with, so a new chat shows
     the chat's. *)
  Or_error.ok_exn (Agent.new_session agent |> Or_error.return);
  Agent.set_mode agent ~chat:true;
  prompt agent "fresh";
  let system = Option.value_exn (last_request requests ~key:"main").system in
  let system = mask t system in
  print_endline
    (String.concat ~sep:"\n" (List.take (String.split_lines system) 12));
  print_s
    [%sexp
      { mentions_cwd : bool = String.is_substring system ~substring:"$DIR"
      ; mentions_agents_md : bool =
          String.is_substring system ~substring:"use tabs"
      ; environment : string list =
          List.filter (String.split_lines system) ~f:(fun l ->
            String.is_prefix l ~prefix:"- Date"
            || String.is_prefix l ~prefix:"- Working"
            || String.is_prefix l ~prefix:"- OS"
            || String.is_prefix l ~prefix:"- This session")
          |> List.map ~f:(fun l -> String.prefix l 6)
      }];
  [%expect
    {|
    assistant: coding
    ((chat_mode true) (plan_mode false) (no_host ()))
    assistant: chatting
    (web_fetch subagent subagent_status subagent_cancel subagent_message
     subagent_continue update_plan ask_user render_ui)
    (bash read write edit ls grep find job_status job_output job_input job_kill)
    assistant: fresh
    You are prigh, an assistant chatting with the user.

    This is a chat (chat mode): you have no access to any machine. There is no project, working directory, shell or file system, so do not offer to run commands, read or change files or look at code you have not been shown. Answer from what you know, what the user tells you and the tools below. If something needs a machine, say so briefly: only the user can leave chat mode.

    Guidelines:
    - Be concise. Explain non-trivial points briefly. No filler.

    Background work: never block on it.
    - Subagents always run in the background: subagent returns at once with the agent id.
    - Each finished job or subagent reports automatically, in a message starting with "[job <id> exited <code>]" (or killed, failed) or "[subagent <id> finished]" (or failed, or out of turns: subagent_continue gives it more turns), sent by prigh, not typed by the user. You are woken by it even after you end your turn.
    - So while they run, keep working on whatever does not depend on them, or end your turn. Never sleep or poll to pass the time.

    ((mentions_cwd false) (mentions_agents_md false)
     (environment ("- Date" "- This")))
    |}]
;;

let%expect_test "host tools are refused in a chat; a subagent is a chat too" =
  with_chat_agent
    ~children:
      [ ( "look it up"
        , [ Reply.tool_call ~id:"s1" ~name:"ls" ~arguments:"{}" ()
          ; Reply.text "sub done"
          ] )
      ]
    (fixed
       [ Reply.tool_call
           ~id:"c1"
           ~name:"bash"
           ~arguments:{|{"command":"pwd"}|}
           ()
       ; Reply.tool_call
           ~id:"c2"
           ~name:"read"
           ~arguments:{|{"path":"AGENTS.md"}|}
           ()
       ; Reply.tool_call
           ~id:"c3"
           ~name:"subagent"
           ~arguments:{|{"task":"x","worktree":"auto"}|}
           ()
       ; Reply.tool_call
           ~id:"c4"
           ~name:"subagent"
           ~arguments:{|{"task":"look it up","background":false}|}
           ()
       ; Reply.text "done"
       ; Reply.text "noted"
       ])
  @@ fun t agent requests ->
  Agent.set_mode agent ~chat:true;
  prompt agent "go";
  let sub = last_request requests ~key:"look it up" in
  print_s [%sexp (tool_names sub : string list)];
  let system = mask t (Option.value_exn sub.system) in
  print_s
    [%sexp
      { says_chat : bool =
          String.is_substring
            system
            ~substring:"you have no access to any machine"
      ; mentions_cwd : bool = String.is_substring system ~substring:"$DIR"
      }];
  [%expect
    {|
    assistant:
    bash: unknown tool "bash"
    assistant:
    read: unknown tool "read"
    assistant:
    subagent: a subagent cannot have a worktree in chat mode, which has no machine: start it without one
    assistant:
    subagent: started agent a1 (look it up); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    assistant: done
    assistant: noted
    (web_fetch subagent subagent_continue)
    ((says_chat true) (mentions_cwd false))
    |}]
;;

let%expect_test
    "switching chat mode tells the model; it is kept with the session"
  =
  with_chat_agent
    (fixed [ Reply.text "one"; Reply.text "two"; Reply.text "three" ])
  @@ fun t agent _ ->
  let last_user () =
    List.rev (Agent.messages agent)
    |> List.find_map ~f:(function
      | Message.User u -> Some u.text
      | _ -> None)
    |> Option.value_exn
    |> mask t
  in
  prompt agent "start";
  Agent.set_mode agent ~plan:true;
  Agent.set_mode agent ~chat:true;
  show_mode agent;
  prompt agent "chat";
  print_endline (last_user ());
  (* Reloaded, the session is still a chat. *)
  let path = Session.path (Agent.session agent) in
  Or_error.ok_exn (Agent.switch_session agent ~path);
  show_mode agent;
  print_s
    [%sexp
      (Session.approval (Or_error.ok_exn (Session.load path))
       : Session.Mode.t option)];
  (* Leaving it returns to plan mode, as before. *)
  Agent.set_mode agent ~chat:false;
  show_mode agent;
  prompt agent "back";
  print_endline (last_user ());
  [%expect
    {|
    assistant: one
    ((chat_mode true) (plan_mode true) (no_host ()))
    assistant: two
    [Chat mode is on: this is now a chat, with no access to any machine. The tool host, the working directory, the shell, the file and project tools and the project instructions are gone: only the tools you are now offered remain, and calls to others are refused. Do not offer to run commands or touch files. Only the user leaves chat mode.]

    chat
    ((chat_mode true) (plan_mode true) (no_host ()))
    (((approval auto) (plan true) (chat true)))
    ((chat_mode false) (plan_mode true) (no_host ()))
    assistant: three
    [Chat mode is off: you work on a machine again. The tool host is backend and the working directory $DIR. Your tools are back. Project instructions (AGENTS.md/CLAUDE.md) may apply there: read them if the task involves the project.]
    [Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one) and ask the user about anything unclear with ask_user. Keep the steps in update_plan. When the plan is ready, present it as your reply (concise: the steps and the files they touch), then stop and wait. Do not implement any of it: do not change files, worktrees or pull requests (they are refused) and do not run commands that change anything. Only the user turns plan mode off, when they want the plan implemented: do not try to leave plan mode or ask them to.]

    back
    |}]
;;

let%expect_test "no tool host: other modes refuse, a chat runs" =
  with_chat_agent
    ~backend_host:false
    (fixed [ Reply.text "hello from the chat" ])
  @@ fun _ agent requests ->
  show_mode agent;
  prompt agent "hi";
  Agent.chat_without_host agent;
  show_mode agent;
  prompt agent "hi";
  print_s [%sexp (Queue.length requests : int)];
  (* Not on a session that already chose its mode or has messages. *)
  Agent.set_mode agent ~chat:false;
  Agent.chat_without_host agent;
  show_mode agent;
  [%expect
    {|
    ((chat_mode false) (plan_mode false)
     (no_host
      ("no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)")))
    prompt refused: no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)
    ((chat_mode true) (plan_mode false)
     (no_host
      ("no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)")))
    assistant: hello from the chat
    1
    ((chat_mode false) (plan_mode false)
     (no_host
      ("no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)")))
    |}]
;;

let fetch_replies ~url =
  [ Reply.tool_call
      ~id:"c1"
      ~name:"web_fetch"
      ~arguments:(sprintf {|{"url":%S}|} url)
      ()
  ; Reply.text "fetched"
  ]
;;

let%expect_test "web_fetch in a chat runs in the backend, not on the tool host" =
  with_chat_agent fetch_replies
  @@ fun t agent _ ->
  Agent.set_hosts
    agent
    [ { id = "h"
      ; name = "laptop"
      ; cwd = t.dir
      ; session_id = None
      ; session_name = None
      ; home = Some t.dir
      ; exec_input = false
      ; git_location = false
      ; computer = None
      }
    ];
  ignore (Or_error.ok_exn (Agent.set_active_host agent "h" ~cwd:None));
  Agent.set_mode agent ~chat:true;
  prompt agent "fetch it";
  [%expect
    {|
    assistant:
    web_fetch: http://127.0.0.1:PORT/page (HTTP 200, text/html, 67 B)

    # Served

    by the test server.
    assistant: fetched
    |}]
;;

let%expect_test "web_fetch in a chat without the backend host: not its network" =
  with_chat_agent ~backend_host:false fetch_replies
  @@ fun _ agent _ ->
  Agent.set_mode agent ~chat:true;
  prompt agent "fetch it";
  [%expect
    {|
    assistant:
    web_fetch: http://127.0.0.1:PORT/page is on prigh's server's own network (127.0.0.1), which this conversation cannot reach: fetch a public URL
    assistant: fetched
    |}]
;;

let%expect_test "a host tool call from before the switch is refused" =
  let requests_seen = ref 0 in
  with_chat_agent
    ~before:(fun agent ->
      incr requests_seen;
      (* Switched while the model writes its second reply: the run's tool
         list is the coding one. *)
      if !requests_seen = 2 then Agent.set_mode agent ~chat:true)
    (fixed
       [ Reply.tool_call ~id:"c1" ~name:"ls" ~arguments:"{}" ()
       ; Reply.tool_call
           ~id:"c2"
           ~name:"bash"
           ~arguments:{|{"command":"pwd"}|}
           ()
       ; Reply.text "ok"
       ])
  @@ fun _ agent requests ->
  prompt agent "go";
  let last = last_request requests ~key:"main" in
  List.iter last.messages ~f:(function
    | Message.User u -> print_endline u.text
    | _ -> ());
  [%expect
    {|
    assistant:
    ls: AGENTS.md
    sessions/

    assistant:
    bash: bash is not available: this is a chat (chat mode), with no access to any machine. Answer without it; only the user can leave chat mode.
    assistant: ok
    [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).]

    go
    [Chat mode is on: this is now a chat, with no access to any machine. The tool host, the working directory, the shell, the file and project tools and the project instructions are gone: only the tools you are now offered remain, and calls to others are refused. Do not offer to run commands or touch files. Only the user leaves chat mode.]
    |}]
;;
