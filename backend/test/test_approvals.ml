open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* An agent with the interaction tools and a background-capable subagent;
   [answer] answers confirmations and [reply] questions as they come, and
   everything worth seeing is printed in order. *)
let with_agent
      ?config
      ?(sub_replies = [])
      ?sub_script
      ?(answer =
        fun (_ : Agent.Pending_confirm.t) -> Agent.Confirm_answer.Allow)
      ?(reply = fun (_ : User_question.t list) -> User_question.Reply.Declined)
      replies
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Option.iter config ~f:(fun json -> write t ".prigh/config.json" json);
  let last_user = Queue.create () in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        List.rev r.messages
        |> List.find_map ~f:(function
          | Message.User u -> Some u.text
          | _ -> None)
        |> Option.iter ~f:(Queue.enqueue last_user))
      replies
  in
  let subagent =
    Tool_subagent.create
      ~provider:
        (Faux_provider.create
           (match sub_script with
            | Some script -> script ~dir:t.dir
            | None -> sub_replies))
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
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
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  let mode = ref "" in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Tool_confirm { call_id; agent_id; name; summary; rules }) ->
      printf
        "confirm %s%s %s: %s [%s]\n"
        (Option.value_map agent_id ~default:"" ~f:(fun a -> a ^ "/"))
        call_id
        name
        (mask t summary)
        (String.concat ~sep:" " rules);
      let answer =
        answer { Agent.Pending_confirm.call_id; agent_id; name; summary; rules }
      in
      printf
        "  -> %s\n"
        (Sexp.to_string [%sexp (answer : Agent.Confirm_answer.t)]);
      Or_error.ok_exn (Agent.respond_confirm ?agent_id agent ~call_id answer)
    | Loop (Ask_user { call_id; questions }) ->
      printf "ask_user %s:\n" call_id;
      List.iter questions ~f:(fun q -> print_s [%sexp (q : User_question.t)]);
      print_s
        [%sexp
          (List.map (Agent.pending_questions agent) ~f:(fun q -> q.call_id)
           : string list)];
      let r = reply questions in
      printf "  -> %s\n" (Sexp.to_string [%sexp (r : User_question.Reply.t)]);
      print_s [%sexp (Agent.answer_question agent ~call_id r : unit Or_error.t)];
      print_s
        [%sexp
          "again", (Agent.answer_question agent ~call_id r : unit Or_error.t)]
    | Loop (Tool_end { call; result }) ->
      printf
        "%s %s -> %s%s\n"
        call.name
        (String.prefix (mask t call.arguments) 60)
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | Loop (Subagent { event = Tool_end { call; result }; agent_id; _ }) ->
      printf
        "  [%s] %s -> %s%s\n"
        agent_id
        call.name
        (if result.is_error then "ERROR " else "")
        (mask t (String.strip result.text))
    | State_changed s ->
      let now =
        sprintf
          "mode=%s%s plan=%s rules=[%s]"
          (Approval_mode.to_string s.approval_mode)
          (if s.plan_mode then "+plan" else "")
          (match s.plan with
           | None -> "none"
           | Some p ->
             String.concat
               ~sep:","
               (List.map p.steps ~f:(fun s ->
                  sprintf "%s:%s" s.step (Plan.Status.to_string s.status))))
          (String.concat ~sep:" " s.session_rules)
      in
      if not (String.equal now !mode)
      then (
        mode := now;
        print_endline now)
    | _ -> ());
  (* The user messages the requests ended with, each once. *)
  let requests () =
    List.remove_consecutive_duplicates
      (Queue.to_list last_user)
      ~equal:String.equal
    |> List.iter ~f:(fun text ->
      printf "user message sent:\n%s\n" (mask t text));
    Queue.clear last_user
  in
  f t agent requests
;;

let%expect_test
    "auto-edit: edits run, bash asks; always allowing adds a rule the session \
     keeps"
  =
  with_agent
    ~answer:(fun _ -> Always)
    [ Reply.tool_call
        ~id:"c1"
        ~name:"write"
        ~arguments:{|{"path":"a.txt","content":"x\n"}|}
        ()
    ; Reply.tool_call
        ~id:"c2"
        ~name:"bash"
        ~arguments:{|{"command":"echo one && true"}|}
        ()
    ; Reply.tool_call
        ~id:"c3"
        ~name:"bash"
        ~arguments:{|{"command":"true; echo two"}|}
        ()
    ; Reply.tool_call
        ~id:"c4"
        ~name:"bash"
        ~arguments:{|{"command":"printf three"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun t agent _requests ->
  Agent.set_mode agent ~approval:Auto_edit;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  let path = Session.path (Agent.session agent) in
  print_s
    [%sexp
      (Session.approval (Or_error.ok_exn (Session.load path))
       : (string * bool) option)
    , (Session.allow_rules (Or_error.ok_exn (Session.load path)) : string list)];
  Eio.Switch.run (fun sw ->
    let reloaded =
      Agent.create
        ~env:t.env
        ~sw
        ~provider:(Faux_provider.create [])
        ~tools:Tools.all
        ~sessions_dir:(Filename.concat t.dir "sessions")
        ~home:t.dir
        ~session:(Or_error.ok_exn (Session.load path))
        ~cwd:t.dir
        ()
    in
    let s = Agent.state reloaded in
    print_s
      [%message
        "reloaded"
          ~mode:(Approval_mode.to_string s.approval_mode)
          ~rules:(s.session_rules : string list)]);
  [%expect
    {|
    mode=auto-edit plan=none rules=[]
    write {"path":"a.txt","content":"x\n"} -> wrote 1 line to $DIR/a.txt
    confirm c2 bash: echo one && true [bash(echo:*) bash(true:*)]
      -> Always
    mode=auto-edit plan=none rules=[bash(echo:*) bash(true:*)]
    bash {"command":"echo one && true"} -> one
    bash {"command":"true; echo two"} -> two
    confirm c4 bash: printf three [bash(printf:*)]
      -> Always
    mode=auto-edit plan=none rules=[bash(echo:*) bash(true:*) bash(printf:*)]
    bash {"command":"printf three"} -> three
    (((auto-edit false)) ("bash(echo:*)" "bash(true:*)" "bash(printf:*)"))
    (reloaded (mode auto-edit)
     (rules ("bash(echo:*)" "bash(true:*)" "bash(printf:*)")))
    |}]
;;

let%expect_test "ask: denying with a reason tells the model why" =
  with_agent
    ~answer:(fun _ -> Deny (Some "use make test instead"))
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"dune test"}|}
        ()
    ; Reply.tool_call ~id:"c2" ~name:"read" ~arguments:{|{"path":"nope"}|} ()
    ; Reply.text "done"
    ]
  @@ fun _t agent _requests ->
  Agent.set_mode agent ~approval:Ask;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=ask plan=none rules=[]
    confirm c1 bash: dune test [bash(dune test:*)]
      -> (Deny("use make test instead"))
    bash {"command":"dune test"} -> ERROR [denied by user] Their reason: use make test instead
    read {"path":"nope"} -> ERROR file not found: $DIR/nope
    |}]
;;

let%expect_test
    "config rules: deny refuses in every mode, ask asks even in auto, allow \
     lets through"
  =
  with_agent
    ~config:
      {|{"approval_mode": "auto",
         "permissions": {"deny": ["bash(rm:*)"], "ask": ["write(*.secret)"],
                         "allow": ["edit(notes/**)", "bash(echo hi)"]}}|}
    ~answer:(fun _ -> Allow)
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"cd /tmp && rm -rf x"}|}
        ()
    ; Reply.tool_call
        ~id:"c2"
        ~name:"write"
        ~arguments:{|{"path":"k.secret","content":"s"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun _t agent _requests ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  Agent.set_mode agent ~approval:Ask;
  print_s
    [%sexp
      (List.map
         (Agent.config agent).permissions.allow
         ~f:Approval_rule.to_string
       : string list)];
  [%expect
    {|
    mode=auto plan=none rules=[]
    bash {"command":"cd /tmp && rm -rf x"} -> ERROR [denied by the rule bash(rm:*) in permissions.deny; do not try to get around it: ask the user]
    confirm c2 write: k.secret []
      -> Allow
    write {"path":"k.secret","content":"s"} -> wrote 1 line to $DIR/k.secret
    mode=ask plan=none rules=[]
    ("edit(notes/**)" "bash(echo hi)")
    |}]
;;

let%expect_test "a subagent's calls are checked too, under its agent id" =
  with_agent
    ~answer:(fun _ -> Deny None)
    ~sub_replies:
      [ Reply.tool_call
          ~id:"s1"
          ~name:"bash"
          ~arguments:{|{"command":"rm -rf build"}|}
          ()
      ; Reply.text "could not"
      ]
    [ Reply.tool_call
        ~id:"c1"
        ~name:"subagent"
        ~arguments:{|{"task":"clean up"}|}
        ()
    ; Reply.text "started"
    ; Reply.text "noted"
    ]
  @@ fun _t agent _requests ->
  Agent.set_mode agent ~approval:Ask;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=ask plan=none rules=[]
    subagent {"task":"clean up"} -> started agent a1 (clean up); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    confirm a1/s1 bash: rm -rf build [bash(rm:*)]
      -> (Deny())
      [a1] bash -> ERROR [denied by user]
    |}]
;;

let%expect_test
    "rules resolve against the session's cwd, not a subagent's chosen one"
  =
  with_agent
    ~config:
      {|{"approval_mode": "auto", "permissions": {"deny": ["edit(secrets/**)"]}}|}
    ~sub_script:(fun ~dir ->
      [ Reply.tool_call
          ~id:"s1"
          ~name:"write"
          ~arguments:(sprintf {|{"path":"%s/secrets/k","content":"x"}|} dir)
          ()
      ; Reply.text "done"
      ; Reply.tool_call
          ~id:"s2"
          ~name:"bash"
          ~arguments:{|{"command":"pwd"}|}
          ()
      ; Reply.text "done"
      ])
    [ Reply.tool_call
        ~id:"c1"
        ~name:"subagent"
        ~arguments:{|{"task":"write","cwd":"/"}|}
        ()
    ; Reply.text "started"
    ; Reply.text "noted"
    ; Reply.tool_call
        ~id:"c2"
        ~name:"subagent"
        ~arguments:{|{"task":"pwd","cwd":"secrets"}|}
        ()
    ; Reply.text "started"
    ; Reply.text "noted"
    ]
  @@ fun t agent _requests ->
  Core_unix.mkdir_p (Filename.concat t.dir "secrets");
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "again");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=auto plan=none rules=[]
    subagent {"task":"write","cwd":"/"} -> started agent a1 (write); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [a1] write -> ERROR [denied by the rule edit(secrets/**) in permissions.deny; do not try to get around it: ask the user]
    subagent {"task":"pwd","cwd":"secrets"} -> started agent a2 (pwd); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
      [a2] bash -> $DIR/secrets
    |}]
;;

let%expect_test "switching to auto lets a pending confirmation through" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:
        (Faux_provider.create
           [ Reply.tool_call
               ~id:"c1"
               ~name:"bash"
               ~arguments:{|{"command":"echo ran"}|}
               ()
           ; Reply.text "done"
           ])
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.set_mode agent ~approval:Ask;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; _ }) ->
      print_s
        [%sexp
          (List.map (Agent.pending_confirms agent) ~f:(fun c -> c.call_id)
           : string list)];
      printf "confirm %s; switching to auto\n" call_id;
      Agent.set_mode agent ~approval:Auto
    | Loop (Tool_end { result; _ }) -> printf "result: %s" result.text
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_s
    [%sexp
      (List.map (Agent.pending_confirms agent) ~f:(fun c -> c.call_id)
       : string list)];
  [%expect
    {|
    (c1)
    confirm c1; switching to auto
    result: ran
    ()
    |}]
;;

let plan_args =
  {|{"explanation":"first pass","plan":[{"step":"read the code","status":"completed"},{"step":"change it","status":"in_progress"},{"step":"test","status":"pending"}]}|}
;;

let%expect_test
    "plan mode: edits refused, bash asks, the plan is approved and implemented"
  =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"edit"
        ~arguments:
          {|{"path":"a.txt","edits":[{"old_text":"a","new_text":"b"}]}|}
        ()
    ; Reply.tool_call ~id:"c2" ~name:"bash" ~arguments:{|{"command":"ls"}|} ()
    ; Reply.tool_call ~id:"c3" ~name:"update_plan" ~arguments:plan_args ()
    ; Reply.tool_call
        ~id:"c4"
        ~name:"exit_plan_mode"
        ~arguments:{|{"plan":"1. change a.txt\n2. test"}|}
        ()
    ; Reply.tool_call
        ~id:"c5"
        ~name:"write"
        ~arguments:{|{"path":"a.txt","content":"b\n"}|}
        ()
    ; Reply.text "done"
    ; Reply.text "second"
    ]
  @@ fun _t agent requests ->
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.prompt agent "add a feature");
  Agent.wait_idle agent;
  requests ();
  (* Told once: the next message carries no note. *)
  Or_error.ok_exn (Agent.prompt agent "thanks");
  Agent.wait_idle agent;
  requests ();
  [%expect
    {|
    mode=auto+plan plan=none rules=[]
    edit {"path":"a.txt","edits":[{"old_text":"a","new_text":"b"}]} -> ERROR [plan mode] Files cannot be changed while planning. Keep investigating with read-only tools, then call exit_plan_mode with the plan for the user to approve.
    confirm c2 bash: ls [bash(ls:*)]
      -> Allow
    bash {"command":"ls"} -> sessions
    mode=auto+plan plan=read the code:completed,change it:in_progress,test:pending rules=[]
    update_plan {"explanation":"first pass","plan":[{"step":"read the code", -> Plan updated (1 of 3 steps completed)
    confirm c4 exit_plan_mode: 1. change a.txt
    2. test []
      -> Allow
    mode=auto plan=read the code:completed,change it:in_progress,test:pending rules=[]
    exit_plan_mode {"plan":"1. change a.txt\n2. test"} -> The user approved the plan. Plan mode is off (approval mode: auto), so you can change files now: implement the plan.
    write {"path":"a.txt","content":"b\n"} -> wrote 1 line to $DIR/a.txt
    user message sent:
    [Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]

    add a feature
    user message sent:
    thanks
    |}]
;;

let%expect_test
    "plan mode: an unapproved plan keeps planning, with the feedback"
  =
  with_agent
    ~answer:(fun _ -> Deny (Some "also update the docs"))
    [ Reply.tool_call
        ~id:"c1"
        ~name:"exit_plan_mode"
        ~arguments:{|{"plan":"change a.txt"}|}
        ()
    ; Reply.text "revising"
    ]
  @@ fun _t agent _requests ->
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=auto+plan plan=none rules=[]
    confirm c1 exit_plan_mode: change a.txt []
      -> (Deny("also update the docs"))
    exit_plan_mode {"plan":"change a.txt"} -> ERROR [the user did not approve the plan; their feedback: also update the docs] Stay in plan mode: revise the plan and call exit_plan_mode again when it is ready.
    |}]
;;

let%expect_test
    "plan mode turned off while exit_plan_mode waits: not called approved"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:
        (Faux_provider.create
           [ Reply.tool_call
               ~id:"c1"
               ~name:"exit_plan_mode"
               ~arguments:{|{"plan":"change a.txt"}|}
               ()
           ; Reply.text "done"
           ])
      ~tools:(Tools.all @ Tools.interaction)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.set_mode agent ~plan:true;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_confirm { call_id; _ }) ->
      printf "confirm %s; turning plan mode off\n" call_id;
      Agent.set_mode agent ~plan:false
    | Loop (Tool_end { result; _ }) -> printf "result: %s\n" result.text
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    confirm c1; turning plan mode off
    result: The user turned plan mode off without answering about this plan (approval mode: auto): you can change files now. Implement the plan unless they said otherwise.
    |}]
;;

let%expect_test "exit_plan_mode outside plan mode does not ask" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"exit_plan_mode"
        ~arguments:{|{"plan":"x"}|}
        ()
    ; Reply.text "ok"
    ]
  @@ fun _t agent _requests ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=auto plan=none rules=[]
    exit_plan_mode {"plan":"x"} -> ERROR plan mode is not on, so there is no plan to approve: go ahead
    |}]
;;

let%expect_test "plan mode switched on during a run is told between turns" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let users = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        users
        := List.filter_map r.messages ~f:(function
             | Message.User u -> Some u.text
             | _ -> None))
      [ Reply.tool_call
          ~id:"c1"
          ~name:"bash"
          ~arguments:{|{"command":"true"}|}
          ()
      ; Reply.tool_call
          ~id:"c2"
          ~name:"write"
          ~arguments:{|{"path":"a","content":"x"}|}
          ()
      ; Reply.text "ok"
      ]
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ Tools.interaction)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_end { call; result }) ->
      printf "%s -> %s\n" call.name (String.prefix result.text 40);
      if String.equal call.name "bash" then Agent.set_mode agent ~plan:true
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  List.iter !users ~f:(fun u -> print_endline ("user: " ^ String.prefix u 40));
  [%expect
    {|
    bash ->
    write -> [plan mode] Files cannot be changed whil
    user: go
    user: [Plan mode is on: you are planning, not
    |}]
;;

let%expect_test "update_plan: the plan is session state, checked and restored" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"update_plan" ~arguments:plan_args ()
    ; Reply.tool_call
        ~id:"c2"
        ~name:"update_plan"
        ~arguments:
          {|{"plan":[{"step":"a","status":"in_progress"},{"step":"b","status":"in_progress"}]}|}
        ()
    ; Reply.tool_call
        ~id:"c3"
        ~name:"update_plan"
        ~arguments:{|{"plan":[{"step":"a","status":"done"}]}|}
        ()
    ; Reply.text "planned"
    ; Reply.tool_call ~id:"c4" ~name:"update_plan" ~arguments:{|{"plan":[]}|} ()
    ; Reply.text "cleared"
    ]
  @@ fun t agent _requests ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  let path = Session.path (Agent.session agent) in
  print_s
    [%sexp
      (Plan.latest (Session.messages (Or_error.ok_exn (Session.load path)))
       : Plan.t option)];
  let head = Option.value_exn (Session.head (Agent.session agent)) in
  Or_error.ok_exn (Agent.prompt agent "clear it");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.rewind agent ~to_:head);
  ignore t;
  [%expect
    {|
    mode=auto plan=none rules=[]
    mode=auto plan=read the code:completed,change it:in_progress,test:pending rules=[]
    update_plan {"explanation":"first pass","plan":[{"step":"read the code", -> Plan updated (1 of 3 steps completed)
    update_plan {"plan":[{"step":"a","status":"in_progress"},{"step":"b","st -> ERROR 2 steps are in_progress; at most one step can be in progress at a time
    update_plan {"plan":[{"step":"a","status":"done"}]} -> ERROR plan[0].status is "done"; use pending, in_progress or completed
    (((explanation ("first pass"))
      (steps
       (((step "read the code") (status Completed))
        ((step "change it") (status In_progress)) ((step test) (status Pending))))))
    mode=auto plan=none rules=[]
    update_plan {"plan":[]} -> Plan cleared
    mode=auto plan=read the code:completed,change it:in_progress,test:pending rules=[]
    |}]
;;

let questions =
  {|{"questions":[{"question":"Which database?","header":"Storage","options":[{"label":"Postgres","description":"what prod uses"},{"label":"SQLite"}]},{"question":"Which features?","options":[{"label":"auth"},{"label":"billing"}],"multiple":true},{"question":"Anything else?"}]}|}
;;

let%expect_test "ask_user: answered, first answer wins" =
  with_agent
    ~reply:(fun _ ->
      Answered [ [ "Postgres" ]; [ "auth"; "billing" ]; [ "keep it small" ] ])
    [ Reply.tool_call ~id:"q1" ~name:"ask_user" ~arguments:questions ()
    ; Reply.text "thanks"
    ]
  @@ fun _t agent _requests ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=auto plan=none rules=[]
    ask_user q1:
    ((question "Which database?") (header (Storage))
     (choices
      (((label Postgres) (description ("what prod uses")))
       ((label SQLite) (description ()))))
     (multiple false))
    ((question "Which features?") (header ())
     (choices
      (((label auth) (description ())) ((label billing) (description ()))))
     (multiple true))
    ((question "Anything else?") (header ()) (choices ()) (multiple false))
    (q1)
      -> (Answered((Postgres)(auth billing)("keep it small")))
    (Ok ())
    (again
     (Error
      "no pending question for tool call \"q1\": it was answered or withdrawn"))
    ask_user {"questions":[{"question":"Which database?","header":"Storag -> The user answered:
    Q: Which database?
    A: Postgres
    Q: Which features?
    A: auth; billing
    Q: Anything else?
    A: keep it small
    |}]
;;

let%expect_test "ask_user: wrong answers are refused, declining is an answer" =
  with_agent
    ~reply:(fun _ -> Declined)
    [ Reply.tool_call
        ~id:"q1"
        ~name:"ask_user"
        ~arguments:
          {|{"questions":[{"question":"Pick","options":[{"label":"a"},{"label":"b"}]}]}|}
        ()
    ; Reply.tool_call
        ~id:"q2"
        ~name:"ask_user"
        ~arguments:{|{"questions":[]}|}
        ()
    ; Reply.text "ok"
    ]
  @@ fun _t agent _requests ->
  Agent.subscribe agent ~f:(function
    | Loop (Ask_user { call_id; _ }) ->
      print_s
        [%sexp
          (Agent.answer_question agent ~call_id (Answered [ [ "a"; "b" ] ])
           : unit Or_error.t)];
      print_s
        [%sexp
          (Agent.answer_question agent ~call_id (Answered [ [ "a" ]; [] ])
           : unit Or_error.t)]
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=auto plan=none rules=[]
    ask_user q1:
    ((question Pick) (header ())
     (choices (((label a) (description ())) ((label b) (description ()))))
     (multiple false))
    (q1)
      -> Declined
    (Ok ())
    (again
     (Error
      "no pending question for tool call \"q1\": it was answered or withdrawn"))
    (Error
     "no pending question for tool call \"q1\": it was answered or withdrawn")
    (Error
     "no pending question for tool call \"q1\": it was answered or withdrawn")
    ask_user {"questions":[{"question":"Pick","options":[{"label":"a"},{" -> ERROR [the user declined to answer; carry on with your best judgement or ask differently]
    ask_user {"questions":[]} -> ERROR give at least one question in "questions"
    |}]
;;

let%expect_test "ask_user: aborting withdraws the question" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:
        (Faux_provider.create
           [ Reply.tool_call
               ~id:"q1"
               ~name:"ask_user"
               ~arguments:{|{"questions":[{"question":"Sure?"}]}|}
               ()
           ])
      ~tools:Tools.interaction
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  let asked, asked_r = Eio.Promise.create () in
  Agent.subscribe agent ~f:(function
    | Loop (Ask_user _) -> Eio.Promise.resolve asked_r ()
    | Loop (Tool_end { result; _ }) -> printf "result: %s\n" result.text
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Promise.await asked;
  print_s
    [%sexp
      (List.map (Agent.pending_questions agent) ~f:(fun q -> q.call_id)
       : string list)];
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  print_s
    [%sexp
      (List.map (Agent.pending_questions agent) ~f:(fun q -> q.call_id)
       : string list)
    , (Agent.answer_question agent ~call_id:"q1" Declined : unit Or_error.t)];
  [%expect
    {|
    (q1)
    result: [cancelled]
    (()
     (Error
      "no pending question for tool call \"q1\": it was answered or withdrawn"))
    |}]
;;

let%expect_test "subagents do not get the interaction tools" =
  (* Nor [goal_complete] or the job controls ([job_input] among them); the
     web tools they keep. *)
  print_s
    [%sexp
      (Tools.for_context
         ~parent:
           (Tools.all
            @ Tools.interaction
            @ Tool_jobs.tools
            @ [ Tool_goal.create ~complete:(fun ~summary:_ -> Ok ()) ])
         ~depth:1
         ()
       |> Or_error.ok_exn
       |> List.map ~f:Tool.name
       : string list)];
  [%expect {| (bash read write edit ls grep find web_fetch web_search) |}]
;;

let%expect_test
    "config: approval_mode, the confirm_tools it replaces, permissions problems"
  =
  with_sandbox
  @@ fun t ->
  let show json =
    write t ".prigh/config.json" json;
    print_s
      [%sexp
        (Or_error.map (Config.load ~home:t.dir) ~f:(fun c ->
           ( c.approval_mode
           , Approval.Rules.to_json c.permissions |> Jsonaf.to_string ))
         : (Approval_mode.t * string) Or_error.t)];
    List.iter (Config.problems ~home:t.dir) ~f:(fun p ->
      print_endline (mask t p))
  in
  show {|{"confirm_tools": true}|};
  show {|{"confirm_tools": false}|};
  show {|{"approval_mode": "auto-edit", "confirm_tools": true}|};
  show {|{"approval_mode": "careful"}|};
  show {|{"permissions": {"allow": ["bash(ls:*)", "bash(ls"], "maybe": []}}|};
  show {|{"permissions": ["bash"]}|};
  (* Saving keeps the rules as written and drops [confirm_tools]. *)
  write
    t
    ".prigh/config.json"
    {|{"confirm_tools": true, "permissions": {"allow": ["Bash(ls", "bash(ls:*)"]}}|};
  let config = Or_error.ok_exn (Config.load ~home:t.dir) in
  Or_error.ok_exn
    (Config.save ~home:t.dir { config with scoped_models = [ "x" ] });
  print_endline (read t ".prigh/config.json");
  [%expect
    {|
    (Ok (Ask "{\"allow\":[],\"ask\":[],\"deny\":[]}"))
    (Ok (Auto "{\"allow\":[],\"ask\":[],\"deny\":[]}"))
    (Ok (Auto_edit "{\"allow\":[],\"ask\":[],\"deny\":[]}"))
    (Error
     (config.approval_mode
      "unknown approval mode \"careful\"; the modes are ask, auto-edit, auto"))
    (config.approval_mode
     "unknown approval mode \"careful\"; the modes are ask, auto-edit, auto") (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    (Ok (Auto "{\"allow\":[\"bash(ls:*)\"],\"ask\":[],\"deny\":[]}"))
    permissions.allow: rule "bash(ls": the pattern must end with ")" (left out) in $DIR/.prigh/config.json
    permissions.maybe is not a list prigh knows (allow, ask, deny) in $DIR/.prigh/config.json
    (Error "config.permissions must be an object with allow, ask and deny lists")
    config.permissions must be an object with allow, ask and deny lists (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    {
      "permissions": {
        "allow": [
          "Bash(ls",
          "bash(ls:*)"
        ]
      },
      "scoped_models": [
        "x"
      ],
      "approval_mode": "ask",
      "default_model": null,
      "default_thinking": null,
      "fallback_models": [],
      "default_cwd": null,
      "max_budget_usd": null,
      "goal_max_continuations": 20,
      "resume_on_limit_reset": true,
      "archive_after_days": 7,
      "notify": "bell",
      "snippets": [],
      "auto_background_seconds": 30
    }
    |}]
;;

let%expect_test
    "ask: job_input asks with what it writes; web_fetch and job_status do not"
  =
  with_agent
    ~answer:(fun _ -> Allow)
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"cat","background":true,"stdin":true}|}
        ()
    ; Reply.tool_call
        ~id:"c2"
        ~name:"job_input"
        ~arguments:{|{"id":"j1","input":"hi\n","close":true,"wait_ms":10000}|}
        ()
    ; Reply.tool_call ~id:"c3" ~name:"job_status" ~arguments:{|{}|} ()
    ; Reply.tool_call
        ~id:"c4"
        ~name:"web_fetch"
        ~arguments:{|{"url":"not a url"}|}
        ()
    ; Reply.text "done"
    ; Reply.text "noted"
    ]
  @@ fun _t agent _requests ->
  Agent.set_mode agent ~approval:Ask;
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=ask plan=none rules=[]
    confirm c1 bash: cat [bash(cat:*)]
      -> Allow
    bash {"command":"cat","background":true,"stdin":true} -> started job j1: cat; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.
    confirm c2 job_input: j1: "hi\n" [job_input]
      -> Allow
    job_input {"id":"j1","input":"hi\n","close":true,"wait_ms":10000} -> sent 3 bytes to job j1 and closed its stdin; it then exited 0
    hi
    job_status {} -> j1  exited 0, delivered  0s  cat  (last: 3 B, hi)
    web_fetch {"url":"not a url"} -> ERROR "not a url" is not a URL: it must start with http:// or https://, e.g. https://not a url
    |}]
;;

let%expect_test
    "goal in plan mode: no continuations while planning; leaving plan mode \
     carries on"
  =
  with_agent
    [ Reply.text "here is my plan"
    ; Reply.tool_call
        ~id:"c1"
        ~name:"goal_complete"
        ~arguments:{|{"summary":"implemented"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun _t agent requests ->
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.set_goal agent "ship it");
  Agent.wait_idle agent;
  requests ();
  let goal () = print_s [%sexp ((Agent.state agent).goal : Goal.t option)] in
  goal ();
  Agent.set_mode agent ~plan:false;
  Agent.wait_idle agent;
  requests ();
  goal ();
  [%expect
    {|
    mode=auto+plan plan=none rules=[]
    user message sent:
    [Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]

    [prigh goal] Work on this goal until it is fully achieved:

    <goal>
    ship it
    </goal>

    You keep working across turns: when you end a turn while the goal is active, you are asked to continue. Once current evidence (files, command output, tests) proves every part of it is done, call goal_complete with a short summary of what was achieved.
    (((text "ship it") (status Active) (continuations 0)))
    mode=auto plan=none rules=[]
    goal_complete {"summary":"implemented"} -> goal marked complete
    user message sent:
    [Plan mode is off: you can change files and run commands again, as the user's approval mode allows.]

    [prigh goal: continuation 1 of 20] Continue working toward the goal:

    <goal>
    ship it
    </goal>

    Check the current state rather than relying on memory, then take the next concrete step toward the full goal; do not narrow it to what is easy. If every requirement is met and verified, call goal_complete with a short summary instead. If you are blocked on something only the user can resolve, say so plainly and end your turn.
    (((text "ship it") (status (Done (summary implemented))) (continuations 1)))
    |}]
;;

let%expect_test "rules match a write's own arguments, not the hidden scope" =
  with_agent
    ~config:
      {|{"approval_mode": "ask", "permissions": {"allow": ["edit(*.txt)"]}}|}
    [ Reply.tool_call
        ~id:"c1"
        ~name:"write"
        ~arguments:{|{"path":"a.txt","content":"one\n"}|}
        ()
    ; Reply.tool_call ~id:"c2" ~name:"read" ~arguments:{|{"path":"a.txt"}|} ()
    ; Reply.tool_call
        ~id:"c3"
        ~name:"write"
        ~arguments:{|{"path":"a.txt","content":"two\n"}|}
        ()
    ; Reply.tool_call
        ~id:"c4"
        ~name:"write"
        ~arguments:{|{"path":"b.md","content":"x"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun _t agent _requests ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  [%expect
    {|
    mode=ask plan=none rules=[]
    write {"path":"a.txt","content":"one\n"} -> wrote 1 line to $DIR/a.txt
    read {"path":"a.txt"} -> one
    write {"path":"a.txt","content":"two\n"} -> overwrote 1 line to $DIR/a.txt
    confirm c4 write: b.md [edit]
      -> Allow
    write {"path":"b.md","content":"x"} -> wrote 1 line to $DIR/b.md
    |}]
;;
