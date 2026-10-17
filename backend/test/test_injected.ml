open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let show text =
  match Injected.infer text with
  | None -> print_endline "plain"
  | Some origin ->
    print_s [%sexp (origin : Injected.t)];
    print_endline (Jsonaf.to_string (Injected.to_json origin ~text))
;;

let%expect_test "environment notes in front of a message, also several" =
  let host = System_prompt.host_changed_note ~host:"laptop" in
  let cwd = System_prompt.cwd_changed_note ~cwd:"/home/me/proj" in
  (* The note sessions from before the tools note have. *)
  let editor =
    "[Tools: edit replaces apply_patch for editing files; apply_patch is no \
     longer available.]"
  in
  let tools =
    System_prompt.tools_changed_note
      ~added:[ "edit" ]
      ~removed:[ "apply_patch"; "web_search" ]
  in
  show (host ^ "\n\n" ^ "fix the build");
  [%expect
    {|
    ((notes
      (((kind (Host (name laptop)))
        (text
         "[Environment: the tool host is now laptop, so tools run there and its filesystem may differ from the one described above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]")))))
    {"body":"fix the build","notes":[{"kind":"host","title":"Tools now run on laptop","text":"[Environment: the tool host is now laptop, so tools run there and its filesystem may differ from the one described above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]"}]}
    |}];
  show (String.concat ~sep:"\n" [ host; cwd; editor ] ^ "\n\nand now?\n\nmore");
  [%expect
    {|
    ((notes
      (((kind (Host (name laptop)))
        (text
         "[Environment: the tool host is now laptop, so tools run there and its filesystem may differ from the one described above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]"))
       ((kind (Cwd (dir /home/me/proj)))
        (text
         "[Environment: the working directory is now /home/me/proj. Project instructions for it may differ from the ones above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]"))
       ((kind (Editor (before apply_patch) (after edit)))
        (text
         "[Tools: edit replaces apply_patch for editing files; apply_patch is no longer available.]")))))
    {"body":"and now?\n\nmore","notes":[{"kind":"host","title":"Tools now run on laptop","text":"[Environment: the tool host is now laptop, so tools run there and its filesystem may differ from the one described above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]"},{"kind":"cwd","title":"Working directory is now /home/me/proj","text":"[Environment: the working directory is now /home/me/proj. Project instructions for it may differ from the ones above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]"},{"kind":"tools","title":"File edits now use edit instead of apply_patch","text":"[Tools: edit replaces apply_patch for editing files; apply_patch is no longer available.]"}]}
    |}];
  show (tools ^ "\n\nnext");
  show
    (System_prompt.tools_changed_note ~added:[] ~removed:[ "web_search" ]
     ^ "\n\nnext");
  show
    (System_prompt.tools_changed_note ~added:[ "web_search" ] ~removed:[]
     ^ "\n\nnext");
  [%expect
    {|
    ((notes
      (((kind (Tools (added (edit)) (removed (apply_patch web_search))))
        (text
         "[Tools: the tools changed with the model. Now available: edit. No longer available: apply_patch, web_search.]")))))
    {"body":"next","notes":[{"kind":"tools","title":"Tools changed: edit added; apply_patch, web_search removed","text":"[Tools: the tools changed with the model. Now available: edit. No longer available: apply_patch, web_search.]"}]}
    ((notes
      (((kind (Tools (added ()) (removed (web_search))))
        (text
         "[Tools: the tools changed with the model. No longer available: web_search.]")))))
    {"body":"next","notes":[{"kind":"tools","title":"Tools changed: web_search removed","text":"[Tools: the tools changed with the model. No longer available: web_search.]"}]}
    ((notes
      (((kind (Tools (added (web_search)) (removed ())))
        (text
         "[Tools: the tools changed with the model. Now available: web_search.]")))))
    {"body":"next","notes":[{"kind":"tools","title":"Tools changed: web_search added","text":"[Tools: the tools changed with the model. Now available: web_search.]"}]}
    |}];
  (* What a user writes that only looks like it stays theirs. *)
  show "[Environment: mine]\n\ntext";
  show (host ^ "\nnot a note\n\ntext");
  show (host ^ "\ntext without a blank line");
  show "plain words";
  [%expect
    {|
    plain
    plain
    plain
    plain
    |}]
;;

let%expect_test "attached files behind a message" =
  show
    "compare @a.ml @b.ml\n\n\
     <file path=\"a.ml\">\n\
     let x = 1 (* </file> *)\n\
     </file>\n\n\
     <file path=\"b.ml\" error=\"no such file\"/>";
  [%expect
    {|
    ((attachments (((path a.ml) (length 50)) ((path b.ml) (length 40)))))
    {"body":"compare @a.ml @b.ml","attachments":[{"path":"a.ml"},{"path":"b.ml"}]}
    |}];
  show "see\n\n<file path=\"a b\\\"c\">\nx\n</file>";
  [%expect
    {|
    ((attachments (((path "a b\"c") (length 30)))))
    {"body":"see","attachments":[{"path":"a b\"c"}]}
    |}];
  (* Something after the blocks: not attachments. *)
  show "see\n\n<file path=\"a\">\nx\n</file>\nmore";
  [%expect {| plain |}]
;;

let%expect_test "messages of the harness's" =
  let goal =
    { Goal.text = "all tests pass"; status = Active; continuations = 3 }
  in
  List.iter
    [ "[subagent a1 finished] fix the parser\n\
       report\n\n\
       [job j2 exited 1] make test\n\
       FAIL"
    ; "[prigh: openai/gpt-5.5 cannot continue (HTTP 429: usage limit \
       (reached), so), so anthropic/claude-opus-4-6 takes over this \
       conversation from here. Carry on with the task where it left off.]"
    ; Goal.start_message goal
    ; Goal.continuation_message goal ~max_continuations:20
    ; "[prigh: the usage limit has reset. Carry on with the interrupted task \
       where it left off.]"
    ]
    ~f:show;
  (match Session.summary_message "they talked" with
   | User { text; origin; _ } ->
     show text;
     print_s [%sexp (origin : Injected.t option)]
   | _ -> assert false);
  [%expect
    {|
    ((source (Delivery (reports ("subagent a1 finished" "job j2 exited 1")))))
    {"source":{"kind":"delivery","title":"Subagent a1 finished, job j2 exited 1"}}
    ((source
      (Handover (from openai/gpt-5.5) (to_ anthropic/claude-opus-4-6)
       (error "HTTP 429: usage limit (reached), so"))))
    {"source":{"kind":"handover","title":"Handed over from openai/gpt-5.5 to anthropic/claude-opus-4-6"}}
    ((source (Goal_start (goal "all tests pass"))))
    {"source":{"kind":"goal","title":"Goal: all tests pass"}}
    ((source (Goal_continuation (continuation 3) (max 20))))
    {"source":{"kind":"goal_continuation","title":"Goal: continuing (3/20)"}}
    ((source Resume))
    {"source":{"kind":"resume","title":"Usage limit reset: carrying on"}}
    ((source Summary))
    {"source":{"kind":"summary","title":"Summary of the earlier conversation"}}
    (((source Summary)))
    |}]
;;

let%expect_test "reminders: recorded, never inferred" =
  List.iter
    [ Injected.Source.Reminder { name = "disk"; repeat = None; max = None }
    ; Reminder { name = "keep-going"; repeat = Some 1; max = Some 5 }
    ; Reminder
        { name = "Check disk usage.\nThen clean up."
        ; repeat = None
        ; max = None
        }
    ]
    ~f:(fun source ->
      let origin = { Injected.empty with source = Some source } in
      print_s [%sexp (origin : Injected.t)];
      print_endline (Jsonaf.to_string (Injected.to_json origin ~text:"x"));
      print_s
        [%sexp
          (Injected.t_of_jsonaf (Injected.jsonaf_of_t origin)
           |> Injected.equal origin
           : bool)]);
  print_s
    [%sexp
      (Injected.infer "[prigh reminder: disk, every 40 turns] Check disk."
       : Injected.t option)];
  [%expect
    {|
    ((source (Reminder (name disk))))
    {"source":{"kind":"reminder","title":"Reminder: disk"}}
    true
    ((source (Reminder (name keep-going) (repeat 1) (max 5))))
    {"source":{"kind":"reminder","title":"Reminder: keep-going (1/5)"}}
    true
    ((source (Reminder (name  "Check disk usage.\
                             \nThen clean up."))))
    {"source":{"kind":"reminder","title":"Reminder: Check disk usage. …"}}
    true
    ()
    |}]
;;

(* ---- the agent records what it adds ----------------------------------- *)

let with_agent ?on_request replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider = Faux_provider.create ?on_request replies in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  f t agent
;;

(* Each user message as the frontends get it, and whether the legacy
   recognition of its text agrees with what was recorded. *)
let print_users t agent =
  List.iter
    (Session.messages (Agent.session agent))
    ~f:(fun (m : Message.t) ->
      match m with
      | User u ->
        print_endline (mask t (Jsonaf.to_string (Rpc_json.message m)));
        if not ([%equal: Injected.t option] u.origin (Injected.infer u.text))
        then print_endline "  (recognised differently without the record)"
      | Assistant _ | Tool_result _ -> ())
;;

let%expect_test
    "notes and attachments are recorded with the message; the model gets the \
     same text as ever"
  =
  let sent = Queue.create () in
  with_agent
    ~on_request:(fun request ->
      List.iter request.messages ~f:(function
        | Message.User u -> Queue.enqueue sent u.text
        | _ -> ()))
    [ Reply.text "one"; Reply.text "two" ]
  @@ fun t agent ->
  write t "sub/notes.txt" "some notes\n";
  Or_error.ok_exn (Agent.prompt agent "first");
  Agent.wait_idle agent;
  ignore
    (Or_error.ok_exn (Agent.set_cwd agent ~path:(Filename.concat t.dir "sub"))
     : Dir_change.Outcome.t);
  Or_error.ok_exn
    (Agent.prompt agent "read @notes.txt please" ~attachments:[ "notes.txt" ]);
  Agent.wait_idle agent;
  printf "model got: %S\n" (mask t (Queue.last_exn sent));
  print_users t agent;
  (* The session file keeps it. *)
  let reloaded =
    Or_error.ok_exn (Session.load (Agent.state agent).session_path)
  in
  print_s
    [%sexp
      (List.equal
         Message.equal
         (Session.messages reloaded)
         (Session.messages (Agent.session agent))
       : bool)];
  [%expect
    {|
    model got: "[Environment: the working directory is now $DIR/sub. Project instructions for it may differ from the ones above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]\n\nread @notes.txt please\n\n<file path=\"notes.txt\">\nsome notes\n</file>"
    {"role":"user","text":"first"}
    {"role":"user","text":"[Environment: the working directory is now $DIR/sub. Project instructions for it may differ from the ones above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]\n\nread @notes.txt please\n\n<file path=\"notes.txt\">\nsome notes\n</file>","origin":{"body":"read @notes.txt please","notes":[{"kind":"cwd","title":"Working directory is now $DIR/sub","text":"[Environment: the working directory is now $DIR/sub. Project instructions for it may differ from the ones above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]"}],"attachments":[{"path":"notes.txt"}]}}
    true
    |}]
;;

let%expect_test "goals, shell output and a legacy session line" =
  with_agent [ Reply.text "working"; Reply.text "done" ]
  @@ fun t agent ->
  Or_error.ok_exn (Agent.set_goal agent "write the docs");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.clear_goal agent);
  ignore
    (Or_error.ok_exn (Agent.shell agent ~command:"echo hi" ~add_to_context:true)
     : Tool_result.t);
  print_users t agent;
  (* A line written before origins existed. *)
  let legacy =
    Jsonaf.of_string
      {|{"text":"[Environment: the tool host is now old, so tools run there and its filesystem may differ from the one described above. x]\n\nhello"}|}
  in
  let m = Message.User ([%of_jsonaf: Message.User.t] legacy) in
  print_endline (Jsonaf.to_string (Rpc_json.message m));
  print_endline
    (Jsonaf.to_string ([%jsonaf_of: Message.t] (Message.user "unchanged")));
  [%expect
    {|
    {"role":"user","text":"[prigh goal] Work on this goal until it is fully achieved:\n\n<goal>\nwrite the docs\n</goal>\n\nYou keep working across turns: when you end a turn while the goal is active, you are asked to continue. Once current evidence (files, command output, tests) proves every part of it is done, call goal_complete with a short summary of what was achieved.","origin":{"source":{"kind":"goal","title":"Goal: write the docs"}}}
    {"role":"user","text":"[prigh goal: continuation 1 of 20] Continue working toward the goal:\n\n<goal>\nwrite the docs\n</goal>\n\nCheck the current state rather than relying on memory, then take the next concrete step toward the full goal; do not narrow it to what is easy. If every requirement is met and verified, call goal_complete with a short summary instead. If you are blocked on something only the user can resolve, say so plainly and end your turn.","origin":{"source":{"kind":"goal_continuation","title":"Goal: continuing (1/20)"}}}
    {"role":"user","text":"[prigh goal: continuation 2 of 20] Continue working toward the goal:\n\n<goal>\nwrite the docs\n</goal>\n\nCheck the current state rather than relying on memory, then take the next concrete step toward the full goal; do not narrow it to what is easy. If every requirement is met and verified, call goal_complete with a short summary instead. If you are blocked on something only the user can resolve, say so plainly and end your turn.","origin":{"source":{"kind":"goal_continuation","title":"Goal: continuing (2/20)"}}}
    {"role":"user","text":"$ echo hi\nhi\n","origin":{"source":{"kind":"shell","title":"Output of $ echo hi added to the context"}}}
      (recognised differently without the record)
    {"role":"user","text":"[Environment: the tool host is now old, so tools run there and its filesystem may differ from the one described above. x]\n\nhello","origin":{"body":"hello","notes":[{"kind":"host","title":"Tools now run on old","text":"[Environment: the tool host is now old, so tools run there and its filesystem may differ from the one described above. x]"}]}}
    ["User",{"text":"unchanged"}]
    |}]
;;

let%expect_test
    "hand-overs recorded before origins: the error may contain anything"
  =
  List.iter
    [ "[prigh: openai-codex/gpt-6-sol cannot continue (HTTP 429: The usage \
       limit has been reached (usage limit reached)), so \
       anthropic/claude-opus-5-5 takes over this conversation from here. Carry \
       on with the task where it left off.]"
    ; "[prigh: a/b cannot continue (no, so what), so c/d takes over this \
       conversation from here. Carry on with the task where it left off.]"
    ; "[prigh: a/x cannot continue (HTTP 429: {\"error\": \"usage limit \
       reached (plan: pro), try later\"}), so b/y takes over this conversation \
       from here. Carry on with the task where it left off.]"
    ; "[prigh: a/x cannot continue (quota (daily), so wait; b/y takes over \
       this conversation), so b/y takes over this conversation from here. \
       Carry on with the task where it left off.]"
    ; "[prigh: something else]"
    ; "[prigh: a/b cannot continue (x), so c/d takes over this conversation \
       from here."
    ; "[prigh: a/b cannot continue (x) and c/d takes over this conversation.]"
    ; "please: [prigh: a/b cannot continue (x), so c/d takes over this \
       conversation.]"
    ; "[prigh: a/b cannot continue (x), so c d takes over this conversation.]"
    ; "[prigh: a b cannot continue (x), so c/d takes over this conversation.]"
    ; "[prigh:  cannot continue (x), so c/d takes over this conversation.]"
    ]
    ~f:(fun text ->
      match Injected.infer text with
      | None -> print_endline "none"
      | Some { source = Some source; _ } ->
        print_s [%sexp (source : Injected.Source.t)];
        print_endline (Injected.Source.title source)
      | Some other -> print_s [%sexp (other : Injected.t)]);
  [%expect
    {|
    (Handover (from openai-codex/gpt-6-sol) (to_ anthropic/claude-opus-5-5)
     (error "HTTP 429: The usage limit has been reached (usage limit reached)"))
    Handed over from openai-codex/gpt-6-sol to anthropic/claude-opus-5-5
    (Handover (from a/b) (to_ c/d) (error "no, so what"))
    Handed over from a/b to c/d
    (Handover (from a/x) (to_ b/y)
     (error
      "HTTP 429: {\"error\": \"usage limit reached (plan: pro), try later\"}"))
    Handed over from a/x to b/y
    (Handover (from a/x) (to_ b/y)
     (error "quota (daily), so wait; b/y takes over this conversation"))
    Handed over from a/x to b/y
    none
    none
    none
    none
    none
    none
    none
    |}]
;;

let%expect_test
    "plan mode notes: with a message, before a goal's, and between turns"
  =
  with_agent
    [ Reply.text "looked"
    ; Reply.tool_call
        ~id:"g1"
        ~name:"goal_complete"
        ~arguments:{|{"summary":"shipped"}|}
        ()
    ; Reply.text "done"
    ; Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"true"}|} ()
    ; Reply.text "ok"
    ]
  @@ fun t agent ->
  Agent.set_mode agent ~plan:true;
  Or_error.ok_exn (Agent.prompt agent "look around");
  Agent.wait_idle agent;
  Agent.set_mode agent ~plan:false;
  Or_error.ok_exn (Agent.set_goal agent "ship it");
  Agent.wait_idle agent;
  Agent.subscribe agent ~f:(function
    | Loop (Tool_end { call; _ }) when String.equal call.name "bash" ->
      Agent.set_mode agent ~plan:true
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_users t agent;
  [%expect
    {|
    {"role":"user","text":"[Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]\n\nlook around","origin":{"body":"look around","notes":[{"kind":"plan_mode","title":"Plan mode on: the model was told to plan, not change files","text":"[Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]"}]}}
    {"role":"user","text":"[Plan mode is off: you can change files and run commands again, as the user's approval mode allows.]\n\n[prigh goal] Work on this goal until it is fully achieved:\n\n<goal>\nship it\n</goal>\n\nYou keep working across turns: when you end a turn while the goal is active, you are asked to continue. Once current evidence (files, command output, tests) proves every part of it is done, call goal_complete with a short summary of what was achieved.","origin":{"source":{"kind":"goal","title":"Goal: ship it"},"notes":[{"kind":"plan_mode","title":"Plan mode off: the model was told it can change files","text":"[Plan mode is off: you can change files and run commands again, as the user's approval mode allows.]"}]}}
    {"role":"user","text":"go"}
    {"role":"user","text":"[Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]","origin":{"body":"","notes":[{"kind":"plan_mode","title":"Plan mode on: the model was told to plan, not change files","text":"[Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]"}]}}
    |}]
;;
