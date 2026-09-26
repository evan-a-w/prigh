open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let with_agent ?tools ?on_request ?auto_describe replies f =
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
      ~tools:(Option.value tools ~default:Tools.all)
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?auto_describe
      ~cwd:t.dir
      ()
  in
  let log = Queue.create () in
  Agent.subscribe agent ~f:(fun e ->
    let line =
      match e with
      | Loop (Message_end m) ->
        Some
          (match m with
           | User u -> "user: " ^ u.text
           | Assistant a ->
             "assistant: " ^ String.prefix (Message.Assistant.text a) 40
           | Tool_result r -> "tool_result: " ^ String.strip r.text)
      | Loop _ -> None
      | State_changed s ->
        Some (sprintf "state: running=%b messages=%d" s.running s.message_count)
      | Compacted { summary } -> Some ("compacted: " ^ summary)
      | Config_changed _ -> None
      | Notice n -> Some ("notice: " ^ n)
      | Queue_update { steer; follow_up } ->
        Some (sprintf "queue: steer=%d follow_up=%d" steer follow_up)
      | Tool_exec { name; _ } -> Some ("tool_exec: " ^ name)
      | Tool_exec_cancel _ -> Some "tool_exec_cancel"
    in
    Option.iter line ~f:(fun l -> Queue.enqueue log (mask t l)));
  let dump () =
    Queue.iter log ~f:print_endline;
    Queue.clear log
  in
  f t agent dump
;;

let%expect_test
    "prompt runs to completion, persists, and rejects concurrent prompts"
  =
  with_agent [ Reply.text "hi there" ]
  @@ fun _t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "hello");
  print_s
    [%sexp
      (Agent.is_running agent : bool)
    , (Agent.prompt agent "again" : unit Or_error.t)];
  Agent.wait_idle agent;
  let state = Agent.state agent in
  print_s
    [%sexp
      { running = (state.running : bool)
      ; messages = (state.message_count : int)
      ; usage = (state.usage : Usage.t)
      }];
  let reloaded = Or_error.ok_exn (Session.load state.session_path) in
  print_s [%sexp (List.length (Session.messages reloaded) : int)];
  dump ();
  [%expect
    {|
    (true (Error "a run is already in progress; use steer or follow_up"))
    ((running false) (messages 2) (usage ((input 10) (output 5) (cache_read 0))))
    2
    state: running=true messages=0
    user: hello
    assistant: hi there
    state: running=false messages=2
    |}]
;;

let%expect_test
    "follow_up while running queues a second run; steer while idle just runs"
  =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.2"}|}
        ()
    ; Reply.text "first done"
    ; Reply.text "second done"
    ; Reply.text "third done"
    ]
  @@ fun _t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.follow_up agent "two";
  Agent.wait_idle agent;
  Agent.steer agent "three";
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    state: running=true messages=0
    user: one
    queue: steer=0 follow_up=1
    assistant:
    tool_result:
    assistant: first done
    state: running=false messages=4
    queue: steer=0 follow_up=0
    state: running=true messages=4
    user: two
    assistant: second done
    state: running=false messages=6
    state: running=true messages=6
    user: three
    assistant: third done
    state: running=false messages=8
    |}]
;;

let%expect_test "steer while running is injected after the tool results" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.2; echo hi"}|}
        ()
    ; Reply.text "noted"
    ]
  @@ fun _t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.steer agent "also this";
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    state: running=true messages=0
    user: one
    queue: steer=1 follow_up=0
    assistant:
    tool_result: hi
    queue: steer=0 follow_up=0
    user: also this
    assistant: noted
    state: running=false messages=5
    |}]
;;

let%expect_test "abort cancels the tool and drops queued follow-ups" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ; Reply.text "never"
    ]
  @@ fun t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.follow_up agent "dropped";
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.2;
  ignore (Agent.abort agent);
  Agent.wait_idle agent;
  print_s [%sexp (Agent.is_running agent : bool)];
  dump ();
  [%expect
    {|
    false
    state: running=true messages=0
    user: go
    queue: steer=0 follow_up=1
    assistant:
    queue: steer=0 follow_up=0
    tool_result: [cancelled]
    state: running=false messages=3
    |}]
;;

let%expect_test
    "abort restores the raw text of a queued message with attachments"
  =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ]
  @@ fun t agent _dump ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.steer agent ~attachments:[ "notes.txt" ] "look at @notes.txt";
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.1;
  let restored = Agent.abort agent in
  print_s [%message (restored : string list)];
  Agent.wait_idle agent;
  [%expect {| (restored ("look at @notes.txt")) |}]
;;

let%expect_test "abort restores queued steer and follow-up messages" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ]
  @@ fun t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.steer agent "first steer";
  Agent.follow_up agent "second follow up";
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.2;
  let restored = Agent.abort agent in
  print_s [%message (restored : string list)];
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    (restored ("first steer" "second follow up"))
    state: running=true messages=0
    user: go
    queue: steer=1 follow_up=0
    queue: steer=1 follow_up=1
    assistant:
    queue: steer=0 follow_up=0
    tool_result: [cancelled]
    state: running=false messages=3
    |}]
;;

let%expect_test "dequeue pops the most recently queued message" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.2"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun _t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.steer agent "steer one";
  Agent.follow_up agent "follow up";
  let pop () = print_s [%sexp (Agent.dequeue agent : Agent.Queued.t option)] in
  pop ();
  pop ();
  pop ();
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    (((text "follow up") (attachments ())))
    (((text "steer one") (attachments ())))
    ()
    state: running=true messages=0
    user: go
    queue: steer=1 follow_up=0
    queue: steer=1 follow_up=1
    queue: steer=1 follow_up=0
    queue: steer=0 follow_up=0
    assistant:
    tool_result:
    assistant: done
    state: running=false messages=4
    |}]
;;

let%expect_test "model and thinking changes persist across session reload" =
  with_agent [ Reply.text "ok" ]
  @@ fun t agent _dump ->
  Agent.set_model agent (Option.value_exn (Model.find "deepseek-v4-pro"));
  Agent.set_thinking agent (On (Some Max));
  let path = (Agent.state agent).session_path in
  (* Settings alone do not create the file; the first message does. *)
  print_s [%sexp (Sys_unix.file_exists_exn path : bool)];
  Or_error.ok_exn (Agent.prompt agent "hello");
  Agent.wait_idle agent;
  print_s [%sexp (Sys_unix.file_exists_exn path : bool)];
  Eio.Switch.run
  @@ fun sw ->
  let agent2 =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~session:(Or_error.ok_exn (Session.load path))
      ~cwd:t.dir
      ()
  in
  let s = Agent.state agent2 in
  print_s [%sexp (s.model.id : string), (s.thinking : Thinking.t)];
  [%expect
    {|
    false
    true
    (deepseek-v4-pro (On (Max)))
    |}]
;;

let%expect_test "new_session, switch_session, fork, rewind" =
  with_agent [ Reply.text "a1"; Reply.text "b1"; Reply.text "a2" ]
  @@ fun _t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "a");
  Agent.wait_idle agent;
  let first = (Agent.state agent).session_path in
  Agent.new_session agent;
  Or_error.ok_exn (Agent.prompt agent "b");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.switch_session agent ~path:first);
  Or_error.ok_exn (Agent.fork agent ());
  let forked = (Agent.state agent).session_path in
  let first_entry = List.hd_exn (Session.active_path (Agent.session agent)) in
  Or_error.ok_exn (Agent.rewind agent ~to_:first_entry.id);
  Or_error.ok_exn (Agent.prompt agent "a again");
  Agent.wait_idle agent;
  print_s
    [%sexp
      { forked_is_new = (not (String.equal forked first) : bool)
      ; messages =
          (List.map (Agent.messages agent) ~f:(function
             | User u -> u.text
             | Assistant a -> Message.Assistant.text a
             | Tool_result _ -> "?")
           : string list)
      ; bad_switch =
          (Agent.switch_session agent ~path:"/nope.jsonl" |> Or_error.is_error
           : bool)
      }];
  dump ();
  [%expect
    {|
    ((forked_is_new true) (messages ("a again" a2)) (bad_switch true))
    state: running=true messages=0
    user: a
    assistant: a1
    state: running=false messages=2
    queue: steer=0 follow_up=0
    state: running=false messages=0
    state: running=true messages=0
    user: b
    assistant: b1
    state: running=false messages=2
    queue: steer=0 follow_up=0
    state: running=false messages=2
    queue: steer=0 follow_up=0
    state: running=false messages=2
    state: running=false messages=0
    state: running=true messages=0
    user: a again
    assistant: a2
    state: running=false messages=2
    |}]
;;

let%expect_test "manual compaction" =
  let long = String.make 40_000 'x' in
  with_agent
    [ Reply.text long
    ; Reply.text "recent reply"
    ; Reply.text "SUMMARY OF OLD STUFF"
    ; Reply.text "after"
    ]
  @@ fun _t agent dump ->
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  Or_error.ok_exn (Agent.prompt agent "old question");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "recent question");
  Agent.wait_idle agent;
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  print_s
    [%sexp
      (List.map (Agent.messages agent) ~f:(function
         | User u -> "user: " ^ String.prefix u.text 45
         | Assistant a ->
           "assistant: " ^ String.prefix (Message.Assistant.text a) 20
         | Tool_result _ -> "?")
       : string list)];
  Or_error.ok_exn (Agent.prompt agent "next");
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    (Error "nothing to compact: conversation is short")
    (Ok "SUMMARY OF OLD STUFF")
    ( "user: Summary of the conversation so far:\
     \nSUMMARY O" "user: recent question" "assistant: recent reply")
    state: running=true messages=0
    user: old question
    assistant: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    state: running=false messages=2
    state: running=true messages=2
    user: recent question
    assistant: recent reply
    state: running=false messages=4
    compacted: SUMMARY OF OLD STUFF
    state: running=false messages=3
    state: running=true messages=3
    user: next
    assistant: after
    state: running=false messages=5
    |}]
;;

let%expect_test "delete_session refuses the active session" =
  with_agent [ Reply.text "hi" ]
  @@ fun _t agent _dump ->
  Or_error.ok_exn (Agent.prompt agent "hello");
  Agent.wait_idle agent;
  let first = (Agent.state agent).session_path in
  Agent.new_session agent;
  let second = (Agent.state agent).session_path in
  print_s [%sexp (Agent.delete_session agent ~path:second : unit Or_error.t)];
  print_s [%sexp (Agent.delete_session agent ~path:first : unit Or_error.t)];
  print_s
    [%sexp
      { first_exists = (Sys_unix.file_exists_exn first : bool)
      ; second_exists = (Sys_unix.file_exists_exn second : bool)
      }];
  [%expect
    {|
    (Error "cannot delete the active session")
    (Ok ())
    ((first_exists false) (second_exists false))
    |}]
;;

let%expect_test "set_cwd changes state, persists, and is refused while running" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.3"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun t agent _dump ->
  let sub = Filename.concat t.dir "sub" in
  Core_unix.mkdir_p sub;
  print_s [%sexp (Agent.set_cwd agent ~path:sub : unit Or_error.t)];
  let state = Agent.state agent in
  print_s
    [%sexp
      (mask t state.cwd : string)
    , (mask t (Session.cwd (Agent.session agent)) : string)];
  Or_error.ok_exn (Agent.prompt agent "go");
  print_s [%sexp (Agent.set_cwd agent ~path:t.dir : unit Or_error.t)];
  Agent.wait_idle agent;
  let reloaded = Or_error.ok_exn (Session.load state.session_path) in
  print_s [%sexp (mask t (Session.cwd reloaded) : string)];
  [%expect
    {|
    (Ok ())
    ($DIR/sub $DIR/sub)
    (Error "cannot change directory while a run is in progress")
    $DIR/sub
    |}]
;;

let%expect_test "git_branch is read from .git/HEAD and refreshed" =
  with_agent [ Reply.text "ok" ]
  @@ fun t agent _dump ->
  print_s [%sexp ((Agent.state agent).git_branch : string option)];
  let repo = Filename.concat t.dir "repo" in
  let git = Filename.concat repo ".git" in
  Core_unix.mkdir_p (Filename.concat repo "sub");
  Core_unix.mkdir_p git;
  Out_channel.write_all
    (Filename.concat git "HEAD")
    ~data:"ref: refs/heads/feature\n";
  Or_error.ok_exn (Agent.set_cwd agent ~path:(Filename.concat repo "sub"));
  print_s [%sexp ((Agent.state agent).git_branch : string option)];
  Out_channel.write_all
    (Filename.concat git "HEAD")
    ~data:"0123456789abcdef0123456789abcdef01234567\n";
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_s [%sexp ((Agent.state agent).git_branch : string option)];
  [%expect
    {|
    ()
    (feature)
    (01234567)
    |}]
;;

let confirm_tools agent =
  Or_error.ok_exn
    (Agent.set_config agent { Config.default with confirm_tools = true })
;;

let tool_results t agent =
  List.filter_map (Agent.messages agent) ~f:(function
    | Message.Tool_result r -> Some (mask t r.text, r.is_error)
    | _ -> None)
;;

let%expect_test "confirm_tools: denying a destructive tool continues the run" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo hi"}|}
        ()
    ; Reply.text "after denial"
    ]
  @@ fun t agent dump ->
  confirm_tools agent;
  let confirmed, resolver = Eio.Promise.create () in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Tool_confirm { call_id; name; summary }) ->
      print_s [%sexp (call_id : string), (name : string), (summary : string)];
      Eio.Promise.resolve resolver call_id
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  let call_id = Eio.Promise.await confirmed in
  print_s
    [%sexp
      (Agent.respond_confirm agent ~call_id ~allow:false : unit Or_error.t)];
  Agent.wait_idle agent;
  print_s [%sexp (tool_results t agent : (string * bool) list)];
  dump ();
  [%expect
    {|
    (c1 bash "echo hi")
    (Ok ())
    (("[denied by user]" true))
    state: running=true messages=0
    user: go
    assistant:
    tool_result: [denied by user]
    assistant: after denial
    state: running=false messages=4
    |}]
;;

let%expect_test "confirm_tools: allowing a destructive tool runs it" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo hi"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun t agent dump ->
  confirm_tools agent;
  let confirmed, resolver = Eio.Promise.create () in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Tool_confirm { call_id; _ }) -> Eio.Promise.resolve resolver call_id
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  let call_id = Eio.Promise.await confirmed in
  print_s
    [%sexp (Agent.respond_confirm agent ~call_id ~allow:true : unit Or_error.t)];
  Agent.wait_idle agent;
  print_s [%sexp (tool_results t agent : (string * bool) list)];
  dump ();
  [%expect
    {|
    (Ok ())
    (("hi\n" false))
    state: running=true messages=0
    user: go
    assistant:
    tool_result: hi
    assistant: done
    state: running=false messages=4
    |}]
;;

let%expect_test "confirm_tools: summaries are path/command by tool" =
  with_agent
    [ Reply.tool_calls
        [ "c1", "write", {|{"path":"a.txt","content":"one\ntwo\n"}|}
        ; ( "c2"
          , "edit"
          , {|{"path":"a.txt","edits":[{"old_text":"one","new_text":"ONE"}]}|} )
        ; "c3", "bash", {|{"command":"echo hi"}|}
        ]
    ; Reply.text "done"
    ]
  @@ fun _t agent dump ->
  confirm_tools agent;
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Tool_confirm { call_id; name; summary }) ->
      print_s [%sexp (name : string), (summary : string)];
      print_s
        [%sexp
          (Agent.respond_confirm agent ~call_id ~allow:true : unit Or_error.t)]
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    (write a.txt)
    (Ok ())
    (edit a.txt)
    (Ok ())
    (bash "echo hi")
    (Ok ())
    state: running=true messages=0
    user: go
    assistant:
    tool_result: wrote 2 lines to $DIR/a.txt
    tool_result: --- a/a.txt
    +++ b/a.txt
    @@ -1,2 +1,2 @@
    -one
    +ONE
     two
    tool_result: hi
    assistant: done
    state: running=false messages=6
    |}]
;;

let%expect_test "confirm_tools: read is not gated" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"read" ~arguments:{|{"path":"missing"}|} ()
    ; Reply.text "done"
    ]
  @@ fun t agent dump ->
  confirm_tools agent;
  let confirms = ref 0 in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Tool_confirm _) -> incr confirms
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  printf "confirms=%d\n" !confirms;
  print_s [%sexp (tool_results t agent : (string * bool) list)];
  dump ();
  [%expect
    {|
    confirms=0
    (("file not found: $DIR/missing" true))
    state: running=true messages=0
    user: go
    assistant:
    tool_result: file not found: $DIR/missing
    assistant: done
    state: running=false messages=4
    |}]
;;

let%expect_test "confirm_tools: abort while waiting cancels the tool" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo hi"}|}
        ()
    ; Reply.text "never"
    ]
  @@ fun t agent dump ->
  confirm_tools agent;
  let confirmed, resolver = Eio.Promise.create () in
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Tool_confirm { call_id; _ }) -> Eio.Promise.resolve resolver call_id
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  let _call_id = Eio.Promise.await confirmed in
  ignore (Agent.abort agent : string list);
  Agent.wait_idle agent;
  print_s [%sexp (tool_results t agent : (string * bool) list)];
  dump ();
  [%expect
    {|
    (([cancelled] true))
    state: running=true messages=0
    user: go
    assistant:
    queue: steer=0 follow_up=0
    tool_result: [cancelled]
    state: running=false messages=3
    |}]
;;

let%expect_test "confirm_tools: responding to an unknown call id is an error" =
  with_agent []
  @@ fun _t agent _dump ->
  confirm_tools agent;
  print_s
    [%sexp
      (Agent.respond_confirm agent ~call_id:"nope" ~allow:true
       : unit Or_error.t)];
  [%expect {| (Error "no pending confirmation for tool call \"nope\"") |}]
;;

let%expect_test "abort kills a tool's whole process group promptly" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo started; sleep 30; echo never"}|}
        ()
    ; Reply.text "never"
    ]
  @@ fun t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
  let started = Time_ns.now () in
  ignore (Agent.abort agent);
  Agent.wait_idle agent;
  let elapsed = Time_ns.diff (Time_ns.now ()) started in
  printf "idle within a second: %b\n" Time_ns.Span.(elapsed < of_int_sec 1);
  dump ();
  [%expect
    {|
    idle within a second: true
    state: running=true messages=0
    user: go
    assistant:
    queue: steer=0 follow_up=0
    tool_result: started
    [cancelled]
    state: running=false messages=3
    |}]
;;

let%expect_test
    "the system prompt is fixed at the first run and reused after cwd changes, \
     which reach the model as notes on the next message"
  =
  let systems = Queue.create () in
  let users = Queue.create () in
  with_agent
    ~on_request:(fun request ->
      Queue.enqueue systems (Option.value request.system ~default:"");
      List.iter request.messages ~f:(function
        | Message.User u -> Queue.enqueue users u.text
        | _ -> ()))
    [ Reply.text "one"; Reply.text "two"; Reply.text "three" ]
  @@ fun t agent _dump ->
  let sub = Filename.concat t.dir "sub" in
  Core_unix.mkdir_p sub;
  Out_channel.write_all (Filename.concat sub "AGENTS.md") ~data:"sub rules";
  Or_error.ok_exn (Agent.prompt agent "first");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.set_cwd agent ~path:sub);
  Or_error.ok_exn (Agent.prompt agent "second");
  Agent.wait_idle agent;
  let systems = Queue.to_list systems in
  printf
    "requests: %d, identical system prompts: %b\n"
    (List.length systems)
    (List.all_equal systems ~equal:String.equal |> Option.is_some);
  let mentions s sub = String.is_substring s ~substring:sub in
  printf
    "system prompt mentions sub rules: %b\n"
    (mentions (List.hd_exn systems) "sub rules");
  Queue.to_list users
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:(fun u -> printf "user: %s\n" (mask t u));
  print_s
    [%sexp
      (Option.is_some (Session.system_prompt (Agent.session agent)) : bool)];
  let reloaded =
    Or_error.ok_exn (Session.load (Agent.state agent).session_path)
  in
  printf
    "reloaded prompt identical: %b\n"
    (Option.equal
       String.equal
       (Session.system_prompt reloaded)
       (Some (List.hd_exn systems)));
  Or_error.ok_exn (Agent.prompt agent "third");
  Agent.wait_idle agent;
  printf
    "third message carries no note: %b\n"
    (not (mentions (Queue.last_exn users) "Environment"));
  [%expect
    {|
    requests: 2, identical system prompts: true
    system prompt mentions sub rules: false
    user: [Environment: the working directory is now $DIR/sub. Project instructions for it may differ from the ones above. Re-reading AGENTS.md/CLAUDE.md there is at your discretion: they are often unchanged, and missing an update is not serious.]

    second
    user: first
    true
    reloaded prompt identical: true
    third message carries no note: true
    |}]
;;

let%expect_test "set_active_host with a cwd validates it on the new host" =
  with_agent [ Reply.text "ok" ]
  @@ fun t agent dump ->
  let sub = Filename.concat t.dir "sub" in
  Core_unix.mkdir_p sub;
  Or_error.ok_exn (Agent.prompt agent "first");
  Agent.wait_idle agent;
  dump ();
  let switch cwd =
    print_endline
      (mask
         t
         (Sexp.to_string_hum
            [%sexp
              (Agent.set_active_host agent "backend" ~cwd : unit Or_error.t)]));
    printf
      "cwd=%s hosts=%s\n"
      (mask t (Agent.state agent).cwd)
      (String.concat
         ~sep:","
         (List.map (Agent.hosts agent) ~f:(fun h -> mask t h.cwd)))
  in
  switch (Some "/no/such/dir");
  switch (Some sub);
  switch None;
  dump ();
  [%expect
    {|
    state: running=true messages=0
    user: first
    assistant: ok
    state: running=false messages=2
    (Error "<host>: not a directory: /no/such/dir")
    cwd=$DIR hosts=$DIR
    (Ok ())
    cwd=$DIR/sub hosts=$DIR/sub
    (Ok ())
    cwd=$DIR/sub hosts=$DIR/sub
    notice: tools now run on <host> in $DIR/sub
    state: running=false messages=2
    notice: tools now run on <host> in $DIR/sub
    state: running=false messages=2
    |}]
;;

let%expect_test
    "a message sent right after abort runs as soon as the cancelled tool is \
     gone"
  =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo started; sleep 30; echo never"}|}
        ()
    ; Reply.text "second run"
    ]
  @@ fun t agent dump ->
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.steer agent "queued";
  Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
  let restored = Agent.abort agent in
  print_s [%message (restored : string list)];
  (* The run is still winding down, so this queues; it starts by itself. *)
  Agent.steer agent "queued again";
  Agent.wait_idle agent;
  dump ();
  [%expect
    {|
    (restored (queued))
    state: running=true messages=0
    user: go
    queue: steer=1 follow_up=0
    assistant:
    queue: steer=0 follow_up=0
    queue: steer=1 follow_up=0
    tool_result: started
    [cancelled]
    queue: steer=0 follow_up=1
    state: running=false messages=3
    queue: steer=0 follow_up=0
    state: running=true messages=3
    user: queued again
    assistant: second run
    state: running=false messages=5
    |}]
;;

let%expect_test
    "auto-describe: after the second user turn, once, off the turn's critical \
     path"
  =
  let requests = Queue.create () in
  with_agent
    ~auto_describe:true
    ~on_request:(fun (r : Provider.Request.t) ->
      Queue.enqueue
        requests
        (sprintf
           "%s: %s"
           (Option.value_map r.system ~default:"-" ~f:(fun s ->
              String.prefix s 24))
           (match List.last r.messages with
            | Some (User u) ->
              String.prefix u.text 30
              |> String.split_lines
              |> String.concat ~sep:"|"
            | _ -> "?")))
    [ Reply.text "one"
    ; Reply.text "two"
    ; Reply.text "  \"Investigating flaky tests.\"\nignored second line"
    ; Reply.text "three"
    ]
  @@ fun _t agent dump ->
  let show_description () =
    print_s
      [%sexp
        (Session.description (Agent.session agent) : string option)
      , ((Agent.state agent).session_description : string option)]
  in
  Or_error.ok_exn (Agent.prompt agent "first question");
  Agent.wait_idle agent;
  show_description ();
  Or_error.ok_exn (Agent.prompt agent "second question");
  Agent.wait_idle agent;
  show_description ();
  Or_error.ok_exn (Agent.prompt agent "third question");
  Agent.wait_idle agent;
  show_description ();
  dump ();
  Queue.iter requests ~f:print_endline;
  [%expect
    {|
    (() ())
    (("Investigating flaky tests") ("Investigating flaky tests"))
    (("Investigating flaky tests") ("Investigating flaky tests"))
    state: running=true messages=0
    user: first question
    assistant: one
    state: running=false messages=2
    state: running=true messages=2
    user: second question
    assistant: two
    state: running=false messages=4
    state: running=false messages=4
    state: running=true messages=4
    user: third question
    assistant: three
    state: running=false messages=6
    You are prigh, a coding : first question
    You are prigh, a coding : second question
    Describe the conversatio: USER:|first question||ASSISTAN
    You are prigh, a coding : third question
    |}]
;;

let%expect_test
    "auto-describe: a failed description is a notice and is retried later"
  =
  with_agent
    ~auto_describe:true
    [ Reply.text "one"
    ; Reply.text "two"
    ; Reply.text ""
    ; Reply.text "three"
    ; Reply.text "Adding OAuth login"
    ]
  @@ fun _t agent dump ->
  List.iter [ "a"; "b"; "c" ] ~f:(fun p ->
    Or_error.ok_exn (Agent.prompt agent p);
    Agent.wait_idle agent);
  print_s [%sexp (Session.description (Agent.session agent) : string option)];
  dump ();
  [%expect
    {|
    ("Adding OAuth login")
    state: running=true messages=0
    user: a
    assistant: one
    state: running=false messages=2
    state: running=true messages=2
    user: b
    assistant: two
    state: running=false messages=4
    notice: session description failed: description failed: empty reply
    state: running=true messages=4
    user: c
    assistant: three
    state: running=false messages=6
    state: running=false messages=6
    |}]
;;

let%expect_test "session_description: cleaning replies" =
  List.iter
    [ "Fixing the build"
    ; "\"Fixing the build.\""
    ; "\n\n  Title: yes  \nmore"
    ; ""
    ; String.make 120 'x'
    ]
    ~f:(fun s -> printf "%S\n" (Session_description.clean s));
  [%expect
    {|
    "Fixing the build"
    "Fixing the build"
    "Title: yes"
    ""
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\226\128\166"
    |}]
;;
