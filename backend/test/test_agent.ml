open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let with_agent ?tools ?on_request replies f =
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
      | Notice n -> Some ("notice: " ^ n)
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
    assistant:
    tool_result:
    assistant: first done
    state: running=false messages=4
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
    assistant:
    tool_result: hi
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
  Agent.abort agent;
  Agent.wait_idle agent;
  print_s [%sexp (Agent.is_running agent : bool)];
  dump ();
  [%expect
    {|
    false
    state: running=true messages=0
    user: go
    assistant:
    tool_result: [cancelled]
    state: running=false messages=3
    |}]
;;

let%expect_test "model and thinking changes persist across session reload" =
  with_agent [ Reply.text "ok" ]
  @@ fun t agent _dump ->
  Agent.set_model agent (Option.value_exn (Model.find "deepseek-v4-pro"));
  Agent.set_thinking agent (On (Some Max));
  let path = (Agent.state agent).session_path in
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
  [%expect {| (deepseek-v4-pro (On (Max))) |}]
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
    ((forked_is_new true) (messages (a "a again" a2)) (bad_switch true))
    state: running=true messages=0
    user: a
    assistant: a1
    state: running=false messages=2
    state: running=false messages=0
    state: running=true messages=0
    user: b
    assistant: b1
    state: running=false messages=2
    state: running=false messages=2
    state: running=false messages=2
    state: running=false messages=1
    state: running=true messages=1
    user: a again
    assistant: a2
    state: running=false messages=3
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
