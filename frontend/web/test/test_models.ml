open! Core
module H = Harness

let%expect_test
    "the model picker: logged-in providers first, fuzzy filter, Enter switches"
  =
  let h = H.create () in
  H.text h ~selector:".composer-controls";
  [%expect
    {|
    (Claude Opus 5.5)
    (Thinking on)
    (Auto: never asks)
    |}];
  H.act h (Run "/model");
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus picker-input)
    Switch model
    (Close (Esc))
    []
    Claude Opus 5.5 anthropic, 200k ctx ✓
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  (* The highlight starts on the current model. *)
  H.show h ~selector:".picker-item.selected .picker-label";
  [%expect {| <span class="picker-label"> Claude Opus 5.5 </span> |}];
  H.act h (Picker_query "sonn");
  H.text h ~selector:".picker-items";
  [%expect {| Claude Sonnet 5 anthropic, 200k ctx |}];
  H.act h (Picker_query "");
  H.key h "ArrowDown" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.text h ~selector:".picker-item.selected";
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    GPT-6 openai, 200k ctx, not logged in
    |}];
  (* It switches, and says how to log in to its provider. *)
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".toast";
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_model) (params ((model openai/gpt-6))) (tag Model_set))
    (Expire_toast (id 0) (after_ms 4140))
    GPT-6 needs openai, which is not logged in: /login openai logs in.
    |}];
  H.text h ~selector:".modal";
  [%expect {| |}];
  (* Esc closes without switching. *)
  H.act h (Run "/model");
  H.key h "Escape" ~target:Field;
  [%expect
    {|
    (Focus picker-input)
    Close_dialog
    (Focus editor)
    |}];
  (* Clicking an item chooses it. *)
  H.act h (Run "/model");
  H.act h (Picker_choose "deepseek/deepseek-chat");
  [%expect
    {|
    (Focus picker-input)
    (Focus editor)
    (Rpc (method_ set_model) (params ((model deepseek/deepseek-chat)))
     (tag Model_set))
    (Expire_toast (id 1) (after_ms 4620))
    |}];
  H.act h (Run "/model");
  H.act h (Picker_query "zz");
  H.text h ~selector:".picker-empty";
  [%expect
    {|
    (Focus picker-input)
    Nothing matches “zz”: Backspace widens the search.
    |}]
;;

let%expect_test "/model with a name switches, or says what matches" =
  let h = H.create () in
  H.type_ h "/model sonnet";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model sonnet") (max 500))) (tag Ignore))
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Model_set))
    |}];
  H.type_ h "/model claude";
  H.act h Send;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model claude") (max 500))) (tag Ignore))
    (Focus picker-input)
    Claude Opus 5.5 anthropic, 200k ctx ✓
    Claude Sonnet 5 anthropic, 200k ctx
    |}];
  H.act h Close_dialog;
  H.type_ h "/model gemini";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model gemini") (max 500))) (tag Ignore))
    No model matches “gemini”. Ctrl+L lists them all. (Dismiss)
    |}]
;;

let%expect_test "thinking: a picker and /thinking, when the model supports it" =
  let h = H.create () in
  H.act h (Run "/thinking");
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Focus picker-input)
    off answer straight away
    low a little
    on the provider's default budget ✓
    high more
    max as much as the model allows
    |}];
  H.act h (Picker_choose "high");
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    |}];
  H.type_ h "/thinking max";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/thinking max") (max 500))) (tag Ignore))
    (Rpc (method_ set_thinking) (params ((thinking max))) (tag Thinking_set))
    |}];
  H.type_ h "/thinking lots";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/thinking lots") (max 500))) (tag Ignore))
    Unknown thinking level “lots”: use off, low, on, high, max. (Dismiss)
    |}];
  let deepseek =
    Jsonaf.of_string
      {|{"id":"deepseek-chat","provider":"deepseek","key":"deepseek/deepseek-chat","name":"DeepSeek Chat","context_window":128000,"max_output":8000,"supports_thinking":false,"cost":{"input":1,"output":2,"cache_read":0.1}}|}
  in
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:[ "model", deepseek; "thinking", `String "off" ]
          ()));
  H.text h ~selector:".composer-controls";
  [%expect
    {|
    (DeepSeek Chat)
    (No thinking)
    (Auto: never asks)
    |}];
  H.type_ h "/thinking high";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/thinking high") (max 500))) (tag Ignore))
    DeepSeek Chat has no thinking levels: switch to a model that thinks with /model (Dismiss)
    |}]
;;

let busy_state =
  H.state_json
    ~fields:
      [ "running", `True
      ; "context_tokens", `Number "150000"
      ; "cost_usd", `Number "1.2345"
      ; ( "usage"
        , Jsonaf.of_string
            {|{"input":123456,"output":7890,"cache_read":100000}|} )
      ; "active_host", `String "c7"
      ; ( "hosts"
        , Jsonaf.of_string
            {|[{"id":"backend","name":"backend","cwd":"/work"},{"id":"c7","name":"laptop","cwd":"/home/ann/work","session_id":"s1","session_name":null}]|}
        )
      ; ( "subagents"
        , Jsonaf.of_string
            {|[{"id":"a1","task":"read the tests","running":true}]|} )
      ; ( "jobs"
        , Jsonaf.of_string
            {|[{"id":"j1","command":"make test","running":true,"exit":null},{"id":"j2","command":"sleep 1","running":false,"exit":"exited 0"}]|}
        )
      ]
    ()
;;

let%expect_test
    "the status line: running, context, tokens, cost, queue, background, host, \
     user"
  =
  let h = H.create () in
  H.text h ~selector:".status";
  H.text h ~selector:".status-extras";
  [%expect {| (0%) ↑0 ↓0 $0.0000 Ready |}];
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "bob"; user = Some "ann" });
  H.event h (sprintf {|{"event":"state","state":%s}|} busy_state);
  H.event
    h
    {|{"event":"queue_update","steer":1,"follow_up":2,"steer_texts":["a"],"follow_up_texts":["b","c"]}|};
  H.text h ~selector:".status";
  H.text h ~selector:".status-extras";
  H.text h ~selector:".card-where";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    (75%) ↑123k ↓7.9k $1.2345 Working
    (3 queued)
    (laptop) (main)
    |}];
  H.show h ~selector:".context";
  [%expect
    {|
    <button type="button"
            title="Context: 150k of 200k tokens (/compact frees some). Click for the session's details (/session)"
            aria-label="Context 75% full"
            class="context link status-item warm"
            @on_click>
      <span class="meter">
        <span class="meter-fill" style={ width: 75.00%; }> </span>
      </span>
      <span class="percent"> 75% </span>
    </button>
    |}];
  (* The queued messages come back into the editor, last first. *)
  H.type_ h "draft";
  H.act h Dequeue;
  H.reply h "dequeue" {|{"text":"also check the docs","attachments":[]}|};
  print_endline (H.model h).draft;
  [%expect
    {|
    (Rpc (method_ dequeue) (params ()) (tag Dequeued))
    (Focus editor)
    also check the docs

    draft
    |}];
  H.act h Dequeue;
  H.reply h "dequeue" "null";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ dequeue) (params ()) (tag Dequeued))
    (Expire_toast (id 0) (after_ms 2500))
    Nothing is queued
    |}]
;;

let%expect_test "aborting brings the queued prompts back into the editor" =
  let h = H.create () in
  H.event h (sprintf {|{"event":"state","state":%s}|} busy_state);
  H.text h ~selector:".composer-buttons";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    (Attach files (or drop them here)) (Stop (Esc)) (Steer (Enter))
    |}];
  H.type_ h "half-written";
  H.key h "Escape";
  [%expect
    {|
    Abort
    (Rpc (method_ abort) (params ()) (tag Restored))
    |}];
  H.reply h "abort" {|{"restored":["first","second"]}|};
  print_endline (H.model h).draft;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 3300))
    (Focus editor)
    first

    second

    half-written
    Stopped; 2 queued messages back in the editor
    |}];
  (* Nothing queued: only the run stops. *)
  H.act h Abort;
  H.reply h "abort" {|{"restored":[]}|};
  [%expect {| (Rpc (method_ abort) (params ()) (tag Restored)) |}]
;;
