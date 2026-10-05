open! Core
module H = Harness

let%expect_test
    "the model picker: logged-in providers first, fuzzy filter, Enter switches"
  =
  let h = H.create () in
  H.text h ~selector:".controls";
  [%expect {| (Claude Opus 5.5) (on) |}];
  H.key h "l" ~ctrl:true;
  H.text h ~selector:".modal";
  [%expect
    {|
    Open_model_picker
    (Focus picker-input)
    Switch model
    (Close (Esc))
    []
    Claude Opus 5.5 anthropic · 200k ctx ✓
    Claude Sonnet 5 anthropic · 200k ctx
    GPT-6 openai · 200k ctx · not logged in
    DeepSeek Chat deepseek · 200k ctx · not logged in
    ↑↓ move · Enter choose · Esc close
    |}];
  (* The highlight starts on the current model. *)
  H.show h ~selector:".picker-item.selected .picker-label";
  [%expect {| <span class="picker-label"> Claude Opus 5.5 </span> |}];
  H.act h (Picker_query "sonn");
  H.text h ~selector:".picker-items";
  [%expect {| Claude Sonnet 5 anthropic · 200k ctx |}];
  H.act h (Picker_query "");
  H.key h "ArrowDown" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.text h ~selector:".picker-item.selected";
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    GPT-6 openai · 200k ctx · not logged in
    |}];
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_model) (params ((model openai/gpt-6))) (tag Show_error))
    |}];
  H.text h ~selector:".modal";
  [%expect {| |}];
  (* Esc closes without switching. *)
  H.act h Open_model_picker;
  H.key h "Escape" ~target:Field;
  [%expect
    {|
    (Focus picker-input)
    Close_dialog
    (Focus editor)
    |}];
  (* Clicking an item chooses it. *)
  H.act h Open_model_picker;
  H.act h (Picker_choose "deepseek/deepseek-chat");
  [%expect
    {|
    (Focus picker-input)
    (Focus editor)
    (Rpc (method_ set_model) (params ((model deepseek/deepseek-chat)))
     (tag Show_error))
    |}];
  H.act h Open_model_picker;
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
    (Save_history ("/model sonnet"))
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Show_error))
    |}];
  H.type_ h "/model claude";
  H.act h Send;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history ("/model claude" "/model sonnet"))
    (Focus picker-input)
    Claude Opus 5.5 anthropic · 200k ctx ✓
    Claude Sonnet 5 anthropic · 200k ctx
    |}];
  H.act h Close_dialog;
  H.type_ h "/model gemini";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Focus editor)
    (Save_history ("/model gemini" "/model claude" "/model sonnet"))
    No model matches "gemini". Did you mean GPT-6, DeepSeek Chat, Claude Sonnet 5? Ctrl+L lists them all.
    |}]
;;

let%expect_test "thinking: a picker and /thinking, when the model supports it" =
  let h = H.create () in
  H.act h Open_thinking_picker;
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
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Show_error))
    |}];
  H.type_ h "/thinking max";
  H.act h Send;
  [%expect
    {|
    (Save_history ("/thinking max"))
    (Rpc (method_ set_thinking) (params ((thinking max))) (tag Show_error))
    |}];
  H.type_ h "/thinking lots";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Save_history ("/thinking lots" "/thinking max"))
    Unknown thinking level "lots": use off, low, on, high, max.
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
  H.text h ~selector:".controls";
  [%expect {| (DeepSeek Chat) |}];
  H.type_ h "/thinking high";
  H.act h Send;
  H.text h ~selector:".toast.error";
  [%expect
    {|
    (Save_history ("/thinking high" "/thinking lots" "/thinking max"))
    Unknown thinking level "lots": use off, low, on, high, max.
    DeepSeek Chat has no thinking levels: switch to a model that thinks with /model
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
  [%expect {| Ready 0% ↑0 ↓0 $0.0000 |}];
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "bob"; user = Some "ann" });
  H.event h (sprintf {|{"event":"state","state":%s}|} busy_state);
  H.event h {|{"event":"queue_update","steer":1,"follow_up":2}|};
  H.text h ~selector:".status";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Working 75% ↑123k ↓7.9k $1.2345 (3 queued) laptop
    ann as bob
    |}];
  H.show h ~selector:".context";
  [%expect
    {|
    <span title="Context: 150k of 200k tokens (/compact frees some)" class="context status-item warm">
      <span class="meter">
        <span class="meter-fill" style={ width: 75.00%; }> </span>
      </span>
      75%
    </span>
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
    (Expire_toast (id 0) (after_ms 4000))
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
    (Stop (Esc)) (Steer (Enter))
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
    (Expire_toast (id 0) (after_ms 4000))
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
