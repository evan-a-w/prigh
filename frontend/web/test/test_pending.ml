open! Core
module H = Harness

(* Every action shows at once, before the backend answers. *)

let%expect_test "connecting: the chat, the composer and the status say so" =
  let h = H.connecting () in
  H.text h ~selector:".chat";
  H.text h ~selector:".status";
  H.show h ~selector:"button.send";
  [%expect
    {|
    Connecting to the backend…
    Connecting…
    <button type="button"
            title="Not connected to the backend"
            aria-label="Send"
            disabled=""
            class="btn primary send"
            @on_click>
      <icon class="send"> </icon>
    </button>
    |}];
  H.elements h ~selector:".composer";
  [%expect {| <footer class="composer offline"> |}];
  H.quiet h (fun () -> H.reply h "get_state" (H.state_json ()));
  H.text h ~selector:".chat";
  [%expect {| Loading the session… |}];
  H.reply h "get_messages_page" (H.page "[]");
  H.text h ~selector:".chat .empty h2";
  H.text h ~selector:".status-item.idle";
  H.elements h ~selector:".composer";
  [%expect
    {|
    Scroll_to_bottom
    What are we building?
    Ready
    <footer class="composer">
    |}]
;;

let%expect_test "reconnecting: the composer is marked and does not send" =
  let h = H.create () in
  H.act h Backend_closed;
  H.text h ~selector:".status";
  H.elements h ~selector:".composer";
  H.type_ h "still here";
  H.act h Send;
  H.text h ~selector:".toast";
  H.text h ~selector:".composer textarea";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    Reconnecting…
    <footer class="composer offline">
    Not connected to the backend: your message stays here until it is back (/retry-backend-connection tries now). (Dismiss)
    [still here]
    |}]
;;

let%expect_test "a prompt shows at once, until the transcript has it" =
  let h = H.create () in
  H.type_ h "fix the parser";
  H.act h Send;
  H.text h ~selector:".chat";
  H.show h ~selector:"button.send";
  H.text h ~selector:".status-item.running";
  [%expect
    {|
    (Save_history ("fix the parser"))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "fix the parser")))
     (tag (Sent (text "fix the parser") (images ()))))
    fix the parser
    Sending…
    <button type="button"
            title="Sending…"
            aria-label="Sending"
            disabled=""
            aria-busy="true"
            class="btn busy primary send"
            @on_click>
      <span aria-hidden="true" class="lamp"> </span>
    </button>
    Sending…
    |}];
  H.elements h ~selector:".msg.user";
  [%expect {| <div class="msg sending user"> |}];
  H.reply h "prompt" "{}";
  H.event h {|{"event":"agent_start"}|};
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"fix the parser"}}|};
  H.elements h ~selector:".msg.user";
  H.elements h ~selector:"button.send";
  [%expect
    {|
    <div class="msg user">
    <button class="btn primary send">
    |}]
;;

let%expect_test "a prompt the backend refuses goes back to the editor" =
  let h = H.create () in
  H.type_ h "hello";
  H.quiet h (fun () -> H.act h Send);
  H.fail h "prompt" "a run is already in progress";
  H.text h ~selector:".chat";
  H.text h ~selector:".composer textarea";
  H.text h ~selector:".toast";
  [%expect
    {|
    What are we building?
    /work
    / commands
    @ mention a file
    ! run a command
    Ctrl+L switch model
    Ctrl+K find a session
    [hello]
    Couldn't send: a run is already in progress. Your message is back in the editor. (Dismiss)
    |}]
;;

let%expect_test "a model or thinking level chosen shows until confirmed" =
  let h = H.create () in
  H.act h (Set_model "anthropic/claude-sonnet-5");
  H.show h ~selector:".model-chip";
  [%expect
    {|
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Model_set))
    <button type="button"
            title="Switching to Claude Sonnet 5…"
            disabled=""
            aria-busy="true"
            popovertarget="topbar-controls"
            popovertargetaction="hide"
            class="awaiting btn chip-button model-chip"
            @on_click>
      <span aria-hidden="true" class="lamp"> </span>
      <span class="chip-label"> Claude Sonnet 5 </span>
    </button>
    |}];
  let sonnet =
    Jsonaf.of_string
      (H.model_json
         ~provider:"anthropic"
         ~id:"claude-sonnet-5"
         ~name:"Claude Sonnet 5"
         ())
  in
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "model", sonnet ] ()));
  H.text h ~selector:".controls";
  [%expect {| (Claude Sonnet 5) (Thinking on) (Auto: never asks) |}];
  H.elements h ~selector:".chip-button.awaiting";
  [%expect {| |}];
  (* A level the backend refuses: back to what it was, saying why. *)
  H.act h (Set_thinking "max");
  H.text h ~selector:".thinking-chip";
  H.fail h "set_thinking" "no such level";
  H.text h ~selector:".controls";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ set_thinking) (params ((thinking max))) (tag Thinking_set))
    (Thinking max)
    (Claude Sonnet 5) (Thinking on) (Auto: never asks)
    No such level (Dismiss)
    |}]
;;

let%expect_test "a command that lists something opens its dialog at once" =
  let h = H.create () in
  H.type_ h "/session";
  H.act h Send;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/session))
    (Focus dialog)
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
    Session
    (Close (Esc))
    Loading…
    Esc closes
    |}];
  (* Esc closes it, and the answer then opens nothing. *)
  H.key h "Escape" ~target:Field;
  H.reply
    h
    "session_stats"
    {|{"session_id":"s1","path":"/sessions/s1.jsonl","name":null,"cwd":"/work","created_at":"2026-10-01 08:00:00Z","message_count":0,"user_messages":0,"assistant_messages":0,"tool_calls":0,"tool_results":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"model":"anthropic/claude-opus-5-5","thinking":"on"}|};
  H.elements h ~selector:".modal";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  (* The answer replaces it; a failure closes it and says why. *)
  H.type_ h "/fork";
  H.quiet h (fun () -> H.act h Send);
  H.elements h ~selector:".modal";
  H.fail h "get_entries" "no session file yet";
  H.elements h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    <div class="loading-dialog modal">
    (Focus editor)
    No session file yet (Dismiss)
    |}]
;;

let%expect_test "switching: the old session's events meanwhile change nothing" =
  let h = H.create () in
  H.act h (Switch_session "/sessions/s2.jsonl");
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "running", `True ] ()));
  H.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    Loading the session…
    |}];
  (* A session the backend resolves to the one we are in: nothing to load. *)
  H.quiet h (fun () ->
    H.reply h "switch_session" "{}";
    H.reply h "get_state" (H.state_json ()));
  H.text h ~selector:".chat h2";
  [%expect {| What are we building? |}]
;;
