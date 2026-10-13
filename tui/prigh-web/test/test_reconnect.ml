open! Core
open Prigh_web

(* The backend goes away: the page retries with backoff behind a banner
   (no toast per attempt), then rejoins the session and reloads it. *)
let%expect_test "reconnecting after the backend goes away" =
  let h = Harness.create () in
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  [%expect {| |}];
  Harness.act h Backend_closed;
  Harness.text h ~selector:".banner";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    Connection lost: reconnecting… (Retry now)
    |}];
  Harness.act h Backend_closed;
  for _ = 1 to 3 do
    Harness.act h (Reply (Reconnect 1, Error "cannot connect"))
  done;
  Harness.text h ~selector:".toasts";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 250) (session (s1)))
    (Reconnect (generation 1) (delay_ms 500) (session (s1)))
    (Reconnect (generation 1) (delay_ms 1000) (session (s1)))
    |}];
  Harness.act h (Reply (Reconnect 0, Error "stale"));
  Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
  Harness.reply h "get_state" (Harness.state_json ());
  Harness.reply
    h
    "get_messages_page"
    (Harness.page {|[{"role":"user","text":"hello"}]|});
  Harness.text h ~selector:".chat";
  let m = Harness.model h in
  print_s
    [%message
      ""
        ~connection:(m.connection : App.Connection.t)
        ~toasts:(List.length m.toasts : int)];
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 1000000)))
     (tag (Resync (session s1) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    hello
    ((connection Connected) (toasts 1))
    |}]
;;

(* Out of touch, nothing is known to be working: the page says it is
   offline (its lamps go dark, but for the banner's and the status line's). *)
let%expect_test "the page is offline while it reconnects" =
  let h = Harness.create () in
  Harness.elements h ~selector:".app";
  Harness.act h Backend_closed;
  Harness.elements h ~selector:".app";
  Harness.elements h ~selector:".banner .lamp";
  Harness.elements h ~selector:".status .lamp";
  [%expect
    {|
    <div class="app sidebar-open">
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    <div class="app offline sidebar-open">
    <span class="lamp">
    <span class="lamp">
    |}]
;;

let state () =
  Harness.state_json
    ~fields:
      [ "active_host", `String "c7"
      ; "cwd", `String "/home/me/proj"
      ; ( "hosts"
        , Jsonaf.of_string
            {|[{"id":"backend","name":"backend","cwd":"/work"},
               {"id":"c7","name":"laptop","cwd":"/home/me/proj","session_id":"s1","session_name":null}]|}
        )
      ]
    ()
;;

let user text = sprintf {|{"role":"user","text":"%s"}|} text

let assistant text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
;;

let messages list = "[" ^ String.concat ~sep:"," list ^ "]"

let session () =
  let h = Harness.create ~state:(state ()) () in
  Harness.quiet h (fun () ->
    Harness.act
      h
      (Reply
         ( Harness.newest "s1"
         , Ok
             (Jsonaf.of_string
                (Harness.page
                   (messages
                      [ user "first"
                      ; assistant "one"
                      ; user "second"
                      ; assistant "two"
                      ]))) )));
  h
;;

(* What the user had going stays as it was: the transcript (the same
   entries, so the page keeps their DOM and the scroll position), the draft,
   its attachments, the open panel and dialog. The messages are fetched again
   and merged: only what is new is added. *)
let%expect_test "a reconnect keeps the page as it was and merges what is new" =
  let h = session () in
  Harness.quiet h (fun () ->
    Harness.act h (Files (Set_base "http://box:7790"));
    Harness.act h (Files Toggle);
    Harness.reply
      h
      "fs_list"
      {|{"path":"/home/me/proj","parent":"/home/me","home":"/home/me","entries":[
          {"name":"README.md","kind":"file","size":2048,"mtime":1791100000000}],
          "total":1,"hidden":0}|};
    Harness.act h (Files (Select "README.md"));
    Harness.click h ~selector:".files-act.attach";
    Harness.act h Open_help);
  Harness.act h (Chat_scrolled { at_bottom = false });
  Harness.type_ h "half a thought";
  let before = Harness.model h in
  let show () =
    let m = Harness.model h in
    Harness.text h ~selector:".chat .entries";
    Harness.text h ~selector:".attach-chips";
    print_s
      [%message
        ""
          ~connection:(m.connection : App.Connection.t)
          ~draft:m.draft
          ~scrolled_up:(m.scrolled_up : bool)
          ~files_open:(m.files.open_ : bool)
          ~dialog:(Option.is_some m.dialog : bool)
          ~same_entries:
            (List.count
               (List.zip_exn
                  (List.take (Chat.entries m.chat) 4)
                  (Chat.entries before.chat))
               ~f:(fun (a, b) -> phys_equal a b)
             : int)]
  in
  [%expect {| (Files (Attach README.md)) |}];
  Harness.act h Backend_closed;
  Harness.act h (Reply (Reconnect 1, Error "cannot connect to ws://box/ws"));
  show ();
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Reconnect (generation 1) (delay_ms 250) (session (s1)))
    first
    one
    second
    two
    README.md 2.0 KB (Remove README.md)
    ((connection (Reconnecting (attempt 1) (generation 1)))
     (draft "half a thought") (scrolled_up true) (files_open true) (dialog true)
     (same_entries 4))
    |}];
  Harness.act
    h
    (Reply (Reconnect 1, Ok (Jsonaf.of_string {|{"client_id":"c2"}|})));
  Harness.reply h "get_state" (state ());
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 1000000)))
     (tag (Resync (session s1) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  (* The agent answered while we were away. No scrolling: the page stays
     where the user was reading. *)
  Harness.reply
    h
    "get_messages_page"
    (Harness.page
       (messages
          [ user "first"
          ; assistant "one"
          ; user "second"
          ; assistant "two"
          ; user "third"
          ; assistant "three"
          ]));
  Harness.reply
    h
    "get_pending"
    {|{"steer_texts":[],"follow_up_texts":[],"confirms":[],"questions":[]}|};
  show ();
  [%expect
    {|
    first
    one
    second
    two
    third
    three
    README.md 2.0 KB (Remove README.md)
    ((connection Connected) (draft "half a thought") (scrolled_up true)
     (files_open true) (dialog true) (same_entries 4))
    |}]
;;

(* Only the backend refusing the sign-in goes to the sign-in page, saying
   why; any other failure tries again. *)
let%expect_test "a sign-in the backend no longer accepts goes to sign in" =
  let h = session () in
  Harness.act h Backend_closed;
  Harness.act h (Reply (Reconnect 1, Error "timed out"));
  Harness.act
    h
    (Reply (Reconnect 1, Error "unauthorised: bad user name or password"));
  print_s [%sexp ((Harness.model h).connection : App.Connection.t)];
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Reconnect (generation 1) (delay_ms 250) (session (s1)))
    (Auth_failed
     "The backend no longer accepts this sign-in (unauthorised: bad user name or password): sign in again to go back to the session.")
    (Reconnecting (attempt 1) (generation 1))
    |}]
;;

(* The backend restarted without the session (it was never saved): the
   transcript stays, read-only, with what to do next; what the session the
   backend put us on does is not shown in it. *)
let%expect_test "a session the backend lost stays on show, read-only" =
  let h = session () in
  Harness.act h Backend_closed;
  Harness.act
    h
    (Reply
       ( Session_gone { generation = 1; session = "s1" }
       , Ok (Jsonaf.of_string {|{"client_id":"c2"}|}) ));
  Harness.text h ~selector:".banner";
  Harness.text h ~selector:".chat .entries";
  Harness.show h ~selector:"#editor";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    This session is no longer on the backend (it restarted before saving it): what it showed stays here, read-only. (New session) (Sessions)
    first
    one
    second
    two
    <textarea id="editor"
              placeholder="Session gone: /new starts one"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}];
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"another session's"}}|};
  Harness.type_ h "still here";
  Harness.act h Send;
  Harness.text h ~selector:".chat .entries";
  Harness.text h ~selector:".toasts";
  print_endline (Harness.model h).draft;
  [%expect
    {|
    first
    one
    second
    two
    This session is no longer on the backend: start a new one (/new) or pick another in the sidebar. Your message stays here. (Dismiss)
    still here
    |}];
  (* A new session: the draft goes with it. *)
  Harness.click h ~selector:".banner button";
  Harness.reply h "new_session" "{}";
  Harness.reply
    h
    "get_state"
    (Harness.state_json ~fields:[ "session_id", `String "s2" ] ());
  Harness.text h ~selector:".banner";
  print_s
    [%message
      ""
        ~session_gone:((Harness.model h).session_gone : string option)
        ~draft:(Harness.model h).draft];
  [%expect
    {|
    New_session
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    ((session_gone ()) (draft "still here"))
    |}]
;;

(* An empty session is never saved: losing it loses nothing, the backend's
   session takes over. *)
let%expect_test "an empty session the backend lost is replaced quietly" =
  let h = Harness.create () in
  Harness.act h Backend_closed;
  Harness.act
    h
    (Reply
       ( Session_gone { generation = 1; session = "s1" }
       , Ok (Jsonaf.of_string {|{"client_id":"c2"}|}) ));
  Harness.text h ~selector:".banner";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    |}]
;;

(* The browser says the network went or came back, or the page is on show
   again (a phone unlocked, the tab chosen): reconnect at once rather than at
   the end of the backoff; while connected, check that the socket still
   carries anything. *)
let%expect_test "the network and the page's visibility drive reconnecting" =
  let h = session () in
  Harness.act h (Network false);
  Harness.act h Backend_closed;
  Harness.act h (Reply (Reconnect 1, Error "cannot connect"));
  Harness.act h (Reply (Reconnect 1, Error "cannot connect"));
  Harness.act h (Reply (Reconnect 1, Error "cannot connect"));
  [%expect
    {|
    Drop_connection
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Reconnect (generation 1) (delay_ms 250) (session (s1)))
    (Reconnect (generation 1) (delay_ms 500) (session (s1)))
    (Reconnect (generation 1) (delay_ms 1000) (session (s1)))
    |}];
  (* Offline again while reconnecting: nothing to drop. *)
  Harness.act h (Network false);
  Harness.act h (Network true);
  Harness.text h ~selector:".banner";
  [%expect
    {|
    (Reconnect (generation 2) (delay_ms 0) (session (s1)))
    Connection lost: reconnecting… (Retry now)
    |}];
  (* The first attempt's backoff is over: its late answer is not. *)
  Harness.act h (Reply (Reconnect 1, Error "cannot connect"));
  Harness.act h (Reply (Reconnect 2, Error "cannot connect"));
  Harness.act h (Page_hidden true);
  Harness.act h (Page_hidden false);
  [%expect
    {|
    (Reconnect (generation 2) (delay_ms 250) (session (s1)))
    (Reconnect (generation 3) (delay_ms 0) (session (s1)))
    |}];
  Harness.act h (Reply (Reconnect 3, Ok (Jsonaf.of_string "{}")));
  Harness.quiet h (fun () -> ());
  Harness.act h (Page_hidden true);
  Harness.act h (Page_hidden false);
  Harness.act h (Network true);
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 1000000)))
     (tag (Resync (session s1) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Probe_connection
    Probe_connection
    |}]
;;

(* While offline a message cannot be sent: it stays in the composer, which
   says so, until the connection is back. *)
let%expect_test "sending while offline keeps the draft" =
  let h = session () in
  Harness.act h Backend_closed;
  Harness.type_ h "hold on";
  Harness.act h Send;
  Harness.show h ~selector:"#editor";
  Harness.text h ~selector:".toasts";
  print_endline (Harness.model h).draft;
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    <textarea id="editor"
              placeholder="Offline: write now, send once it is back"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value="hold on"
              @on_input> </textarea>
    Not connected to the backend: your message stays here until it is back (/retry-backend-connection tries now). (Dismiss)
    hold on
    |}]
;;

(* What [get_messages] says is merged into the transcript on show: the same
   entries and calls stay the same values (so their views are kept), with
   what only the page had: a call's streamed output, the message still being
   streamed. *)
let%expect_test "merging the messages after reconnecting" =
  let call =
    {|{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
  in
  let result =
    {|{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"built\n","is_error":false}|}
  in
  let of_messages ~running list =
    Chat.of_messages
      ~running
      (List.map list ~f:(fun json ->
         Prigh_protocol.Message.of_json (Jsonaf.of_string json)
         |> Or_error.ok_exn))
  in
  let old =
    List.fold
      [ {|{"event":"agent_start"}|}
      ; {|{"event":"message_start","message":{"role":"user","text":"build it"}}|}
      ; sprintf {|{"event":"message_end","message":%s}|} call
      ; {|{"event":"tool_output","call_id":"c1","chunk":"compiling\n"}|}
      ]
      ~init:Chat.empty
      ~f:Chat_harness.apply
  in
  let fresh = of_messages ~running:true [ user "build it"; call ] in
  let merged = Chat.reconcile ~old fresh in
  print_s [%message "nothing new" ~same:(phys_equal merged old : bool)];
  Chat_harness.text merged;
  [%expect
    {|
    ("nothing new" (same true))
    build it bash make compiling
    |}];
  (* The call finished while we were away, and the agent said something
     (and is saying more). *)
  let streaming =
    Chat_harness.apply
      old
      {|{"event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"All bu"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"text_delta","text":"All bu"}}|}
  in
  let fresh = of_messages ~running:true [ user "build it"; call; result ] in
  let merged = Chat.reconcile ~old:streaming fresh in
  let kept =
    List.zip_exn (Chat.entries merged) (Chat.entries streaming)
    |> List.map ~f:(fun (a, b) -> phys_equal a b)
  in
  print_s
    [%message
      ""
        (kept : bool list)
        ~result:
          (Option.bind (Chat.tool merged "c1") ~f:(fun t -> t.result)
           |> Option.map ~f:(fun r -> r.text)
           : string option)];
  [%expect {| ((kept (true true true)) (result ("built\n"))) |}];
  (* Once it has stopped, the message being streamed is in [get_messages] (or
     never will be). *)
  let merged =
    Chat.reconcile
      ~old:streaming
      (of_messages ~running:false [ user "build it"; call; result ])
  in
  print_s [%message "" ~entries:(List.length (Chat.entries merged) : int)];
  [%expect {| (entries 2) |}]
;;

(* The connection went while the session was loading: the calls it took
   with it say nothing (the banner does), the skeleton stays until the
   reconnect brings the messages. *)
let%expect_test "losing the connection while a session loads" =
  let h = Harness.create () in
  Harness.act h (Switch_session "/sessions/s2.jsonl");
  Harness.reply h "switch_session" "{}";
  Harness.reply
    h
    "get_state"
    (Harness.state_json ~fields:[ "session_id", `String "s2" ] ());
  Harness.act h Backend_closed;
  Harness.fail h "get_messages_page" "backend closed";
  Harness.fail h "list_sessions" "backend closed";
  Harness.text h ~selector:".chat";
  Harness.text h ~selector:".toasts";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Set_url_session s2)
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Reconnect (generation 1) (delay_ms 0) (session (s2)))
    Loading the session…
    |}];
  Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_state"
      (Harness.state_json ~fields:[ "session_id", `String "s2" ] ()));
  Harness.reply
    h
    "get_messages_page"
    (Harness.page (messages [ user "hi"; assistant "hello" ]));
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Resync (session s2) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Scroll_to_bottom
    hi
    hello
    |}]
;;

(* Lost with the backend, the session's live state goes too: what its agent
   waited for, its queue, its run. *)
let%expect_test "a session the backend lost leaves nothing waiting" =
  let h = session () in
  Harness.quiet h (fun () ->
    Harness.event
      h
      (Harness.state_json ~fields:[ "running", `True ] ()
       |> sprintf {|{"event":"state","state":%s}|});
    Harness.event
      h
      {|{"event":"tool_confirm","call_id":"c1","agent_id":null,"name":"bash","summary":"rm -rf build","rules":[]}|};
    Harness.event
      h
      {|{"event":"queue_update","steer":0,"follow_up":1,"steer_texts":[],"follow_up_texts":["and then this"]}|});
  let show () =
    let m = Harness.model h in
    print_s
      [%message
        ""
          ~running:(App.Model.running m : bool)
          ~confirms:(List.length m.confirms : int)
          ~queued:(Queued.count m.queue : int)];
    Harness.elements h ~selector:".app .lamp"
  in
  show ();
  Harness.act h Backend_closed;
  Harness.act
    h
    (Reply
       ( Session_gone { generation = 1; session = "s1" }
       , Ok (Jsonaf.of_string {|{"client_id":"c2"}|}) ));
  show ();
  [%expect
    {|
    ((running true) (confirms 1) (queued 1))
    <span class="lamp">
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    ((running false) (confirms 0) (queued 0))
    |}]
;;

(* The connection went while a message was being sent: it may or may not
   have reached the agent. Once back, the transcript says: there, it is not
   put back (sending it again would send it twice); not there, it is back in
   the editor. *)
let%expect_test "a message sent as the connection went" =
  let pending =
    {|{"confirms":[],"questions":[],"steer_texts":[],"follow_up_texts":[]}|}
  in
  let lost_while_sending ~arrived =
    let h = session () in
    Harness.type_ h "third";
    Harness.quiet h (fun () ->
      Harness.act h Send;
      Harness.fail h "prompt" "backend closed";
      Harness.act h Backend_closed;
      Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
      Harness.reply h "get_state" (Harness.state_json ());
      Harness.reply
        h
        "get_messages_page"
        (Harness.page
           (messages
              ([ user "first"; assistant "one"; user "second"; assistant "two" ]
               @ if arrived then [ user "third" ] else [])));
      Harness.reply h "get_pending" pending);
    Harness.text h ~selector:".chat .entries";
    Harness.text h ~selector:".toasts";
    print_s [%message "" ~draft:(Harness.model h).draft]
  in
  lost_while_sending ~arrived:true;
  [%expect
    {|
    first
    one
    second
    two
    third
    Reconnected
    (draft "")
    |}];
  lost_while_sending ~arrived:false;
  [%expect
    {|
    first
    one
    second
    two
    Reconnected
    The connection went before your message reached the agent: it is back in the editor. (Dismiss)
    (draft third)
    |}]
;;

(* What only the page shows (a [!!command]'s card, which is not in the
   context) stays where it was when the transcript is merged after
   reconnecting, and so do the views of the messages after it. *)
let%expect_test "reconnecting keeps a shell card the backend does not have" =
  let h = session () in
  Harness.quiet h (fun () ->
    Harness.event
      h
      {|{"event":"tool_start","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"ls\"}"}}|};
    Harness.event
      h
      {|{"event":"message_start","message":{"role":"user","text":"third"}}|});
  let before = Chat.entries (Harness.model h).chat in
  Harness.quiet h (fun () ->
    Harness.act h Backend_closed;
    Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
    Harness.reply h "get_state" (Harness.state_json ());
    Harness.reply
      h
      "get_messages_page"
      (Harness.page
         (messages
            [ user "first"
            ; assistant "one"
            ; user "second"
            ; assistant "two"
            ; user "third"
            ; assistant "three"
            ])));
  let after = Chat.entries (Harness.model h).chat in
  Harness.text h ~selector:".chat .entries";
  print_s
    [%message
      ""
        ~same_entries:
          (List.count
             (List.zip_exn before (List.take after 6))
             ~f:(fun (a, b) -> phys_equal a b)
           : int)];
  [%expect
    {|
    first
    one
    second
    two
    bash ls
    third
    three
    (same_entries 6)
    |}]
;;

(* A command's output streamed while the connection was down is not on show:
   the card says so until its result comes. *)
let%expect_test "output streamed while disconnected" =
  let h = session () in
  Harness.quiet h (fun () ->
    Harness.event
      h
      {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|};
    Harness.event
      h
      {|{"event":"tool_start","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"}}|};
    Harness.event
      h
      {|{"event":"tool_output","call_id":"c1","chunk":"step 1\n"}|};
    Harness.act h Backend_closed;
    Harness.act
      h
      (Reply (Reconnect 1, Ok (Jsonaf.of_string {|{"client_id":"c2"}|})));
    Harness.event
      h
      {|{"event":"tool_output","call_id":"c1","chunk":"step 3\n"}|});
  Harness.text h ~selector:".chat .tool";
  [%expect
    {|
    bash make
    Output from while this page was away shows when it ends.
    step 1
    step 3
    |}];
  Harness.event
    h
    {|{"event":"tool_end","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"step 1\nstep 2\nstep 3\n","is_error":false}}|};
  Harness.text h ~selector:".chat .tool";
  [%expect
    {|
    ✓ bash make
    step 1
    step 2
    step 3
    |}]
;;
