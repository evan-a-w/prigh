open! Core
open Prigh_web

let user text = sprintf {|{"role":"user","text":"%s"}|} text

let assistant ?(calls = []) text =
  let calls =
    List.map calls ~f:(fun id ->
      sprintf
        {|,{"type":"tool_call","id":"%s","name":"bash","arguments":"{\"command\":\"seq 12\"}"}|}
        id)
  in
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":"%s"}%s],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
    (String.concat calls)
;;

let result ?elided id text =
  sprintf
    {|{"role":"tool_result","tool_call_id":"%s","tool_name":"bash","text":%S,"is_error":false%s}|}
    id
    text
    (match elided with
     | None -> ""
     | Some (at, lines, bytes) ->
       sprintf {|,"elided":{"at":%d,"lines":%d,"bytes":%d}|} at lines bytes)
;;

let messages list = "[" ^ String.concat ~sep:"," list ^ "]"

let exchange n =
  [ user (sprintf "question %d" n); assistant (sprintf "answer %d" n) ]
;;

let exchanges ns = messages (List.concat_map ns ~f:exchange)

let paging h =
  print_s [%sexp ((Harness.model h).paging : Paging.t)];
  Harness.text h ~selector:".chat"
;;

(* Opens session [s] (not cached): its newest page comes with the reply. *)
let open_session h s =
  Harness.quiet h (fun () ->
    Harness.act h (Switch_session (sprintf "/sessions/%s.jsonl" s));
    Harness.reply h "switch_session" "{}";
    Harness.reply
      h
      "get_state"
      (Harness.state_json ~fields:[ "session_id", `String s ] ()))
;;

let%expect_test "the newest page first, older pages on scrolling up" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.reply
    h
    "get_messages_page"
    (Harness.page ~older:4 ~before:"e5" (exchanges [ 3; 4 ]));
  paging h;
  [%expect
    {|
    Scroll_to_bottom
    ((older 4) (before (e5)) (loading false))
    (Show earlier messages) 4 more
    question 3
    answer 3
    question 4
    answer 4
    |}];
  (* The page asks as the user scrolls near the top; asking again while it
     is on its way does nothing. *)
  Harness.act h Load_older;
  Harness.act h Load_older;
  paging h;
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e5) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before (e5)))))
    ((older 4) (before (e5)) (loading true))
    Loading earlier messages…
    question 3
    answer 3
    question 4
    answer 4
    |}];
  (* Added above, what shows stays in place. *)
  Harness.reply
    h
    "get_messages_page"
    (Harness.page ~older:2 ~before:"e3" (exchanges [ 2 ]));
  paging h;
  [%expect
    {|
    Keep_scroll_position
    ((older 2) (before (e3)) (loading false))
    (Show earlier messages) 2 more
    question 2
    answer 2
    question 3
    answer 3
    question 4
    answer 4
    |}];
  Harness.click h ~selector:".load-older";
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1 ]));
  paging h;
  [%expect
    {|
    Load_older
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e3) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before (e3)))))
    Keep_scroll_position
    ((older 0) (before ()) (loading false))
    question 1
    answer 1
    question 2
    answer 2
    question 3
    answer 3
    question 4
    answer 4
    |}];
  (* Everything is loaded. *)
  Harness.act h Load_older;
  [%expect {| |}]
;;

let%expect_test "the entries keep their keys as older pages come" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page ~older:2 ~before:"e3" (exchanges [ 2 ])));
  let keys () =
    let chat = (Harness.model h).chat in
    print_s
      [%sexp (Chat.first chat : int), (List.length (Chat.entries chat) : int)]
  in
  keys ();
  Harness.quiet h (fun () ->
    Harness.act h Load_older;
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1 ])));
  keys ();
  [%expect
    {|
    (0 2)
    (-2 4)
    |}]
;;

let%expect_test "an older page that fails, or is no longer wanted" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page ~older:2 ~before:"e3" (exchanges [ 2 ])));
  Harness.quiet h (fun () -> Harness.act h Load_older);
  Harness.fail h "get_messages_page" "the disk is full";
  paging h;
  Harness.text h ~selector:".toasts";
  [%expect
    {|
    ((older 2) (before (e3)) (loading false))
    (Show earlier messages) 2 more
    question 2
    answer 2
    Couldn't load earlier messages: the disk is full. Scroll up to try again. (Dismiss)
    |}];
  (* A page for a session left meanwhile is dropped. *)
  Harness.quiet h (fun () -> Harness.act h Load_older);
  open_session h "s3";
  Harness.quiet h (fun () ->
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1 ])));
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 7 ]));
  paging h;
  [%expect
    {|
    Scroll_to_bottom
    ((older 0) (before ()) (loading false))
    question 7
    answer 7
    |}]
;;

let%expect_test "a long output comes cut, whole when unfolded" =
  let h = Harness.create () in
  let head = String.concat (List.init 3 ~f:(fun i -> sprintf "%d\n" (i + 1))) in
  let tail =
    String.concat (List.init 3 ~f:(fun i -> sprintf "%d\n" (i + 10)))
  in
  let whole =
    String.concat (List.init 12 ~f:(fun i -> sprintf "%d\n" (i + 1)))
  in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page
         (messages
            [ user "count"
            ; assistant ~calls:[ "c1" ] "counting"
            ; result
                "c1"
                (head ^ tail)
                ~elided:(String.length head, 12, String.length whole)
            ])));
  Harness.text h ~selector:".tool";
  [%expect
    {|
    ✓ bash seq 12
    1
    2
    3
    (Show all 12 lines)
    |}];
  (* Unfolded, the rest is fetched (once), the gap saying how much. *)
  Harness.act h (Toggle_fold "c1");
  Harness.act h (Load_output "c1");
  Harness.text h ~selector:".tool";
  [%expect
    {|
    (Rpc (method_ get_tool_output) (params ((call_id c1)))
     (tag (Tool_output (agent_id ()) (call_id c1))))
    ✓ bash seq 12
    1
    2
    3
    6 more lines (Load them)
    10
    11
    12
    (Fold to 6 lines)
    |}];
  Harness.reply
    h
    "get_tool_output"
    (Jsonaf.to_string (`Object [ "text", `String whole ]));
  Harness.text h ~selector:".tool .output";
  print_s [%sexp (Set.to_list (Harness.model h).outputs_loading : string list)];
  [%expect
    {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
    11
    12
    (Fold to 6 lines)
    ()
    |}];
  (* Folded and unfolded again: it is whole now. *)
  Harness.act h (Toggle_fold "c1");
  Harness.act h (Toggle_fold "c1");
  [%expect {| |}]
;;

let%expect_test "an output that fails to load" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page
         (messages
            [ user "count"
            ; assistant ~calls:[ "c1" ] "counting"
            ; result "c1" "1\n100\n" ~elided:(2, 100, 292)
            ]));
    Harness.act h (Toggle_fold "c1"));
  Harness.fail h "get_tool_output" "no tool call c1";
  Harness.text h ~selector:".toasts";
  (* The button tries again. *)
  Harness.act h (Load_output "c1");
  [%expect
    {|
    Couldn't load the whole output: no tool call c1. Load them tries again. (Dismiss)
    (Rpc (method_ get_tool_output) (params ((call_id c1)))
     (tag (Tool_output (agent_id ()) (call_id c1))))
    |}]
;;

let%expect_test "switching back shows the transcript at once, then refreshes it"
  =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1 ])));
  open_session h "s3";
  Harness.quiet h (fun () ->
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 5 ])));
  (* Back to s2: no skeleton, its transcript shows while it is refreshed. *)
  Harness.act h (Switch_session "/sessions/s2.jsonl");
  Harness.reply h "switch_session" "{}";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_state"
      (Harness.state_json ~fields:[ "session_id", `String "s2" ] ()));
  paging h;
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    ((older 0) (before ()) (loading false))
    question 1
    answer 1
    |}];
  (* Unchanged: the same transcript (and the same views). *)
  let before = (Harness.model h).chat in
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1 ]));
  print_s [%sexp (phys_equal before (Harness.model h).chat : bool)];
  [%expect {| true |}];
  (* Changed meanwhile: merged into what is on show, which keeps its place
     (and its views) and what events added before the page came. *)
  open_session h "s3";
  Harness.quiet h (fun () ->
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 5 ])));
  open_session h "s2";
  let before = Chat.entries (Harness.model h).chat in
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"question 3"}}|};
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1; 2 ]));
  Harness.text h ~selector:".chat";
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1; 2; 3 ]));
  paging h;
  let after = Chat.entries (Harness.model h).chat in
  print_s
    [%message
      ""
        ~same_entries:
          (List.count (List.zip_exn before (List.take after 2)) ~f:(fun (a, b) ->
             phys_equal a b)
           : int)];
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 1000000)))
     (tag (Resync (session s2) (from ()))))
    question 1
    answer 1
    question 3
    ((older 0) (before ()) (loading false))
    question 1
    answer 1
    question 2
    answer 2
    question 3
    answer 3
    (same_entries 2)
    |}]
;;

(* Seen from elsewhere, a cached session's run goes on: switching to it, its
   tool calls without a result are running, not interrupted. *)
let%expect_test "a cached transcript of a running session" =
  let h = Harness.create () in
  open_session h "s2";
  let running_call =
    {|[{"role":"user","text":"build it"},
       {"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"faux"}]|}
  in
  Harness.quiet h (fun () ->
    Harness.reply h "get_messages_page" (Harness.page running_call);
    Harness.event h {|{"event":"tool_output","call_id":"c1","chunk":"step 1\n"}|});
  open_session h "s3";
  Harness.quiet h (fun () ->
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 5 ])));
  List.iter [ "s2" ] ~f:(fun s ->
    Harness.quiet h (fun () ->
      Harness.act h (Switch_session (sprintf "/sessions/%s.jsonl" s));
      Harness.reply h "switch_session" "{}";
      Harness.reply
        h
        "get_state"
        (Harness.state_json
           ~fields:[ "session_id", `String s; "running", `True ]
           ()));
    Harness.text h ~selector:".chat";
    Harness.elements h ~selector:".chat .tool");
  (* What it streams once back follows what it streamed before it left; what
     came between shows with the result. *)
  Harness.event h {|{"event":"tool_output","call_id":"c1","chunk":"step 3\n"}|};
  Harness.text h ~selector:".chat .tool";
  [%expect
    {|
    build it
    bash
    Output from while this page was away shows when it ends.
    step 1
    <div class="running tool tool-bash">
    bash
    Output from while this page was away shows when it ends.
    step 1
    step 3
    |}]
;;

let%expect_test "resting on a session in the sidebar fetches its newest page" =
  let h = Harness.create () in
  Harness.act h (Prefetch_session "/sessions/s2.jsonl");
  (* One at a time: the last one asked for goes next. *)
  Harness.act h (Prefetch_session "/sessions/s3.jsonl");
  Harness.act h (Prefetch_session "/sessions/s4.jsonl");
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 2 ]));
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 4 ]));
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s4.jsonl) (limit 60)))
     (tag (Page (path /sessions/s4.jsonl) (before ()))))
    |}];
  (* Fetched already, or the current session: nothing to do. *)
  Harness.act h (Prefetch_session "/sessions/s2.jsonl");
  Harness.act h (Prefetch_session "/sessions/s1.jsonl");
  [%expect {| |}];
  (* Opening it shows it at once. *)
  Harness.act h (Switch_session "/sessions/s2.jsonl");
  Harness.reply h "switch_session" "{}";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_state"
      (Harness.state_json ~fields:[ "session_id", `String "s2" ] ()));
  Harness.text h ~selector:".chat";
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    question 2
    answer 2
    |}]
;;

(* A session deleted (here or in another tab) leaves the cache with the
   list that no longer has it: nothing of it is kept or shown again. *)
let%expect_test "deleted sessions leave the cache" =
  let h = Harness.create () in
  Harness.quiet h (fun () ->
    Harness.act h (Prefetch_session "/sessions/s2.jsonl");
    Harness.reply h "get_messages_page" (Harness.page (exchanges [ 2 ])));
  Harness.act h (Prefetch_session "/sessions/s2.jsonl");
  [%expect {| |}];
  Harness.event h {|{"event":"sessions_changed"}|};
  Harness.reply
    h
    "list_sessions"
    ("[" ^ Harness.session_json "s1" ^ "," ^ Harness.session_json "s3" ^ "]");
  Harness.act h (Prefetch_session "/sessions/s2.jsonl");
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    |}]
;;

(* Reconnecting fetches again what is on show, from its first message (the
   older pages loaded stay), and what came since; they are merged into the
   transcript. *)
let%expect_test "reconnecting with older pages loaded" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page ~older:4 ~before:"e5" (exchanges [ 3; 4 ]));
    Harness.act h Load_older;
    Harness.reply
      h
      "get_messages_page"
      (Harness.page ~older:2 ~before:"e3" (exchanges [ 2 ])));
  let entries = Chat.entries (Harness.model h).chat in
  Harness.act h Backend_closed;
  Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_state"
      (Harness.state_json ~fields:[ "session_id", `String "s2" ] ()));
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s2)))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (from e3) (limit 60)))
     (tag (Resync (session s2) (from (e3)))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  Harness.reply
    h
    "get_messages_page"
    (Harness.page ~older:2 ~before:"e3" (exchanges [ 2; 3; 4; 5 ]));
  paging h;
  let kept =
    List.zip_with_remainder entries (Chat.entries (Harness.model h).chat)
    |> fst
    |> List.count ~f:(fun (a, b) -> phys_equal a b)
  in
  print_s [%message (kept : int)];
  [%expect
    {|
    ((older 2) (before (e3)) (loading false))
    (Show earlier messages) 2 more
    question 2
    answer 2
    question 3
    answer 3
    question 4
    answer 4
    question 5
    answer 5
    (kept 6)
    |}]
;;

let%expect_test "reconnecting when what was on show has gone from the session" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page ~older:4 ~before:"e5" (exchanges [ 3; 4 ])));
  Harness.act h Backend_closed;
  Harness.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_state"
      (Harness.state_json ~fields:[ "session_id", `String "s2" ] ()));
  Harness.fail
    h
    "get_messages_page"
    "message e5 is not in the session any more: load its latest messages again";
  Harness.reply h "get_messages_page" (Harness.page (exchanges [ 1 ]));
  paging h;
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s2)))
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (from e5) (limit 60)))
     (tag (Resync (session s2) (from (e5)))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    Scroll_to_bottom
    ((older 0) (before ()) (loading false))
    question 1
    answer 1
    |}]
;;

(* Compacted while older pages were left to load: their cursor is gone. The
   session's newest page (from its summary) replaces the transcript, once,
   rather than an error at every scroll. *)
let%expect_test "scrolling up after the session was compacted" =
  let h = Harness.create () in
  open_session h "s2";
  Harness.quiet h (fun () ->
    Harness.reply
      h
      "get_messages_page"
      (Harness.page ~older:40 ~before:"e41" (exchanges [ 21; 22 ])));
  Harness.event h {|{"event":"compacted","summary":"Worked on the parser."}|};
  Harness.act h Load_older;
  Harness.fail
    h
    "get_messages_page"
    "message e41 is not in the session any more: load its latest messages \
     again";
  Harness.reply
    h
    "get_messages_page"
    (Harness.page
       (messages
          [ user "Worked on the parser."; user "question 22"; assistant "answer 22" ]));
  paging h;
  Harness.text h ~selector:".toasts";
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e41) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before (e41)))))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Expire_toast (id 0) (after_ms 4260))
    Scroll_to_bottom
    ((older 0) (before ()) (loading false))
    Worked on the parser.
    question 22
    answer 22
    Earlier messages were compacted: showing the session from its summary
    |}]
;;
