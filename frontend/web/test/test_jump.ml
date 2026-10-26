open! Core
open Prigh_web
module H = Harness

let user ?(at = 0) text =
  (* Ints are 32 bits under JavaScript. *)
  sprintf
    {|{"role":"user","text":"%s","at":%.0f}|}
    text
    (1791194400000. +. Float.of_int at)
;;

let assistant ?(error = false) text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":%s,"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
    (if error
     then {|{"type":"error","message":"HTTP 529: overloaded"}|}
     else {|{"type":"end_turn"}|})
;;

let messages list = "[" ^ String.concat ~sep:"," list ^ "]"

(* Exchange [n] is messages [2(n-1)] and [2n-1]. *)
let exchange n =
  [ user ~at:(n * 60_000) (sprintf "question %d" n)
  ; assistant (sprintf "answer %d" n)
  ]
;;

let exchanges ns = messages (List.concat_map ns ~f:exchange)

let outline points =
  sprintf
    {|{"points":[%s]}|}
    (String.concat
       ~sep:","
       (List.map points ~f:(fun (index, kind, text) ->
          sprintf
            {|{"index":%d,"kind":"%s","text":"%s","at":%.0f}|}
            index
            kind
            text
            (1791194400000. +. Float.of_int (index * 30_000)))))
;;

(* Session s2, six exchanges, the last three loaded; the outline has the
   first three. *)
let long_session ?(outline_reply = true) () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  H.quiet h (fun () ->
    H.reply
      h
      "get_messages_page"
      (H.page ~older:6 ~before:"e7" (exchanges [ 4; 5; 6 ])));
  if outline_reply
  then
    H.reply
      h
      "get_outline"
      (outline
         [ 0, "user", "question 1"
         ; 2, "user", "question 2"
         ; 4, "user", "question 3"
         ; 6, "user", "question 4"
         ; 8, "user", "question 5"
         ]);
  h
;;

let%expect_test "opening a long session asks for its outline" =
  let h = long_session ~outline_reply:false () in
  print_s [%sexp ((H.model h).jump : Jump.t)];
  [%expect {| ((outline ()) (fetching (/sessions/s2.jsonl)) (pending ())) |}];
  H.reply
    h
    "get_outline"
    (outline [ 0, "user", "question 1"; 9, "future", "x" ]);
  print_s [%sexp ((H.model h).jump : Jump.t)];
  [%expect
    {|
    ((outline
      ((/sessions/s2.jsonl
        (((index 0) (kind User) (text "question 1") (at "2026-10-05 10:00:00Z"))))))
     (fetching ()) (pending ()))
    |}];
  (* Another session: its own, none when all of it is loaded. *)
  H.act h (Switch_session "/sessions/s3.jsonl");
  H.quiet h (fun () ->
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s3" ] ()));
  H.reply h "get_messages_page" (H.page (exchanges [ 1 ]));
  print_s [%sexp ((H.model h).jump : Jump.t)];
  [%expect
    {|
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s3.jsonl) (limit 60)))
     (tag (Page (path /sessions/s3.jsonl) (before ()))))
    (Focus editor)
    Scroll_to_bottom
    ((outline ()) (fetching ()) (pending ()))
    |}]
;;

let%expect_test
    "the rail: a mark per message of the user's, those not loaded too"
  =
  let h = long_session () in
  H.names h ~selector:".jump-nav button";
  H.elements h ~selector:".rail .tick";
  [%expect
    {|
    button "Jump to a message" -- Jump to a message (Ctrl+J)
    button "Your message 1, question 1, 10:00"
    button "Your message 2, question 2, 10:01"
    button "Your message 3, question 3, 10:02"
    button "Your message 4, question 4, 10:04"
    button "Your message 5, question 5, 10:05"
    button "Your message 6, question 6, 10:06"
    <button class="tick unloaded user">
    <button class="tick unloaded user">
    <button class="tick unloaded user">
    <button class="tick user">
    <button class="tick user">
    <button class="tick user">
    |}];
  (* What it stands for, on hover; the loaded ones say which entry. *)
  H.show h ~selector:".rail .tick:nth-child(4)";
  [%expect
    {|
    <button type="button"
            tabindex="-1"
            aria-label="Your message 4, question 4, 10:04"
            data-jump-entry="0"
            class="tick user"
            @on_click>
      <span aria-hidden="true" class="tip">
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        <span class="tip-text"> question 4 </span>
        <span class="tip-time"> 10:04 </span>
      </span>
    </button>
    |}];
  H.elements h ~selector:"#chat [data-entry]";
  [%expect
    {|
    <div class="msg user">
    <div class="msg user">
    <div class="msg user">
    |}];
  (* A loaded one: there at once. *)
  H.click h ~selector:".rail .tick" ~nth:4;
  [%expect
    {|
    (Jump_to (Entry 2))
    (Scroll_to_entry 2)
    |}]
;;

let%expect_test
    "a message not loaded: the pages down to it in one go, then there"
  =
  let h = long_session () in
  H.click h ~selector:".rail .tick" ~nth:1;
  print_s [%sexp ((H.model h).paging : Paging.t)];
  [%expect
    {|
    (Jump_to (Message 2))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e7) (limit 4)))
     (tag (Page (path /sessions/s2.jsonl) (before (e7)))))
    ((older 6) (before (e7)) (loading true))
    |}];
  (* Clicking again meanwhile only changes where to. *)
  H.click h ~selector:".rail .tick" ~nth:2;
  [%expect {| (Jump_to (Message 4)) |}];
  H.reply
    h
    "get_messages_page"
    (H.page ~older:2 ~before:"e3" (exchanges [ 2; 3 ]));
  H.text h ~selector:"#chat [data-entry=\"-2\"]";
  H.elements h ~selector:".rail .tick.unloaded";
  [%expect
    {|
    Keep_scroll_position
    (Scroll_to_entry -2)
    question 3
    10:03 (✂ Trim everything before this)
    <button class="tick unloaded user">
    |}];
  (* Further up than a page brought: the next one, then there. *)
  H.click h ~selector:".rail .tick" ~nth:0;
  H.reply h "get_messages_page" (H.page (exchanges [ 1 ]));
  H.text h ~selector:"#chat [data-entry=\"-6\"]";
  H.elements h ~selector:".rail .tick.unloaded";
  [%expect
    {|
    (Jump_to (Message 0))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e3) (limit 2)))
     (tag (Page (path /sessions/s2.jsonl) (before (e3)))))
    Keep_scroll_position
    (Scroll_to_entry -6)
    question 1
    10:01
    |}]
;;

let%expect_test "a jump while a page is on its way waits for it" =
  let h = long_session () in
  H.quiet h (fun () -> H.act h Load_older);
  H.act h (Jump_to (Message 0));
  [%expect {| |}];
  (* The page the scrolling asked for does not reach it: on to it. *)
  H.reply h "get_messages_page" (H.page ~older:4 ~before:"e5" (exchanges [ 3 ]));
  [%expect
    {|
    Keep_scroll_position
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e5) (limit 4)))
     (tag (Page (path /sessions/s2.jsonl) (before (e5)))))
    |}];
  (* It fails: no jump left waiting. *)
  H.fail h "get_messages_page" "the disk is full";
  print_s [%sexp ((H.model h).jump.pending : Jump.Target.t option)];
  H.text h ~selector:".toasts";
  [%expect
    {|
    ()
    Couldn't load earlier messages: the disk is full. Scroll up to try again. (Dismiss)
    |}]
;;

let%expect_test "Ctrl+J: the user's messages, newest first, filtered as typed" =
  let h = long_session () in
  H.key h ~ctrl:true "j";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    Open_jump_list
    (Focus picker-input)
    question 6 6 of 6  10:06 (Trim before)
    question 5 5 of 6  10:05 (Trim before)
    question 4 4 of 6  10:04 (Trim before)
    question 3 3 of 6  10:02 (Trim before)
    question 2 2 of 6  10:01 (Trim before)
    question 1 1 of 6  10:00
    |}];
  H.input h ~selector:"#picker-input" "qn2";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Picker_query qn2)
    question 2 2 of 6  10:01 (Trim before)
    |}];
  (* Enter jumps there (loading it), the dialog closes. *)
  H.key h ~target:Field "Enter";
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e7) (limit 4)))
     (tag (Page (path /sessions/s2.jsonl) (before (e7)))))
    false
    |}];
  (* Esc closes it without a jump. *)
  let h = long_session () in
  H.quiet h (fun () -> H.key h ~ctrl:true "j");
  H.key h ~target:Field "Escape";
  [%expect
    {|
    Open_jump_list
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "nothing to jump to yet" =
  let h = H.create () in
  H.key h ~ctrl:true "j";
  H.text h ~selector:".toasts";
  H.elements h ~selector:".jump-nav";
  [%expect
    {|
    Open_jump_list
    (Expire_toast (id 0) (after_ms 3540))
    No messages of yours to jump to yet: send one first
    |}]
;;

let%expect_test "Ctrl+↑ past the first message loaded loads the one before" =
  let h = long_session () in
  H.key h ~ctrl:true "ArrowUp";
  H.key h ~ctrl:true "ArrowDown";
  [%expect
    {|
    (Jump_to_user_message -1)
    (Jump_to_user_message -1)
    (Jump_to_user_message 1)
    (Jump_to_user_message 1)
    |}];
  (* The page found none above: the previous one, which is not loaded. *)
  H.act h Jump_older;
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e7) (limit 2)))
     (tag (Page (path /sessions/s2.jsonl) (before (e7)))))
    |}];
  (* Without the outline (an older backend): the page before. *)
  let h = long_session ~outline_reply:false () in
  H.fail h "get_outline" "unknown method get_outline";
  H.text h ~selector:".toasts";
  H.act h Jump_older;
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (before e7) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before (e7)))))
    |}]
;;

let%expect_test
    "errors and compactions are marks too; streaming does not redraw the rail"
  =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ());
    H.reply
      h
      "get_messages_page"
      (H.page
         (messages
            [ {|{"role":"user","text":"Summary of the conversation so far:\nwe did things","origin":{"source":{"kind":"summary","title":"Summary"}}}|}
            ; user "fix the build\\nit fails on CI"
            ; assistant ~error:true ""
            ; user "again"
            ; assistant "done"
            ])));
  H.names h ~selector:".rail .tick";
  H.elements h ~selector:".rail .tick";
  [%expect
    {|
    button "Context compacted"
    button "Your message 1, fix the build, 10:00"
    button "The model returned an error, HTTP 529: overloaded"
    button "Your message 2, again, 10:00"
    <button class="compaction tick">
    <button class="tick user">
    <button class="error tick">
    <button class="tick user">
    |}];
  let inject _ = Virtual_dom.Vdom.Effect.Ignore in
  let rail () = Jump_view.view (H.model h) ~inject in
  ignore (rail () : Virtual_dom.Vdom.Node.t);
  let before = rail () in
  H.quiet h (fun () ->
    H.event h {|{"event":"agent_start"}|};
    H.event
      h
      {|{"event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|};
    H.event
      h
      {|{"event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"Working"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"text_delta","text":"Working"}}|});
  print_s [%sexp (phys_equal before (rail ()) : bool)];
  [%expect {| true |}];
  H.quiet h (fun () -> H.act h (Set_draft "x"));
  print_s [%sexp (phys_equal before (rail ()) : bool)];
  [%expect {| true |}]
;;

let%expect_test "on a phone: no rail, the button opens the list" =
  let h = long_session () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".jump-nav > *";
  H.click h ~selector:".jump-list";
  H.elements h ~selector:".picker-dialog";
  [%expect
    {|
    <button class="btn ghost icon jump-list">
    <Vdom.Node.none-widget class="">
    Open_jump_list
    (Focus picker-input)
    <div class="modal picker-dialog">
    |}]
;;

let%expect_test
    "an outline for another session or before a compaction is not used"
  =
  let h = long_session ~outline_reply:false () in
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s3.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s3" ] ()));
  H.reply h "get_outline" (outline [ 0, "user", "question 1" ]);
  print_s [%sexp ((H.model h).jump : Jump.t)];
  [%expect {| ((outline ()) (fetching ()) (pending ())) |}];
  let h = long_session () in
  H.quiet h (fun () ->
    H.event h {|{"event":"compacted","summary":"we did things"}|});
  H.elements h ~selector:".rail .tick.unloaded";
  [%expect {| |}]
;;

let%expect_test "past 120 marks the rail picks evenly, the newest always" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ());
    H.reply h "get_messages_page" (H.page (exchanges (List.range 1 301))));
  H.names h ~selector:".rail .tick:last-child";
  H.names h ~selector:".rail .tick:first-child";
  [%expect
    {|
    button "Your message 300, question 300, 15:00"
    button "Your message 1, question 1, 10:01"
    |}];
  print_s
    [%sexp
      (List.length
         (Jump.points
            (H.model h).jump
            ~path:(Some "/sessions/s2.jsonl")
            ~paging:(H.model h).paging
            ~trim:None
            (H.model h).chat)
       : int)];
  [%expect {| 300 |}]
;;

let%expect_test "targets as picker ids" =
  List.iter [ "e12"; "e-3"; "m40"; "m-1"; "x1"; "e"; "" ] ~f:(fun s ->
    print_s
      [%sexp (s : string), (Jump.Target.of_string s : Jump.Target.t option)]);
  [%expect
    {|
    (e12 ((Entry 12)))
    (e-3 ((Entry -3)))
    (m40 ((Message 40)))
    (m-1 ())
    (x1 ())
    (e ())
    ("" ())
    |}]
;;

let%expect_test "a message's entry: tool results join their calls'" =
  let page =
    Prigh_protocol.Message_page.of_json
      (Jsonaf.of_string
         (H.page
            ~older:10
            ~before:"e11"
            (messages
               [ user "one"
               ; {|{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
               ; {|{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"ok","is_error":false}|}
               ; assistant "done"
               ; user "two"
               ])))
    |> Or_error.ok_exn
  in
  List.iter [ 9; 10; 13; 14; 15 ] ~f:(fun index ->
    print_s
      [%sexp
        (index : int)
      , (Jump.entry_of_index ~page ~first:(-4) index : int option)]);
  [%expect
    {|
    (9 ())
    (10 (-4))
    (13 (-2))
    (14 (-1))
    (15 ())
    |}]
;;

let%expect_test "a message sent adds its mark" =
  let h = long_session () in
  H.quiet h (fun () ->
    H.event
      h
      {|{"event":"message_start","message":{"role":"user","text":"and now?"}}|});
  H.names h ~selector:".rail .tick:last-child";
  [%expect {| button "Your message 7, and now?" |}]
;;
