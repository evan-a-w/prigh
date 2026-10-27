open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H

(* /delete drops a message from the transcript and the model's context;
   /trim after drops everything after one, folded into a divider. *)

let user text = sprintf {|{"role":"user","text":%S}|} text

let assistant text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":%S}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}|}
    text
;;

let tool_call =
  {|{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}|}
;;

let tool_result =
  {|{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"a.ml\nb.ml","is_error":false}|}
;;

let messages items = "[" ^ String.concat ~sep:"," items ^ "]"

let all_messages =
  [ user "first question"
  ; assistant "first answer"
  ; user "list the files"
  ; tool_call
  ; tool_result
  ; assistant "two files"
  ; user "third question"
  ; assistant "third answer"
  ]
;;

(* As [get_message_points] answers for [all_messages]. *)
let points_json =
  {|{"points":[
      {"message":"u1","index":0,"role":"user","text":"first question","at":1700000000000,"delete":{"messages":1,"tokens":40},"trim_after":{"messages":7,"tokens":2400}},
      {"message":"a1","index":1,"role":"assistant","text":"first answer","at":1700000060000,"delete":{"messages":1,"tokens":60},"trim_after":{"messages":6,"tokens":2340}},
      {"message":"u2","index":2,"role":"user","text":"list the files","at":1700000120000,"delete":{"messages":1,"tokens":40}},
      {"message":"a2","index":3,"role":"assistant","text":"bash","at":1700000130000,"delete":{"messages":2,"tokens":1200}},
      {"message":"r2","index":4,"role":"tool_result","text":"bash","at":1700000140000,"trim_after":{"messages":3,"tokens":1100}},
      {"message":"a3","index":5,"role":"assistant","text":"two files","at":1700000150000,"delete":{"messages":1,"tokens":900},"trim_after":{"messages":2,"tokens":200}},
      {"message":"u3","index":6,"role":"user","text":"third question\nwith more","at":1700000600000,"delete":{"messages":1,"tokens":100},"trim_after":{"messages":1,"tokens":100}},
      {"message":"a4","index":7,"role":"assistant","text":"third answer","at":1700000660000,"delete":{"messages":1,"tokens":100}}]}|}
;;

let deleted_json =
  {|{"entry":"d1","kind":"deleted","message":"a2","messages":2,"tokens":1200,"at":1700001000000}|}
;;

let trim_after_json ~divider =
  sprintf
    {|{"entry":"t1","kind":"trim_after","message":"a1","through":"a4","messages":6,"tokens":2340,"at":1700001000000,"divider":%d}|}
    divider
;;

let hidden items = [ "hidden", "[" ^ String.concat ~sep:"," items ^ "]" ]

let connected ?(height = 24) () =
  let h = Test_app.connected ~width:80 ~height () in
  H.reply ~quiet:true h Trim_reload (messages all_messages);
  h
;;

let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

(* The notice or error, in its box or on the editor's top rule. *)
let said h =
  let lines =
    String.split_lines
      (Screen.to_plain ~show_cursor:false (Render.screen h.H.model))
  in
  List.iter lines ~f:(fun line ->
    if
      String.is_prefix line ~prefix:"│"
      || (String.is_prefix line ~prefix:"──"
          && String.is_suffix line ~suffix:"───"
          && String.exists line ~f:Char.is_alpha)
    then print_endline line)
;;

let%expect_test
    "/delete: pick a message (newest first), it goes at once with its tool \
     results"
  =
  let h = connected () in
  run h "/delete";
  [%expect
    {| (Rpc (method_ get_message_points) (params ()) (tag (Message_points Delete))) |}];
  H.reply h (Message_points `Delete) points_json;
  H.show h;
  (* No tool result: it goes with its call. *)
  [%expect
    {|
    ▌ first question
      first answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      two files

    ▌ third question
      third answer
    Delete a message  (7)
    / ▏
    ▸  reply   third answer    2023-11-14 22:24  1 message · ~100 tokens
       you     third question  2023-11-14 22:23  1 message · ~100 tokens
       reply   two files       2023-11-14 22:15  1 message · ~900 tokens
       reply   bash            2023-11-14 22:15  2 messages · ~1.2k tokens
       you     list the files  2023-11-14 22:15  1 message · ~40 tokens
       reply   first answer    2023-11-14 22:14  1 message · ~60 tokens
       you     first question  2023-11-14 22:13  1 message · ~40 tokens
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "bash";
  H.enter h;
  [%expect
    {| (Rpc (method_ delete) (params ((message a2))) (tag (Deleted Delete))) |}];
  said h;
  [%expect
    {| ────────────────────────────────────────────────────────────────── deleting… ─── |}];
  H.reply h (Deleted `Delete) deleted_json;
  said h;
  [%expect
    {|
    │ 2 messages deleted (a reply and its tool results): /delete undo brings them  │
    │ back                                                                         │
    |}];
  (* The state says so (also when another frontend deleted): the transcript
     is the context, loaded again; the message just vanishes. *)
  H.event h (State (state ~extra:(hidden [ deleted_json ]) ()));
  [%expect {| (Rpc (method_ get_messages) (params ()) (tag Trim_reload)) |}];
  H.reply
    h
    Trim_reload
    (messages
       [ user "first question"
       ; assistant "first answer"
       ; user "list the files"
       ; assistant "two files"
       ; user "third question"
       ; assistant "third answer"
       ]);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ first question
      first answer

    ▌ list the files
      two files

    ▌ third question
      third answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ 2 messages deleted (a reply and its tool results): /delete undo brings them  │
    │ back                                                                         │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* The same state again reloads nothing. *)
  H.event h (State (state ~extra:(hidden [ deleted_json ]) ()));
  [%expect {| |}]
;;

let%expect_test "deleting one message says so; /delete undo brings it back" =
  let h = connected ~height:12 () in
  run h "/delete";
  H.reply ~quiet:true h (Message_points `Delete) points_json;
  H.enter h;
  H.reply
    h
    (Deleted `Delete)
    {|{"entry":"d2","kind":"deleted","message":"a4","messages":1,"tokens":100}|};
  said h;
  run h "/delete undo";
  H.reply
    h
    (Deleted `Undelete)
    {|{"entry":"d2","kind":"deleted","message":"a4","messages":1,"tokens":100}|};
  said h;
  run h "/delete undo";
  H.reply h (Deleted `Undelete) deleted_json;
  said h;
  run h "/delete undo";
  H.reply_error
    h
    (Deleted `Undelete)
    "nothing is deleted: /delete picks a message to delete";
  said h;
  [%expect
    {|
    (Rpc (method_ get_message_points) (params ()) (tag (Message_points Delete)))
    (Rpc (method_ delete) (params ((message a4))) (tag (Deleted Delete)))
    ─────────────────────────────── Message deleted: /delete undo brings it back ───
    (Rpc (method_ undelete) (params ()) (tag (Deleted Undelete)))
    ─────────────────────────────────────────────────────────── Message restored ───
    (Rpc (method_ undelete) (params ()) (tag (Deleted Undelete)))
    ──────────────────────────────────────────────────────── 2 messages restored ───
    (Rpc (method_ undelete) (params ()) (tag (Deleted Undelete)))
    ────────────────────── nothing is deleted: /delete picks a message to delete ───
    |}]
;;

let%expect_test "refused while a run is in progress; errors say what to do" =
  let h = connected ~height:8 () in
  H.event h (State (state ~running:true ()));
  (* Not Esc to dismiss them: it would abort the run. *)
  run h "/delete";
  said h;
  run h "/delete undo";
  said h;
  run h "/trim after";
  said h;
  H.event h (State (state ()));
  run h "/delete";
  H.reply h (Message_points `Delete) {|{"points":[]}|};
  said h;
  H.esc h;
  run h "/trim after";
  H.reply
    h
    (Message_points `Trim_after)
    {|{"points":[{"message":"u1","index":0,"role":"user","text":"hi","delete":{"messages":1,"tokens":4}}]}|};
  said h;
  H.esc h;
  run h "/trim later";
  said h;
  H.esc h;
  (* The backend refuses too (a run that started meanwhile): verbatim. *)
  run h "/delete";
  H.reply h (Message_points `Delete) points_json;
  H.enter h;
  H.reply_error
    h
    (Deleted `Delete)
    "cannot delete a message while a run is in progress: wait for the run to \
     end or abort it (Esc)";
  said h;
  [%expect
    {|
    │ cannot delete a message while a run is in progress: wait for the run to end  │
    │ or abort it (Esc)                                                            │
    │ cannot undo a deletion while a run is in progress: wait for the run to end   │
    │ or abort it (Esc)                                                            │
    │ cannot trim while a run is in progress: wait for the run to end or abort it  │
    │ (Esc)                                                                        │
    (Rpc (method_ get_message_points) (params ()) (tag (Message_points Delete)))
    ───────────────────────────── nothing to delete: the context has no messages ───
    (Rpc
      (method_ get_message_points)
      (params ())
      (tag (Message_points Trim_after)))
    │ nothing to trim after: no message has messages after it that can go (/trim   │
    │ trims before one)                                                            │
    │ unknown /trim argument "later": /trim picks a message, /trim after, /trim    │
    │ undo, /trim show or /trim hide                                               │
    (Rpc (method_ get_message_points) (params ()) (tag (Message_points Delete)))
    (Rpc (method_ delete) (params ((message a4))) (tag (Deleted Delete)))
    │ cannot delete a message while a run is in progress: wait for the run to end  │
    │ or abort it (Esc)                                                            │
    |}]
;;

let%expect_test
    "/trim after: pick a message, confirm; the divider stays where they were, \
     later messages below it"
  =
  let h = connected () in
  run h "/trim after";
  [%expect
    {|
    (Rpc
      (method_ get_message_points)
      (params ())
      (tag (Message_points Trim_after)))
    |}];
  H.reply h (Message_points `Trim_after) points_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ first question
      first answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      two files

    ▌ third question
      third answer
    Trim everything after  (5)
    / ▏
    ▸  you     third question  2023-11-14 22:23  1 message trimmed · ~100 tokens
       reply   two files       2023-11-14 22:15  2 messages trimmed · ~200 tokens
       result  bash            2023-11-14 22:15  3 messages trimmed · ~1.1k tokens
       reply   first answer    2023-11-14 22:14  6 messages trimmed · ~2.3k tokens
       you     first question  2023-11-14 22:13  7 messages trimmed · ~2.4k tokens
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  (* n cancels. *)
  H.keys h "first answer";
  H.enter h;
  H.keys h "n";
  said h;
  [%expect
    {| ────────────────────────────────────────────────────────────────── cancelled ─── |}];
  run h "/trim after";
  H.reply h (Message_points `Trim_after) points_json;
  H.keys h "first answer";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ get_message_points)
      (params ())
      (tag (Message_points Trim_after)))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ first question
      first answer

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      two files

    ▌ third question
      third answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Trim 6 messages (~2.3k tokens) after "first answer"? They leave the model's  │
    │ context; /trim undo brings them back (y/n)                                   │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {| (Rpc (method_ trim_after) (params ((after a1))) (tag (Trimmed Trim_after))) |}];
  H.reply h (Trimmed `Trim_after) (trim_after_json ~divider:2);
  said h;
  [%expect
    {| ───────────── trimmed 6 messages (~2.3k tokens): /trim undo brings them back ─── |}];
  (* A message sent since comes after the divider. *)
  H.event h (State (state ~extra:(hidden [ trim_after_json ~divider:2 ]) ()));
  H.reply
    h
    Trim_reload
    (messages
       [ user "first question"
       ; assistant "first answer"
       ; user "fourth question"
       ; assistant "fourth answer"
       ]);
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))












      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ first question
      first answer

    ──── 6 messages trimmed · ~2.3k tokens ──── /trim show · /trim undo

    ▌ fourth question
      fourth answer
    ───────────── trimmed 6 messages (~2.3k tokens): /trim undo brings them back ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* /trim undo cancels the latest trim, after as well as before. *)
  run h "/trim undo";
  H.reply h (Trimmed `Untrim) (trim_after_json ~divider:2);
  said h;
  [%expect
    {|
    (Rpc (method_ untrim) (params ()) (tag (Trimmed Untrim)))
    ────────────────────── 6 messages (~2.3k tokens) back in the model's context ───
    |}]
;;

let trim_before_json = {|{"before":"u2","messages":2,"tokens":100}|}

let trim_after_two_files =
  {|{"entry":"t1","kind":"trim_after","message":"a3","through":"a4","messages":2,"tokens":200,"divider":4}|}
;;

let trim_before_hidden =
  {|{"entry":"t0","kind":"trim_before","message":"u2","messages":2,"tokens":100}|}
;;

(* Trimmed before "list the files" and after "two files"; "fourth question"
   was sent since. *)
let both () =
  let h = connected ~height:30 () in
  H.event
    h
    (State
       (state
          ~extra:
            (("trim", trim_before_json)
             :: hidden [ trim_before_hidden; trim_after_two_files ])
          ()));
  H.reply
    ~quiet:true
    h
    Trim_reload
    (messages
       [ user "list the files"
       ; tool_call
       ; tool_result
       ; assistant "two files"
       ; user "fourth question"
       ; assistant "fourth answer"
       ]);
  h
;;

let%expect_test "a trim before and a trim after: both dividers" =
  let h = both () in
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))













      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ──── 2 messages trimmed · ~100 tokens ──── /trim show · /trim undo

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      two files

    ──── 2 messages trimmed · ~200 tokens ──── /trim show · /trim undo

    ▌ fourth question
      fourth answer
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/trim show unfolds every divider, read-only; hide folds them" =
  let h = both () in
  run h "/trim show";
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    (Rpc (method_ get_trimmed) (params ()) (tag (Trimmed_shown ())))
    (Rpc (method_ get_trimmed) (params ((entry t1))) (tag (Trimmed_shown (t1))))
    |}];
  H.reply
    h
    (Trimmed_shown None)
    (sprintf
       {|{"messages":%s}|}
       (messages [ user "first question"; assistant "first answer" ]));
  H.reply
    h
    (Trimmed_shown (Some "t1"))
    (sprintf
       {|{"messages":%s}|}
       (messages [ user "third question"; assistant "third answer" ]));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ┆ not in the model's context

    ┆ first question
    ┆ first answer

    ──── 2 messages trimmed · ~100 tokens ──── /trim hide · /trim undo

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      two files

    ┆ not in the model's context

    ┆ third question
    ┆ third answer

    ──── 2 messages trimmed · ~200 tokens ──── /trim hide · /trim undo

    ▌ fourth question
      fourth answer
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/trim hide";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ──── 2 messages trimmed · ~100 tokens ──── /trim show · /trim undo

    ▌ list the files
    ✓ bash $ ls
        a.ml
        b.ml
      two files

    ──── 2 messages trimmed · ~200 tokens ──── /trim show · /trim undo

    ▌ fourth question
      fourth answer
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "under /filter, both dividers stay where they were" =
  let h = both () in
  run h "/filter -user -tools";
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    (Rpc
      (method_ set_prefs)
      (params ((prefs ((tui.transcript-filter user,tools)))))
      (tag Ignore))




















      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ──── 2 messages trimmed · ~100 tokens ──── /trim show · /trim undo
      two files

    ──── 2 messages trimmed · ~200 tokens ──── /trim show · /trim undo
      fourth answer
    ──────────────────────── filter: −user −tools — /filter all shows everything ───
    > ▏
    deepseek-flash  think:off  mode:auto  filter: −user −tools  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "only a trim after: /trim show asks for its range alone" =
  let h = connected () in
  H.event h (State (state ~extra:(hidden [ trim_after_json ~divider:2 ]) ()));
  run h "/trim show";
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    (Rpc (method_ get_trimmed) (params ((entry t1))) (tag (Trimmed_shown (t1))))
    |}]
;;

let%expect_test "/rewind and /fork do not offer deleted messages" =
  let h = connected ~height:10 () in
  H.event
    h
    (State
       (state
          ~extra:
            (hidden
               [ {|{"entry":"d3","kind":"deleted","message":"u1","messages":1,"tokens":4}|}
               ])
          ()));
  H.reply ~quiet:true h Trim_reload (messages [ user "second question" ]);
  run h "/rewind";
  H.reply h Entries_for_rewind entries_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_rewind))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ second question
    Rewind to  (1)
    / ▏
    ▸* second question  #1
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;
