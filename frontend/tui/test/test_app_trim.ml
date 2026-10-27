open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* /trim: drop everything before a message (one of yours or a reply) from
   the model's context; the transcript folds it into one divider. *)

let user text = sprintf {|{"role":"user","text":%S}|} text

let assistant text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":%S}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}|}
    text
;;

let messages texts =
  "["
  ^ String.concat
      ~sep:","
      (List.concat_map texts ~f:(fun (q, a) -> [ user q; assistant a ]))
  ^ "]"
;;

let points_json =
  {|{"points":[
      {"before":"a1","index":1,"role":"assistant","text":"first answer","messages":1,"tokens":400},
      {"before":"u2","index":2,"role":"user","text":"second question","at":1700000000000,"messages":2,"tokens":950},
      {"before":"a2","index":3,"role":"assistant","text":"second answer","messages":3,"tokens":1200},
      {"before":"u3","role":"user","index":4,"text":"third question\nwith more","at":1700000600000,"messages":4,"tokens":38000}]}|}
;;

let trim_json = {|{"before":"u3","messages":4,"tokens":38000}|}

let connected ?(height = 16) () =
  let h = Test_app.connected ~width:80 ~height () in
  H.reply
    ~quiet:true
    h
    Trim_reload
    (messages
       [ "first question", "first answer"
       ; "second question", "second answer"
       ; "third question", "third answer"
       ]);
  h
;;

let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let%expect_test
    "/trim: pick a message, confirm, the transcript folds what is before it"
  =
  let h = connected () in
  run h "/trim";
  [%expect
    {| (Rpc (method_ get_trim_points) (params ()) (tag Trim_points_picker)) |}];
  H.reply h Trim_points_picker points_json;
  H.show h;
  [%expect
    {|
      first answer

    ▌ second question
      second answer

    ▌ third question
      third answer
    Trim everything before  (4)
    / ▏
    ▸  you     third question   2023-11-14 22:23  4 messages trimmed · ~38k tokens
       reply   second answer    3 messages trimmed · ~1.2k tokens
       you     second question  2023-11-14 22:13  2 messages trimmed · ~950 tokens
       reply   first answer     1 message trimmed · ~400 tokens
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  (* Newest first: Enter on the third question. *)
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ first question
      first answer

    ▌ second question
      second answer

    ▌ third question
      third answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Trim 4 messages (~38k tokens) before "third question"? They leave the        │
    │ model's context; /trim undo brings them back (y/n)                           │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {| (Rpc (method_ trim) (params ((before u3))) (tag (Trimmed Trim))) |}];
  H.reply h (Trimmed `Trim) {|{"messages":4,"tokens":38000}|};
  (* The state says so (also when another frontend trimmed): the transcript
     is the context, loaded again. *)
  H.event h (State (state ~extra:[ "trim", trim_json ] ()));
  [%expect {| (Rpc (method_ get_messages) (params ()) (tag Trim_reload)) |}];
  H.reply h Trim_reload (messages [ "third question", "third answer" ]);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ──── 4 messages trimmed · ~38k tokens ──── /trim show · /trim undo

    ▌ third question
      third answer
    ────────────── trimmed 4 messages (~38k tokens): /trim undo brings them back ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/trim before a reply: the context starts at it" =
  let h = connected () in
  run h "/trim";
  H.reply ~quiet:true h Trim_points_picker points_json;
  H.keys h "second answer";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_trim_points) (params ()) (tag Trim_points_picker))

    ▌ first question
      first answer

    ▌ second question
      second answer

    ▌ third question
      third answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Trim 3 messages (~1.2k tokens) before "second answer"? They leave the        │
    │ model's context; /trim undo brings them back (y/n)                           │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {| (Rpc (method_ trim) (params ((before a2))) (tag (Trimmed Trim))) |}];
  H.reply h (Trimmed `Trim) {|{"messages":3,"tokens":1200}|};
  H.event
    h
    (State
       (state
          ~extra:
            [ ( "trim"
              , {|{"before":"a2","role":"assistant","messages":3,"tokens":1200}|}
              )
            ]
          ()));
  H.reply
    h
    Trim_reload
    (sprintf
       "[%s,%s,%s]"
       (assistant "second answer")
       (user "third question")
       (assistant "third answer"));
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ──── 3 messages trimmed · ~1.2k tokens ──── /trim show · /trim undo
      second answer

    ▌ third question
      third answer
    ───────────── trimmed 3 messages (~1.2k tokens): /trim undo brings them back ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "Esc or n cancels without trimming" =
  let h = connected () in
  run h "/trim";
  H.reply h Trim_points_picker points_json;
  H.enter h;
  H.keys h "n";
  H.show h;
  run h "/trim";
  H.reply h Trim_points_picker points_json;
  H.esc h;
  H.mode h;
  [%expect
    {|
    (Rpc (method_ get_trim_points) (params ()) (tag Trim_points_picker))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ first question
      first answer

    ▌ second question
      second answer

    ▌ third question
      third answer
    ────────────────────────────────────────────────────────────────── cancelled ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ get_trim_points) (params ()) (tag Trim_points_picker))
    editing
    |}]
;;

let trimmed () =
  let h = connected ~height:20 () in
  H.event h (State (state ~extra:[ "trim", trim_json ] ()));
  H.reply
    ~quiet:true
    h
    Trim_reload
    (messages
       [ "third question", "third answer"; "fourth question", "fourth answer" ]);
  h
;;

let%expect_test "/trim show unfolds the trimmed messages, read-only; hide folds"
  =
  let h = trimmed () in
  run h "/trim show";
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    (Rpc (method_ get_trimmed) (params ()) (tag (Trimmed_shown ())))
    |}];
  H.reply
    h
    (Trimmed_shown None)
    (sprintf
       {|{"messages":%s}|}
       (messages
          [ "first question", "first answer"
          ; "second question", "second answer"
          ]));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ┆ not in the model's context

    ┆ first question
    ┆ first answer

    ┆ second question
    ┆ second answer

    ──── 4 messages trimmed · ~38k tokens ──── /trim hide · /trim undo

    ▌ third question
      third answer

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

    ──── 4 messages trimmed · ~38k tokens ──── /trim show · /trim undo

    ▌ third question
      third answer

    ▌ fourth question
      fourth answer
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "Ctrl+Up stops at the divider, not at trimmed messages" =
  let h = trimmed () in
  H.step ~quiet:true h (Resize { width = 80; height = 9 });
  let ctrl_up = { (Key.plain Up) with ctrl = true } in
  let ctrl_down = { (Key.plain Down) with ctrl = true } in
  H.key h ctrl_up;
  H.show h;
  H.key h ctrl_up;
  H.show h;
  H.key h ctrl_up;
  H.show h;
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    ──── 4 messages trimmed · ~38k tokens ──── /trim show · /trim undo

    ▌ third question
      third answer

    ▌ fourth question                                                 ↓ bottom · End
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    ──── 4 messages trimmed · ~38k tokens ──── /trim show · /trim undo

    ▌ third question
      third answer

    ▌ fourth question                                                 ↓ bottom · End
    ───────────────────────────────────────────────────────── no earlier message ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    ──── 4 messages trimmed · ~38k tokens ──── /trim show · /trim undo

    ▌ third question
      third answer

    ▌ fourth question                                                 ↓ bottom · End
    ───────────────────────────────────────────────────────── no earlier message ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01

    ▌ third question
      third answer

    ▌ fourth question
      fourth answer                                                   ↓ bottom · End
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/trim undo brings them back" =
  let h = trimmed () in
  run h "/trim undo";
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))
    (Rpc (method_ untrim) (params ()) (tag (Trimmed Untrim)))
    |}];
  H.reply h (Trimmed `Untrim) {|{"messages":4,"tokens":38000}|};
  H.event h (State (state ()));
  H.reply
    h
    Trim_reload
    (messages
       [ "first question", "first answer"
       ; "second question", "second answer"
       ; "third question", "third answer"
       ]);
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Trim_reload))







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ first question
      first answer

    ▌ second question
      second answer

    ▌ third question
      third answer
    ─────────────────────── 4 messages (~38k tokens) back in the model's context ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "refused while a run is in progress; errors say what to do" =
  let h = connected ~height:8 () in
  (* The notice or error, in its box or on the editor's top rule. *)
  let said () =
    let lines =
      String.split_lines
        (Screen.to_plain ~show_cursor:false (Render.screen h.H.model))
    in
    List.iter lines ~f:(fun line ->
      if
        String.is_prefix line ~prefix:"│"
        || (String.is_prefix line ~prefix:"──"
            && String.exists line ~f:Char.is_alpha)
      then print_endline line)
  in
  H.event h (State (state ~running:true ()));
  run h "/trim";
  said ();
  run h "/trim undo";
  said ();
  H.event h (State (state ()));
  H.esc h;
  run h "/trim show";
  said ();
  run h "/trim later";
  said ();
  H.esc h;
  run h "/trim";
  H.reply h Trim_points_picker {|{"points":[]}|};
  said ();
  H.esc h;
  run h "/trim";
  H.reply h Trim_points_picker points_json;
  H.enter h;
  H.keys h "y";
  H.reply_error
    h
    (Trimmed `Trim)
    "cannot trim while a run is in progress: wait for the run to end or abort \
     it (Esc)";
  said ();
  [%expect
    {|
    │ cannot trim while a run is in progress: wait for the run to end or abort it  │
    │ (Esc)                                                                        │
    │ cannot undo a trim while a run is in progress: wait for the run to end or    │
    │ abort it (Esc)                                                               │
    ──────────────────────────────────── nothing is trimmed: /trim picks a point ───
    │ unknown /trim argument "later": /trim picks a message, /trim after, /trim    │
    │ undo, /trim show or /trim hide                                               │
    (Rpc (method_ get_trim_points) (params ()) (tag Trim_points_picker))
    │ nothing to trim: nothing comes before the context's first message (/compact  │
    │ summarises instead)                                                          │
    (Rpc (method_ get_trim_points) (params ()) (tag Trim_points_picker))
    (Rpc (method_ trim) (params ((before u3))) (tag (Trimmed Trim)))
    │ cannot trim while a run is in progress: wait for the run to end or abort it  │
    │ (Esc)                                                                        │
    |}]
;;

let%expect_test "/trim completes undo, show and hide" =
  let h = connected ~height:10 () in
  H.keys h "/trim ";
  H.show h;
  [%expect
    {|
    ▌ third question
      third answer
    ▸ after  drop everything after a message instead
      undo   bring the latest trim's messages back into the context
      show   unfold the trimmed messages (read-only)
      hide   fold them again
    ────────────────────────────────────────────────────────────────────────────────
    > /trim ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides
    |}]
;;

let%expect_test "a session opened with a trim starts at the divider" =
  let h = H.create ~width:80 ~height:10 () in
  H.step ~quiet:true h Start;
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~extra:[ "trim", trim_json ] ());
  H.reply
    ~quiet:true
    h
    Initial_messages
    (messages [ "third question", "third answer" ]);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ──── 4 messages trimmed · ~38k tokens ──── /trim show · /trim undo

    ▌ third question
      third answer
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
