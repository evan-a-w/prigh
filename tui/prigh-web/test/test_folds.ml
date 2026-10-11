open! Core
open Prigh_web
module H = Harness

(* Tool outputs fold to a few lines, as the TUI's do: a toggle shows them
   all and folds them again; verbose shows everything. *)

let lines n =
  String.concat (List.init n ~f:(fun i -> sprintf "line %d\n" (i + 1)))
;;

let bash_call =
  {|{"id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"}|}
;;

let start h =
  H.event h {|{"event":"agent_start"}|};
  H.event
    h
    (sprintf
       {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"make\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|});
  H.event h (sprintf {|{"event":"tool_start","call":%s}|} bash_call)
;;

let output h text =
  H.event
    h
    (sprintf
       {|{"event":"tool_output","call_id":"c1","chunk":%s}|}
       (Jsonaf.to_string (`String text)))
;;

let finish h text =
  H.event
    h
    (sprintf
       {|{"event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":%s,"is_error":false}}|}
       bash_call
       (Jsonaf.to_string (`String text)))
;;

let%expect_test "bash: the last lines while it runs, then folded, expanded" =
  let h = H.create () in
  start h;
  output h (lines 3);
  H.text h ~selector:".tool-body";
  [%expect
    {|
    line 1
    line 2
    line 3
    |}];
  output h (lines 12);
  (* No toggle while it streams: the end is what matters. *)
  H.show h ~selector:".output";
  [%expect
    {|
    <div class="live output">
      <div class="earlier"> 10 earlier lines </div>
      <pre> line 8
    line 9
    line 10
    line 11
    line 12 </pre>
    </div>
    |}];
  finish h (lines 30);
  H.text h ~selector:".tool-body";
  [%expect
    {|
    line 1
    line 2
    line 3
    line 4
    line 5
    (Show all 30 lines (22 hidden))
    line 28
    line 29
    line 30
    |}];
  H.show h ~selector:"button.fold";
  [%expect
    {|
    <button type="button" data-fold="c1" aria-expanded="false" class="fold">
      <span class="chev"> </span>
      Show all 30 lines (22 hidden)
    </button>
    |}];
  H.act h (Toggle_fold "c1");
  H.text h ~selector:".tool-body";
  [%expect
    {|
    line 1
    line 2
    line 3
    line 4
    line 5
    line 6
    line 7
    line 8
    line 9
    line 10
    line 11
    line 12
    line 13
    line 14
    line 15
    line 16
    line 17
    line 18
    line 19
    line 20
    line 21
    line 22
    line 23
    line 24
    line 25
    line 26
    line 27
    line 28
    line 29
    line 30
    (Fold to 8 lines)
    |}];
  H.elements h ~selector:".output";
  [%expect {| <div class="expanded output"> |}];
  H.act h (Toggle_fold "c1");
  H.elements h ~selector:".output";
  H.text h ~selector:"button.fold";
  [%expect
    {|
    <div class="output">
    (Show all 30 lines (22 hidden))
    |}]
;;

let%expect_test "verbosity: verbose unfolds everything, quiet hides it" =
  let h = H.create () in
  start h;
  finish h (lines 20);
  H.act h Cycle_verbosity;
  H.elements h ~selector:".chat";
  H.text h ~selector:"button.fold";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    <div class="chat verbosity-verbose">
    (Fold to 8 lines)
    |}];
  (* In verbose the toggle folds one. *)
  H.act h (Toggle_fold "c1");
  H.text h ~selector:"button.fold";
  [%expect {| (Show all 20 lines (12 hidden)) |}];
  (* Changing the level starts from what it shows. *)
  H.act h Cycle_verbosity;
  H.elements h ~selector:".chat";
  H.text h ~selector:"button.fold";
  print_s [%sexp (Set.to_list (H.model h).folds : string list)];
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 4000))
    <div class="chat verbosity-quiet">
    (Show all 20 lines (12 hidden))
    ()
    |}];
  H.act h Cycle_verbosity;
  H.elements h ~selector:".chat";
  H.text h ~selector:"button.fold";
  [%expect
    {|
    (Expire_toast (id 2) (after_ms 4000))
    <div class="chat verbosity-normal">
    (Show all 20 lines (12 hidden))
    |}]
;;

let%expect_test "folds belong to the session" =
  let h = H.create () in
  start h;
  finish h (lines 20);
  H.act h (Toggle_fold "c1");
  H.quiet h (fun () ->
    H.act h New_session;
    H.reply h "new_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  print_s [%sexp (Set.to_list (H.model h).folds : string list)];
  [%expect {| () |}]
;;

let%expect_test "a long stream keeps only its end" =
  let chat =
    Chat_harness.chat
      ~running:true
      [ sprintf {|{"type":"event","event":"tool_start","call":%s}|} bash_call ]
  in
  let chunk = lines 1000 in
  let chat =
    List.fold (List.init 20 ~f:Fn.id) ~init:chat ~f:(fun chat _ ->
      Chat.apply chat (Tool_output { call_id = "c1"; chunk }))
  in
  let tool = Option.value_exn (Chat.tool chat "c1") in
  print_s
    [%message
      ""
        ~kept_under_twice_the_limit:
          (String.length tool.output <= 2 * Chat.live_limit : bool)
        ~starts_a_line:(String.is_prefix tool.output ~prefix:"line " : bool)
        ~lines:(tool.lines : int)];
  Chat_harness.text ~selector:".output" chat;
  [%expect
    {|
    ((kept_under_twice_the_limit true) (starts_a_line true) (lines 20000))
    19995 earlier lines line 996
    line 997
    line 998
    line 999
    line 1000
    |}]
;;

let%expect_test "diffs fold to 8 rows" =
  let old_text = lines 12 in
  let new_text = String.uppercase old_text in
  let call =
    Jsonaf.to_string
      (`Object
          [ "id", `String "e1"
          ; "name", `String "edit"
          ; ( "arguments"
            , `String
                (Jsonaf.to_string
                   (`Object
                       [ "path", `String "a.txt"
                       ; ( "edits"
                         , `Array
                             [ `Object
                                 [ "old_text", `String old_text
                                 ; "new_text", `String new_text
                                 ]
                             ] )
                       ])) )
          ])
  in
  let chat =
    Chat_harness.chat [ sprintf {|{"event":"tool_start","call":%s}|} call ]
  in
  Chat_harness.text ~selector:".diff-view" chat;
  [%expect
    {| - line 1 - line 2 - line 3 - line 4 - line 5 - line 6 - line 7 - line 8 Show all 24 lines (16 hidden) |}];
  Chat_harness.text
    ~fold:(Fold.create ~verbosity:Verbose ~toggled:String.Set.empty)
    ~selector:"button.fold"
    chat;
  [%expect {| Fold to 8 lines |}]
;;

let%expect_test "subagents' calls fold under their own keys" =
  let fold = Fold.scoped Fold.default "a1/c3" in
  print_s [%sexp (Fold.key fold "c1" : string)];
  let toggled =
    Fold.create ~verbosity:Normal ~toggled:(String.Set.singleton "a1/c3/c1")
  in
  print_s
    [%sexp
      (Fold.expanded (Fold.scoped toggled "a1/c3") "c1" : bool)
    , (Fold.expanded toggled "c1" : bool)];
  [%expect
    {|
    a1/c3/c1
    (true false)
    |}]
;;

let%expect_test "a delivered job's output: its last lines, unfolded on demand" =
  let h = H.create () in
  H.event
    h
    (sprintf
       {|{"event":"message_start","message":{"role":"user","text":%s,"origin":{"source":{"kind":"delivery","title":"Job j1 exited 0"}}}}|}
       (Jsonaf.to_string (`String ("[job j1 exited 0] make\n" ^ lines 12))));
  H.text h ~selector:".delivery-section";
  [%expect
    {|
    ↩ job j1 exited 0 make
    (Show all 12 lines (7 hidden))
    line 8
    line 9
    line 10
    line 11
    line 12
    |}];
  H.act h (Toggle_fold "job:j1");
  H.text h ~selector:".delivery-section button.fold";
  [%expect {| (Fold to 5 lines) |}]
;;
