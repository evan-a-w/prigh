open! Core
open Prigh_web
module H = Harness

(* Quiet verbosity: every run of steps between messages (thinking, tool
   calls, reports of background work) is one line saying what it is doing
   or what it did, which opens the steps, each a line that opens its
   detail. *)

let ev fields = sprintf {|{"type":"event",%s}|} fields

(* 2026-10-05 13:00:00Z and [s] seconds after. *)
let at s = sprintf "%.0f" (1791205200000. +. (Float.of_int s *. 1000.))
let json s = Jsonaf.to_string (`String s)

let user ?(source = "") s text =
  ev
    (sprintf
       {|"event":"message_start","message":{"role":"user","text":%s,"at":%s%s}|}
       (json text)
       (at s)
       source)
;;

let message ?(stop = {|{"type":"tool_use"}|}) ?s content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":%s,"usage":{"input":0,"output":0,"cache_read":0},"model":"m"%s}|}
    (String.concat ~sep:"," content)
    stop
    (Option.value_map s ~default:"" ~f:(fun s -> sprintf {|,"at":%s|} (at s)))
;;

let assistant ?stop ?s content =
  ev (sprintf {|"event":"message_end","message":%s|} (message ?stop ?s content))
;;

let streaming content =
  ev
    (sprintf
       {|"event":"message_update","partial":%s,"delta":{"type":"text_delta","text":""}|}
       (message content))
;;

let reply ?(s = 30) text =
  assistant ~stop:{|{"type":"end_turn"}|} ~s [ sprintf {|{"type":"text","text":%s}|} (json text) ]
;;

let thinking text = sprintf {|{"type":"thinking","text":%s}|} (json text)

let call id name args =
  sprintf
    {|{"type":"tool_call","id":"%s","name":"%s","arguments":%s}|}
    id
    name
    (json args)
;;

let call_json id name args =
  sprintf {|{"id":"%s","name":"%s","arguments":%s}|} id name (json args)
;;

let start id name args =
  ev (sprintf {|"event":"tool_start","call":%s|} (call_json id name args))
;;

let finish ?(is_error = false) ?s id name args text =
  ev
    (sprintf
       {|"event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"%s","tool_name":"%s","text":%s,"is_error":%b%s}|}
       (call_json id name args)
       id
       name
       (json text)
       is_error
       (Option.value_map s ~default:"" ~f:(fun s -> sprintf {|,"at":%s|} (at s))))
;;

let bash = {|{"command":"make"}|}
let read = {|{"path":"src/a.ml"}|}
let edit = {|{"path":"src/a.ml","old_text":"a","new_text":"b"}|}

(* Asked to fix a bug: it thinks, builds, reads, edits a file twice, then
   says it is done. *)
let fixed =
  [ user 0 "fix the bug"
  ; assistant ~s:2 [ thinking "Let me build it first."; call "c1" "bash" bash; call "c2" "read" read ]
  ; finish ~s:5 "c1" "bash" bash "Error: a is not b\n[exit code 2]"
  ; finish ~s:5 "c2" "read" read "let a = 1"
  ; assistant ~s:8 [ call "c3" "edit" edit ]
  ; finish ~s:9 "c3" "edit" edit "edited src/a.ml"
  ; assistant ~s:10 [ call "c4" "edit" edit ]
  ; finish ~s:12 "c4" "edit" edit "edited src/a.ml"
  ; reply ~s:14 "Fixed it."
  ]
;;

let quiet ?(toggled = []) () =
  Fold.create ~verbosity:Quiet ~toggled:(String.Set.of_list toggled) ()
;;

let text ?toggled ?selector chat =
  Chat_harness.text ~fold:(quiet ?toggled ()) ?selector chat
;;

let show ?toggled ?selector chat =
  Chat_harness.show ~fold:(quiet ?toggled ()) ?selector chat
;;

let box = "run:think:1:0"

let%expect_test "a run of steps is one line: what it did and how long it took" =
  let chat = Chat_harness.chat fixed in
  text chat;
  [%expect
    {|
    fix the bug
    15:00
    (✕ Ran a command, read a file, edited a file 1 failed 12s)
    Fixed it.
    m 0 in 0 out
    15:00
    |}];
  show ~selector:".run" chat;
  [%expect
    {|
    <div class="error run">
      <button type="button" data-fold="run:think:1:0" aria-expanded="false" class="run-head">
        <span title="A step failed" role="img" aria-label="A step failed" class="failed mark">
          <icon class="cross"> </icon>
        </span>
        <span class="run-label"> Ran a command, read a file, edited a file </span>
        <span class="run-failed"> 1 failed </span>
        <span class="run-time"> 12s </span>
        <span class="chev">  </span>
      </button>
    </div>
    |}];
  (* Normal shows each step as before. *)
  Chat_harness.text ~fold:Fold.default chat;
  [%expect {| |}]
;;

let%expect_test "opened, it lists the steps; a step opens its detail" =
  let chat = Chat_harness.chat fixed in
  text ~toggled:[ box ] ~selector:".run" chat;
  [%expect {| |}];
  text ~toggled:[ box; "c1#open"; "think:1:0" ] ~selector:".run" chat;
  [%expect {| |}]
;;

let%expect_test "messages break runs; a failed step marks its run" =
  let chat =
    Chat_harness.chat
      [ user 0 "look around"
      ; assistant
          ~s:3
          [ call "c1" "ls" {|{"path":"."}|}
          ; sprintf {|{"type":"text","text":"Now the tests."}|}
          ; call "c2" "bash" {|{"command":"dune test"}|}
          ]
      ; finish ~s:2 "c1" "ls" {|{"path":"."}|} "src\ntest"
      ; finish ~s:6 "c2" "bash" {|{"command":"dune test"}|} "ok"
      ; reply ~s:7 "All good."
      ]
  in
  text chat;
  [%expect {| |}]
;;

let%expect_test "while it runs, the line says what it is doing" =
  let chat =
    Chat_harness.chat
      ~running:true
      [ user 0 "fix the bug"
      ; assistant ~s:2 [ call "c1" "bash" bash ]
      ; start "c1" "bash" bash
      ]
  in
  text ~selector:".run" chat;
  [%expect {| |}];
  let chat = Chat_harness.apply chat (finish ~s:3 "c1" "bash" bash "ok") in
  (* Between steps: the model is at work. *)
  text ~selector:".run" chat;
  [%expect {| |}];
  let chat =
    Chat_harness.apply chat (streaming [ call "c2" "edit" {|{"path":"src/b.ml"|} ])
  in
  text ~selector:".run" chat;
  [%expect {| |}];
  let chat = Chat_harness.apply chat (streaming [ thinking "Hmm, b or c" ]) in
  text ~selector:".run" chat;
  [%expect {| |}];
  (* Text ends the run: what it did. *)
  let chat =
    Chat_harness.apply chat (streaming [ sprintf {|{"type":"text","text":"Done"}|} ])
  in
  text chat;
  [%expect {| |}]
;;

let%expect_test "reports of background work join the run; errors stay out" =
  let chat =
    Chat_harness.chat
      [ user 0 "build it in the background"
      ; assistant ~s:1 [ call "c1" "bash" {|{"command":"make","background":true}|} ]
      ; finish ~s:1 "c1" "bash" bash "started job j1: make"
      ; reply ~s:2 "Started."
      ; user
          ~source:{|,"origin":{"source":{"kind":"delivery","title":"Reports"}}|}
          9
          "[job j1 exited 0] make\nbuilt"
      ; assistant ~s:10 [ call "c2" "job_output" {|{"id":"j1"}|} ]
      ; finish ~s:11 "c2" "job_output" {|{"id":"j1"}|} "built"
      ; assistant
          ~stop:{|{"type":"error","message":"overloaded"}|}
          ~s:12
          [ thinking "It built." ]
      ]
  in
  text chat;
  [%expect {| |}];
  text ~toggled:[ "run:report:4" ] ~selector:".run-steps" chat;
  [%expect {| |}]
;;

let%expect_test "every tool the backend has reads in plain words" =
  (* The tools of backend/lib (grep [name = "] and [~name:"] in tools/,
     agent/ and computer/), and the [!command]'s [shell]: a new one needs a
     phrase in frontend/ui/tool_phrase.ml. *)
  let backend =
    [ "answer_session"; "apply_patch"; "ask_session"; "ask_user"; "bash"
    ; "computer"; "custom_instructions"; "edit"; "find"; "goal_complete"
    ; "grep"; "issue"; "job_input"; "job_kill"; "job_output"; "job_status"
    ; "ls"; "memory"; "on_host"; "pull_request"; "read"; "render_ui"
    ; "reply_to_comment"; "repo"; "sessions"; "shell"; "subagent"
    ; "subagent_cancel"; "subagent_continue"; "subagent_message"
    ; "subagent_status"; "update_plan"; "web_fetch"; "web_search"; "worktree"; "write"
    ]
  in
  let missing =
    List.filter backend ~f:(fun name ->
      not (List.mem Prigh_ui.Tool_phrase.known name ~equal:String.equal))
  in
  print_s [%message (missing : string list)];
  let call name args =
    Prigh_ui.Tool_phrase.Step.Call { id = "x"; name; arguments = args }
  in
  List.iter Prigh_ui.Tool_phrase.known ~f:(fun name ->
    let step = call name "{}" in
    printf
      "%-20s %-28s | %s | %s\n"
      name
      (Prigh_ui.Tool_phrase.doing step)
      (Prigh_ui.Tool_phrase.did [ step ])
      (Prigh_ui.Tool_phrase.did [ step; step; step ]));
  [%expect {| |}];
  let open Prigh_ui.Tool_phrase in
  print_endline (doing (call "edit" edit));
  print_endline (doing (call "mcp__linear__create" "{}"));
  print_endline
    (doing
       (call "on_host" {|{"host":"laptop","tool":"read","args":{"path":"x"}}|}));
  print_endline (did [ Thinking; Thinking ]);
  print_endline
    (did
       [ Thinking
       ; call "edit" edit
       ; call "edit" edit
       ; call "write" {|{"path":"b"}|}
       ; call "edit" {|{"path":"c"}|}
       ; call "mcp__x__y" "{}"
       ; Report
       ]);
  List.iter [ 0.4; 14.; 125.; 3600.; 3780. ] ~f:(fun s ->
    print_endline (duration (Time_ns.Span.of_sec s)));
  [%expect {| |}]
;;

(* The app: the box's line toggles it; its open state survives new steps
   arriving; what waits on the user is outside it. *)
let%expect_test "in the app: toggled by its line, open while steps arrive, approvals outside" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h Cycle_verbosity;
    H.act h Cycle_verbosity);
  H.elements h ~selector:".chat";
  [%expect {| |}];
  H.event h {|{"event":"agent_start"}|};
  List.iter
    [ user 0 "fix the bug"
    ; assistant ~s:2 [ call "c1" "bash" bash ]
    ; finish ~s:3 "c1" "bash" bash "ok"
    ]
    ~f:(H.event h);
  H.text h ~selector:".run";
  H.click h ~selector:".run-head";
  H.text h ~selector:".run";
  [%expect {| |}];
  H.event h (assistant ~s:4 [ call "c2" "write" {|{"path":"x.ml","content":"x"}|} ]);
  H.event h (start "c2" "write" {|{"path":"x.ml","content":"x"}|});
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c2","agent_id":null,"name":"write","summary":"x.ml","rules":[]}|};
  H.text h ~selector:".run";
  H.elements h ~selector:".confirm";
  [%expect {| |}]
;;
