open! Core
open! Prigh

let call id = { Content.Tool_call.id; name = "ls"; arguments = "{}" }

let start ?(call_id = "p1") agent_id task : Agent_event.t =
  Subagent_start
    { call_id
    ; agent_id
    ; task
    ; name = Subagent_name.of_task task
    ; model = "faux"
    ; tools = [ "ls" ]
    ; worktree = None
    ; forked = false
    ; continued = false
    }
;;

let inside agent_id event : Agent_event.t =
  Subagent { call_id = "p1"; agent_id; event }
;;

let finish ?(is_error = false) agent_id text : Agent_event.t =
  Subagent_end
    { call_id = "p1"
    ; agent_id
    ; usage = Usage.zero
    ; turns = 1
    ; cost_usd = 0.
    ; result = { text; is_error; images = [] }
    }
;;

let show log =
  List.iter (Subagent_log.summaries log) ~f:(fun s ->
    print_s [%sexp (s : Subagent_log.Summary.t)])
;;

let%expect_test "statuses, activity and transcripts of nested subagents" =
  let log = Subagent_log.create () in
  let now = ref 0. in
  let record event =
    now := !now +. 1.;
    Subagent_log.record log ~now:!now event
  in
  record Agent_start;
  record (start "a1" "look around");
  record (inside "a1" Turn_start);
  record (inside "a1" (Message_end (Message.user "look around")));
  record (inside "a1" (Tool_start (call "c1")));
  record (inside "a1" (start ~call_id:"c2" "a1/c2" "dig"));
  record (inside "a1" (inside "a1/c2" (Message_end (Message.user "dig"))));
  record (inside "a1" (inside "a1/c2" (Tool_start (call "c3"))));
  show log;
  [%expect
    {|
    ((id a1) (call_id p1) (parent ()) (task "look around") (name ("Look around"))
     (model faux) (state Running) (started_at 2) (updated_at 5) (ended_at ())
     (turns 1) (tool_calls 1) (current_tool ((ls 5))) (message_count 1)
     (stale false) (result ()))
    ((id a1/c2) (call_id c2) (parent (a1)) (task dig) (name (Dig)) (model faux)
     (state Running) (started_at 6) (updated_at 8) (ended_at ()) (turns 0)
     (tool_calls 1) (current_tool ((ls 8))) (message_count 1) (stale false)
     (result ()))
    |}];
  record (inside "a1" (finish ~is_error:true "a1/c2" "boom"));
  record
    (inside
       "a1"
       (Tool_end
          { call = call "c1"
          ; result =
              { tool_call_id = "c1"
              ; tool_name = "ls"
              ; text = "x"
              ; is_error = false
              ; images = []
              }
          }));
  record (finish "a1" "report");
  show log;
  [%expect
    {|
    ((id a1) (call_id p1) (parent ()) (task "look around") (name ("Look around"))
     (model faux) (state Complete) (started_at 2) (updated_at 11) (ended_at (11))
     (turns 1) (tool_calls 1) (current_tool ()) (message_count 1) (stale false)
     (result (((text report) (is_error false)))))
    ((id a1/c2) (call_id c2) (parent (a1)) (task dig) (name (Dig)) (model faux)
     (state Failed) (started_at 6) (updated_at 9) (ended_at (9)) (turns 0)
     (tool_calls 1) (current_tool ()) (message_count 1) (stale false)
     (result (((text boom) (is_error true)))))
    |}];
  let stale () =
    List.iter (Subagent_log.summaries log) ~f:(fun s ->
      printf "%s stale=%b\n" s.id s.stale)
  in
  (* A subagent's own runs do not count; a new run of the parent makes
     finished ones stale. *)
  record (inside "a1" Agent_start);
  stale ();
  record Agent_start;
  stale ();
  [%expect
    {|
    a1 stale=false
    a1/c2 stale=false
    a1 stale=true
    a1/c2 stale=true
    |}];
  let found key =
    match Subagent_log.find log key with
    | None -> print_endline "none"
    | Some (s, messages) ->
      print_s [%sexp (s.id : string), (messages : Timed_message.t list)]
  in
  found "a1";
  found "c2";
  found "nope";
  [%expect
    {|
    (a1 (((message (User ((text "look around")))) (at (4)))))
    (a1/c2 (((message (User ((text dig)))) (at (7)))))
    none
    |}];
  Subagent_log.load log ~dir:(fun () -> None);
  show log;
  [%expect {| |}]
;;

(* A subagent whose top-level ancestor is gone is ended, nested ones first
   and inside their ancestors' envelopes; finished ones and those under a
   live ancestor are left alone. *)
let%expect_test "orphan_ends: subagents whose top-level ancestor is gone" =
  let log = Subagent_log.create () in
  let record event = Subagent_log.record log ~now:1. event in
  record (start "a1" "background");
  record (inside "a1" (start ~call_id:"c2" "a1/c2" "nested"));
  record (inside "a1" (inside "a1/c2" (start ~call_id:"c3" "a1/c2/c3" "deep")));
  record (start "a2" "alive");
  record (inside "a2" (start ~call_id:"c4" "a2/c4" "nested, alive"));
  record (start ~call_id:"c5" "c5" "synchronous, finished");
  record (finish "c5" "done");
  let ends =
    Subagent_log.orphan_ends
      log
      ~alive:(String.equal "a2")
      ~why:(sprintf "%s ended")
  in
  List.iter ends ~f:(fun event -> print_s [%sexp (event : Agent_event.t)]);
  [%expect
    {|
    (Subagent (call_id p1) (agent_id a1)
     (event
      (Subagent (call_id c2) (agent_id a1/c2)
       (event
        (Subagent_end (call_id c3) (agent_id a1/c2/c3)
         (usage ((input 0) (output 0) (cache_read 0))) (turns 0) (cost_usd 0)
         (result ((text "[interrupted] a1 ended") (is_error true))))))))
    (Subagent (call_id p1) (agent_id a1)
     (event
      (Subagent_end (call_id c2) (agent_id a1/c2)
       (usage ((input 0) (output 0) (cache_read 0))) (turns 0) (cost_usd 0)
       (result ((text "[interrupted] a1 ended") (is_error true))))))
    (Subagent_end (call_id p1) (agent_id a1)
     (usage ((input 0) (output 0) (cache_read 0))) (turns 0) (cost_usd 0)
     (result ((text "[interrupted] a1 ended") (is_error true))))
    |}];
  List.iter ends ~f:record;
  List.iter (Subagent_log.summaries log) ~f:(fun s ->
    printf
      "%s %s %s\n"
      s.id
      (Subagent_log.State.to_string s.state)
      (Option.value_map s.result ~default:"" ~f:(fun r -> r.text)));
  [%expect
    {|
    a1 failed [interrupted] a1 ended
    a1/c2 failed [interrupted] a1 ended
    a1/c2/c3 failed [interrupted] a1 ended
    a2 running
    a2/c4 running
    c5 complete done
    |}];
  print_s
    [%sexp
      (Subagent_log.orphan_ends log ~alive:(fun _ -> false) ~why:Fn.id
       |> List.length
       : int)];
  [%expect {| 2 |}]
;;

(* A subagent continued after running out of turns ([continued]) runs again
   in its entry, which ends again; a reloaded log replays the same. *)
let%expect_test "a continued subagent reopens its entry, also when reloaded" =
  let dir = Filename_unix.temp_dir "subagent-log" "" in
  let log = Subagent_log.create () in
  Subagent_log.load log ~dir:(fun () -> Some dir);
  let now = ref 0. in
  let record event =
    now := !now +. 1.;
    Subagent_log.record log ~now:!now event
  in
  let continued : Agent_event.t =
    Subagent_start
      { call_id = "p1"
      ; agent_id = "a1"
      ; task = "fix it"
      ; name = "Fix it"
      ; model = "faux"
      ; tools = [ "ls" ]
      ; worktree = None
      ; forked = true
      ; continued = true
      }
  in
  let states () =
    List.iter (Subagent_log.summaries log) ~f:(fun s ->
      print_s
        [%sexp
          (s.id : string)
        , (s.state : Subagent_log.State.t)
        , (s.result : Tool_result.t option)])
  in
  record (start "a1" "fix it");
  record (inside "a1" (Message_end (Message.user "fix it")));
  record (finish ~is_error:true "a1" "subagent hit the 1-turn limit");
  states ();
  record continued;
  states ();
  record (finish "a1" "fixed");
  states ();
  [%expect
    {|
    (a1 Failed (((text "subagent hit the 1-turn limit") (is_error true))))
    (a1 Running ())
    (a1 Complete (((text fixed) (is_error false))))
    |}];
  let reloaded = Subagent_log.create () in
  Subagent_log.load reloaded ~dir:(fun () -> Some dir);
  List.iter (Subagent_log.summaries reloaded) ~f:(fun s ->
    print_s [%sexp (s.id : string), (s.state : Subagent_log.State.t)]);
  [%expect {| (a1 Complete) |}];
  ignore (Sys_unix.command (sprintf "rm -rf %s" (Filename.quote dir)) : int)
;;
