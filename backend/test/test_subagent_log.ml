open! Core
open! Prigh

let call id = { Content.Tool_call.id; name = "ls"; arguments = "{}" }

let start ?(call_id = "p1") agent_id task : Agent_event.t =
  Subagent_start { call_id; agent_id; task; model = "faux"; tools = [ "ls" ] }
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
    ; result = { text; is_error }
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
    ((id a1) (call_id p1) (parent ()) (task "look around") (model faux)
     (state Running) (started_at 2) (updated_at 5) (ended_at ()) (turns 1)
     (tool_calls 1) (current_tool ((ls 5))) (message_count 1) (stale false)
     (result ()))
    ((id a1/c2) (call_id c2) (parent (a1)) (task dig) (model faux)
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
              }
          }));
  record (finish "a1" "report");
  show log;
  [%expect
    {|
    ((id a1) (call_id p1) (parent ()) (task "look around") (model faux)
     (state Complete) (started_at 2) (updated_at 11) (ended_at (11)) (turns 1)
     (tool_calls 1) (current_tool ()) (message_count 1) (stale false)
     (result (((text report) (is_error false)))))
    ((id a1/c2) (call_id c2) (parent (a1)) (task dig) (model faux) (state Failed)
     (started_at 6) (updated_at 9) (ended_at (9)) (turns 0) (tool_calls 1)
     (current_tool ()) (message_count 1) (stale false)
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
      print_s [%sexp (s.id : string), (messages : Message.t list)]
  in
  found "a1";
  found "c2";
  found "nope";
  [%expect
    {|
    (a1 ((User ((text "look around")))))
    (a1/c2 ((User ((text dig)))))
    none
    |}];
  Subagent_log.clear log;
  show log;
  [%expect {| |}]
;;
