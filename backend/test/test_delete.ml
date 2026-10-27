open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* The conversation and printers of [Test_trim]: u1 a1 u2 a2[c1] t(c1) a2'
   u3 a3. *)
let shapes = Test_trim.shapes
let print_context = Test_trim.print_context

let describe (names : Test_trim.Names.t) (h : Session.Hidden.t) =
  let kind =
    match h.kind with
    | Trim_before { before } -> "before " ^ names.name before
    | Trim_after { after; through } ->
      sprintf "after %s through %s" (names.name after) (names.name through)
    | Deleted { message } -> "deleted " ^ names.name message
  in
  sprintf
    "%s: %d messages, ~%d tokens%s"
    kind
    h.messages
    h.tokens
    (Option.value_map h.divider ~default:"" ~f:(sprintf ", divider at %d"))
;;

let print_hidden names session =
  match Session.hidden session with
  | [] -> print_endline "nothing hidden"
  | hidden ->
    List.iter hidden ~f:(fun h -> print_endline ("  " ^ describe names h))
;;

let print_result names = function
  | Ok h -> print_endline ("ok: " ^ describe names h)
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let with_session = Test_trim.with_session

let%expect_test "deleting a user message takes out just that message" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_delete session ~message:(names.id "u2"));
  print_context session;
  print_hidden names session;
  (* The user sees what the model does. *)
  print_endline
    (shapes
       (List.map (Session.identified_messages session) ~f:(fun (_, m) ->
          m.Timed_message.message)));
  print_endline
    (shapes
       (List.map
          (Session.hidden_messages
             session
             ~entry:(List.hd_exn (Session.hidden session)).entry)
          ~f:(fun (_, m) -> m.Timed_message.message)));
  (* The latest reply had 500 input tokens; "second" was about 1. *)
  print_s [%sexp (Session.context_tokens session : int)];
  [%expect
    {|
    ok: deleted u2: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) A(one) A[c1] T(c1) A(two) U(third) A(three)
      deleted u2: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) A(one) A[c1] T(c1) A(two) U(third) A(three)
    U(second)
    499
    |}];
  print_result names (Session.append_undelete session);
  print_context session;
  print_hidden names session;
  print_s [%sexp (Session.context_tokens session : int)];
  print_result names (Session.append_undelete session);
  [%expect
    {|
    ok: deleted u2: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    nothing hidden
    500
    error: nothing is deleted: /delete picks a message to delete
    |}]
;;

let%expect_test "a reply goes with its tool results; a result not alone" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_delete session ~message:(names.id "t1"));
  print_result names (Session.append_delete session ~message:"nope");
  print_result names (Session.append_delete session ~message:(names.id "a2"));
  print_context session;
  (* Already out of the context. *)
  print_result names (Session.append_delete session ~message:(names.id "a2"));
  print_result names (Session.append_delete session ~message:(names.id "t1"));
  [%expect
    {|
    error: a tool result goes with its call: delete the assistant message that made the call (/delete lists it)
    error: message nope is not in the model's context: delete one of the messages that /delete lists
    ok: deleted a2: 2 messages, ~3 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A(two) U(third) A(three)
    error: message db21c5355ce98e02 is not in the model's context: delete one of the messages that /delete lists
    error: message 31caf10f4f5e05ff is not in the model's context: delete one of the messages that /delete lists
    |}]
;;

let%expect_test "deletions stack; undo restores the latest, or the one named" =
  with_session
  @@ fun ~dir:_ session names ->
  let first = Session.append_delete session ~message:(names.id "a1") in
  print_result names first;
  print_result names (Session.append_delete session ~message:(names.id "u3"));
  print_result names (Session.append_delete session ~message:(names.id "a3"));
  print_context session;
  print_result names (Session.append_undelete session);
  print_context session;
  print_result
    names
    (Session.append_undelete
       session
       ~entry:(Or_error.ok_exn first).Session.Hidden.entry);
  print_context session;
  print_hidden names session;
  print_result
    names
    (Session.append_undelete
       session
       ~entry:(Or_error.ok_exn first).Session.Hidden.entry);
  [%expect
    {|
    ok: deleted a1: 1 messages, ~0 tokens
    ok: deleted u3: 1 messages, ~1 tokens
    ok: deleted a3: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) U(second) A[c1] T(c1) A(two)
    ok: deleted a3: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) U(second) A[c1] T(c1) A(two) A(three)
    ok: deleted a1: 1 messages, ~0 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) A(three)
      deleted u3: 1 messages, ~1 tokens
    error: the deletion abcda1da20ee532e is not in effect: it was undone, or rewound past
    |}]
;;

let%expect_test "trim after: up to the newest; what comes later stays" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_trim_after session ~after:(names.id "a1"));
  print_context session;
  print_s [%sexp (Session.context_tokens session : int)];
  let (_ : Session.Entry.t) =
    Session.append_message session (Message.user "four")
  in
  print_context session;
  print_hidden names session;
  [%expect
    {|
    ok: after a1 through a3: 6 messages, ~6 tokens, divider at 2
    U(aaaaaaaaaaaa) A(one)
    494
    U(aaaaaaaaaaaa) A(one) U(four)
      after a1 through a3: 6 messages, ~6 tokens, divider at 2
    |}];
  (* Undone, they come back where they were: before "four". *)
  print_result names (Session.append_untrim session);
  print_context session;
  [%expect
    {|
    ok: after a1 through a3: 6 messages, ~6 tokens, divider at 2
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three) U(four)
    |}]
;;

let%expect_test "where a trim after a message is refused" =
  with_session
  @@ fun ~dir:_ session names ->
  List.iter [ "a2"; "a3"; "t1" ] ~f:(fun name ->
    printf "%s: " name;
    print_result names (Session.append_trim_after session ~after:(names.id name)));
  print_result names (Session.append_trim_after session ~after:"nope");
  ignore (Session.append_untrim session : Session.Hidden.t Or_error.t);
  (* After a compaction's summary is fine, at or before it is not. *)
  let (_ : Session.Entry.t) =
    Session.append_compaction session ~summary:"S1" ~kept_from:(names.id "u3")
  in
  print_context session;
  print_result names (Session.append_trim_after session ~after:(names.id "u1"));
  print_result names (Session.append_trim_after session ~after:(names.id "u3"));
  print_context session;
  [%expect
    {|
    a2: error: trimming there would part a tool call from its result: pick the message with its result, or a later one
    a3: error: nothing to trim: that message is the newest in the model's context
    t1: ok: after t1 through a3: 3 messages, ~2 tokens, divider at 5
    error: message nope is not in the model's context: trim after one of the messages that /trim after lists
    S(S1) U(third) A(three)
    error: message 3d2aaca08c36e861 is not in the model's context: trim after one of the messages that /trim after lists
    ok: after u3 through a3: 1 messages, ~1 tokens, divider at 2
    S(S1) U(third)
    |}]
;;

let%expect_test "trims before and after and deletions stack" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_trim session ~before:(names.id "u2")
                      |> Result.map ~f:(fun _ -> List.hd_exn (Session.hidden session)));
  print_result names (Session.append_trim_after session ~after:(names.id "a2'"));
  print_result names (Session.append_delete session ~message:(names.id "a2"));
  print_context session;
  print_hidden names session;
  (* Untrim takes the latest trim, the one after a2'; the deletion stays. *)
  print_result names (Session.append_untrim session);
  print_context session;
  print_result names (Session.append_untrim session);
  print_context session;
  print_result names (Session.append_undelete session);
  print_context session;
  [%expect
    {|
    ok: before u2: 2 messages, ~100 tokens
    ok: after a2' through a3: 2 messages, ~2 tokens, divider at 4
    ok: deleted a2: 2 messages, ~3 tokens
    U(second) A(two)
      before u2: 2 messages, ~100 tokens
      after a2' through a3: 2 messages, ~2 tokens, divider at 2
      deleted a2: 2 messages, ~3 tokens
    ok: after a2' through a3: 2 messages, ~2 tokens, divider at 2
    U(second) A(two) U(third) A(three)
    ok: before u2: 2 messages, ~100 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A(two) U(third) A(three)
    ok: deleted a2: 2 messages, ~3 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    |}]
;;

let%expect_test "undoing a range leaves out what another one hides" =
  with_session
  @@ fun ~dir:_ session names ->
  let deleted = Session.append_delete session ~message:(names.id "u3") in
  print_result names deleted;
  print_result names (Session.append_trim_after session ~after:(names.id "a1"));
  print_context session;
  (* The deletion undone, u3 is still after the trim's point. *)
  print_result
    names
    (Session.append_undelete
       session
       ~entry:(Or_error.ok_exn deleted).Session.Hidden.entry);
  print_context session;
  print_hidden names session;
  print_result names (Session.append_untrim session);
  print_context session;
  [%expect
    {|
    ok: deleted u3: 1 messages, ~1 tokens
    ok: after a1 through a3: 5 messages, ~5 tokens, divider at 2
    U(aaaaaaaaaaaa) A(one)
    ok: deleted u3: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) A(one)
      after a1 through a3: 6 messages, ~6 tokens, divider at 2
    ok: after a1 through a3: 6 messages, ~6 tokens, divider at 2
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    |}]
;;

let%expect_test "deletions and trims persist, and load again" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_delete session ~message:(names.id "a2"));
  print_result names (Session.append_trim_after session ~after:(names.id "u3"));
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_context reloaded;
  print_hidden names reloaded;
  print_result names (Session.append_undelete reloaded);
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_context reloaded;
  (* The entries are appended; the messages stay in the file. *)
  let lines = In_channel.read_lines (Session.path session) in
  List.iter lines ~f:(fun line ->
    List.iter [ "\"Delete\""; "\"Trim_after\""; "\"Restore\"" ] ~f:(fun kind ->
      if String.is_substring line ~substring:kind then print_endline kind));
  [%expect
    {|
    ok: deleted a2: 2 messages, ~3 tokens
    ok: after u3 through a3: 1 messages, ~1 tokens, divider at 5
    U(aaaaaaaaaaaa) A(one) U(second) A(two) U(third)
      deleted a2: 2 messages, ~3 tokens
      after u3 through a3: 1 messages, ~1 tokens, divider at 5
    ok: deleted a2: 2 messages, ~3 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third)
    "Delete"
    "Trim_after"
    "Restore"
    |}]
;;

let%expect_test "an old session's trim and untrim still load" =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir "sessions" in
  let session = Session.create ~dir ~cwd:t.dir () in
  let names = Test_trim.conversation session in
  ignore
    (Session.append_trim session ~before:(names.id "u2") : Session.Trim.t Or_error.t);
  ignore
    (Session.append_trim session ~before:(names.id "u3") : Session.Trim.t Or_error.t);
  ignore (Session.append_untrim session : Session.Hidden.t Or_error.t);
  let payloads =
    List.filter_map (In_channel.read_lines (Session.path session)) ~f:(fun l ->
      List.find [ "\"Trim\""; "\"Untrim\"" ] ~f:(fun kind ->
        String.is_substring l ~substring:kind))
  in
  print_s [%sexp (payloads : string list)];
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_context reloaded;
  print_hidden names reloaded;
  [%expect
    {|
    ("\"Trim\"" "\"Trim\"" "\"Untrim\"")
    U(second) A[c1] T(c1) A(two) U(third) A(three)
      before u2: 2 messages, ~100 tokens
    |}]
;;

let%expect_test "a compaction keeps deleted and trimmed messages out" =
  with_session
  @@ fun ~dir:_ session names ->
  (* u1 is carried over by the compaction (pinned), but deleted. *)
  print_result names (Session.append_delete session ~message:(names.id "u1"));
  let (_ : Session.Entry.t) =
    Session.append_compaction
      session
      ~summary:"S1"
      ~kept_from:(names.id "u3")
      ~pinned:
        [ { id = names.id "u1"; shortened = false }
        ; { id = names.id "u2"; shortened = false }
        ]
  in
  print_context session;
  (* Deleting a carried-over message after the compaction takes it out. *)
  print_result names (Session.append_delete session ~message:(names.id "u2"));
  print_context session;
  print_result names (Session.append_undelete session);
  print_context session;
  (* Undoing the deletion from before the compaction, which did not
     summarise u1: it comes back where it was, ahead of the summary. *)
  print_result names (Session.append_undelete session);
  print_context session;
  [%expect
    {|
    ok: deleted u1: 1 messages, ~100 tokens
    U(second) S(S1) U(third) A(three)
    ok: deleted u2: 1 messages, ~1 tokens
    S(S1) U(third) A(three)
    ok: deleted u2: 1 messages, ~1 tokens
    U(second) S(S1) U(third) A(three)
    ok: deleted u1: 1 messages, ~100 tokens
    U(aaaaaaaaaaaa) U(second) S(S1) U(third) A(three)
    |}]
;;

let%expect_test "masked observations can be deleted" =
  with_session
  @@ fun ~dir:_ session names ->
  ignore (Session.append_masked session ~through:(names.id "t1") : Session.Entry.t);
  print_result names (Session.append_delete session ~message:(names.id "a2"));
  print_context session;
  print_result names (Session.append_undelete session);
  print_context session;
  [%expect
    {|
    ok: deleted a2: 2 messages, ~3 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A(two) U(third) A(three)
    ok: deleted a2: 2 messages, ~3 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    |}]
;;

let%expect_test "rewinds and forks keep what is hidden while its message stays" =
  with_session
  @@ fun ~dir session names ->
  print_result names (Session.append_delete session ~message:(names.id "u2"));
  print_result names (Session.append_trim_after session ~after:(names.id "a2'"));
  let (_ : Session.Entry.t) =
    Session.append_message session (Message.user "four")
  in
  (* To a3: both stay. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "a3"));
  print_context session;
  print_hidden names session;
  (* To t1: the trim's message is no longer on the path. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "t1"));
  print_context session;
  print_hidden names session;
  [%expect
    {|
    ok: deleted u2: 1 messages, ~1 tokens
    ok: after a2' through a3: 2 messages, ~2 tokens, divider at 5
    U(aaaaaaaaaaaa) A(one) A[c1] T(c1) A(two)
      deleted u2: 1 messages, ~1 tokens
      after a2' through a3: 2 messages, ~2 tokens, divider at 5
    U(aaaaaaaaaaaa) A(one) A[c1] T(c1)
      deleted u2: 1 messages, ~1 tokens
    |}];
  (* A fork of this branch at t1 keeps the deletion. *)
  let fork = Or_error.ok_exn (Session.fork session ~at:(names.id "t1") ~dir) in
  print_context fork;
  print_hidden names fork;
  let reloaded = Or_error.ok_exn (Session.load (Session.path fork)) in
  print_context reloaded;
  [%expect {|
    U(aaaaaaaaaaaa) A(one) A[c1] T(c1)
      deleted ?: 1 messages, ~1 tokens
    U(aaaaaaaaaaaa) A(one) A[c1] T(c1)
    |}];
  (* To u1: the deleted message is gone too; rewinding back to where the
     ranges were made does not bring them back, as they were not in effect
     at u1. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "u1"));
  print_context session;
  print_hidden names session;
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "a3"));
  print_hidden names session;
  [%expect {|
    U(aaaaaaaaaaaa)
    nothing hidden
    nothing hidden
    |}]
;;

let%expect_test "message points: what deleting or trimming after each drops" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_delete session ~message:(names.id "u3"));
  List.iter
    (Message_point.create (Session.identified_messages session))
    ~f:(fun (p : Message_point.t) ->
      let effect = function
        | None -> "-"
        | Some (e : Message_point.Effect.t) ->
          sprintf "%d/~%d" e.messages e.tokens
      in
      printf
        "%d %s %s %s delete=%s trim_after=%s\n"
        p.index
        (names.name p.message)
        (Sexp.to_string [%sexp (p.role : Message_point.Role.t)])
        p.text
        (effect p.delete)
        (effect p.trim_after));
  [%expect
    {|
    ok: deleted u3: 1 messages, ~1 tokens
    0 u1 User aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa delete=1/~100 trim_after=6/~5
    1 a1 Assistant one delete=1/~0 trim_after=5/~5
    2 u2 User second delete=1/~1 trim_after=4/~4
    3 a2 Assistant looking delete=2/~3 trim_after=-
    4 t1 Tool_result bash delete=- trim_after=2/~1
    5 a2' Assistant two delete=1/~0 trim_after=1/~1
    6 a3 Assistant three delete=1/~1 trim_after=-
    |}]
;;

(* Agents: what the model receives. *)

let with_agent = Test_trim.with_agent
let run = Test_trim.run

let entry agent ~f =
  List.find_map_exn
    (Session.active_path (Agent.session agent))
    ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message m when f m -> Some e.id
      | _ -> None)
;;

let user_entry = Test_trim.user_entry

let reply_entry agent text =
  entry agent ~f:(function
    | Assistant a -> String.equal (Message.Assistant.text a) text
    | User _ | Tool_result _ -> false)
;;

let print_state agent =
  let state = Agent.state agent in
  printf
    "state: messages=%d context_tokens=%d hidden=[%s]\n"
    state.message_count
    state.context_tokens
    (String.concat
       ~sep:"; "
       (List.map state.hidden ~f:(fun (h : Session.Hidden.t) ->
          sprintf
            "%s %d"
            (match h.kind with
             | Trim_before _ -> "before"
             | Trim_after _ -> "after"
             | Deleted _ -> "deleted")
            h.messages)))
;;

let%expect_test "the next request leaves out what was deleted or trimmed" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"true"}|} ()
    ; Reply.text "r1"
    ; Reply.text "r2"
    ; Reply.text "r3"
    ; Reply.text "r4"
    ; Reply.text "r5"
    ; Reply.text "r6"
    ]
  @@ fun agent ->
  run agent "q1";
  run agent "q2";
  [%expect
    {|
    request: U(q1)
    request: U(q1) A[c1] T(c1)
    request: U(q1) A[c1] T(c1) A(r1) U(q2)
    |}];
  print_state agent;
  (* The first user message deleted: the request starts with the reply
     (the provider puts a placeholder user turn in front). *)
  ignore
    (Or_error.ok_exn (Agent.delete_message agent ~message:(user_entry agent "q1"))
     : Session.Hidden.t);
  print_state agent;
  run agent "q3";
  ignore (Or_error.ok_exn (Agent.undelete agent) : Session.Hidden.t);
  [%expect
    {|
    state: messages=6 context_tokens=10 hidden=[]
    state: messages=5 context_tokens=10 hidden=[deleted 1]
    request: A[c1] T(c1) A(r1) U(q2) A(r2) U(q3)
    |}];
  (* The reply with the call goes with its result. *)
  let call =
    entry agent ~f:(function
      | Assistant a -> not (List.is_empty (Message.Assistant.tool_calls a))
      | User _ | Tool_result _ -> false)
  in
  ignore
    (Or_error.ok_exn (Agent.delete_message agent ~message:call) : Session.Hidden.t);
  run agent "q4";
  [%expect {| request: U(q1) A(r1) U(q2) A(r2) U(q3) A(r3) U(q4) |}];
  (* Trimmed after r1: q5 comes after the divider and stays. *)
  ignore
    (Or_error.ok_exn (Agent.trim_after agent ~after:(reply_entry agent "r1"))
     : Session.Hidden.t);
  print_state agent;
  run agent "q5";
  run agent "q6";
  print_state agent;
  [%expect
    {|
    state: messages=2 context_tokens=10 hidden=[deleted 2; after 6]
    request: U(q1) A(r1) U(q5)
    request: U(q1) A(r1) U(q5) A(r5) U(q6)
    state: messages=6 context_tokens=10 hidden=[deleted 2; after 6]
    |}];
  ignore (Or_error.ok_exn (Agent.untrim agent) : Session.Hidden.t);
  ignore (Or_error.ok_exn (Agent.undelete agent) : Session.Hidden.t);
  run agent "q7";
  [%expect
    {| request: U(q1) A[c1] T(c1) A(r1) U(q2) A(r2) U(q3) A(r3) U(q4) A(r4) U(q5) A(r5) U(q6) A(r6) U(q7) |}]
;;

let%expect_test "deleting and trimming after are refused while a run is in progress" =
  with_agent
    [ Reply.text "r1"
    ; Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.2"}|}
        ()
    ; Reply.text "r2"
    ]
  @@ fun agent ->
  run agent "q1";
  let q1 = user_entry agent "q1" in
  Or_error.ok_exn (Agent.prompt agent "q2");
  let show r = print_s [%sexp (Result.map r ~f:(fun _ -> ()) : unit Or_error.t)] in
  show (Agent.delete_message agent ~message:q1);
  show (Agent.undelete agent);
  show (Agent.trim_after agent ~after:q1);
  Agent.wait_idle agent;
  [%expect
    {|
    request: U(q1)
    request: U(q1) A(r1) U(q2)
    (Error
     "cannot delete a message while a run is in progress: wait for the run to end or abort it (Esc)")
    (Error
     "cannot undo a deletion while a run is in progress: wait for the run to end or abort it (Esc)")
    (Error
     "cannot trim while a run is in progress: wait for the run to end or abort it (Esc)")
    request: U(q1) A(r1) U(q2) A[c1] T(c1)
    |}]
;;

let%expect_test "/compact after a deletion: the deleted message is not carried over" =
  let long = String.make 40_000 'x' in
  with_agent
    [ Reply.text "r1"
    ; Reply.text ("r2" ^ long)
    ; Reply.text "r3"
    ; Reply.text "SUMMARY"
    ; Reply.text "r4"
    ; Reply.text "r5"
    ]
  @@ fun agent ->
  run agent "q1";
  run agent "q2";
  run agent "q3";
  ignore
    (Or_error.ok_exn (Agent.delete_message agent ~message:(user_entry agent "q1"))
     : Session.Hidden.t);
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  run agent "q4";
  (* Undone after the compaction, which did not summarise it, q1 comes back
     ahead of the summary. *)
  ignore (Or_error.ok_exn (Agent.undelete agent) : Session.Hidden.t);
  print_state agent;
  run agent "q5";
  [%expect {|
    request: U(q1)
    request: U(q1) A(r1) U(q2)
    request: U(q1) A(r1) U(q2) A(r2xxxxxxxxxx) U(q3)
    summarise: A(r1) U(q2) A(r2xxxxxxxxxx)
    (Ok SUMMARY)
    request: U(q2) S(SUMMARY) U(q3) A(r3) U(q4)
    state: messages=7 context_tokens=10 hidden=[]
    request: U(q1) U(q2) S(SUMMARY) U(q3) A(r3) U(q4) A(r4) U(q5)
    |}]
;;
