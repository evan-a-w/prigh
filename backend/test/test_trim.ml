open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let shape (m : Message.t) =
  match m with
  | User u when Session.is_summary_message m ->
    sprintf "S(%s)" (List.nth_exn (String.split_lines u.text) 1)
  | User u -> sprintf "U(%s)" (String.prefix (Message.User.body u) 12)
  | Assistant a ->
    (match Message.Assistant.tool_calls a with
     | [] -> sprintf "A(%s)" (String.prefix (Message.Assistant.text a) 12)
     | calls ->
       sprintf
         "A[%s]"
         (String.concat ~sep:"," (List.map calls ~f:(fun c -> c.id))))
  | Tool_result r -> sprintf "T(%s)" r.tool_call_id
;;

let shapes messages = String.concat ~sep:" " (List.map messages ~f:shape)
let print_context session = print_endline (shapes (Session.messages session))

let assistant ?(calls = []) ?(input = 100) text : Message.t =
  Assistant
    { content =
        Content.Text text
        :: List.map calls ~f:(fun id ->
          Content.Tool_call { id; name = "bash"; arguments = "{}" })
    ; stop_reason = (if List.is_empty calls then End_turn else Tool_use)
    ; usage = { input; output = 10; cache_read = 0 }
    ; model = "m"
    }
;;

let result id : Message.t =
  Tool_result
    { tool_call_id = id
    ; tool_name = "bash"
    ; text = "ok"
    ; is_error = false
    ; images = []
    }
;;

module Names = struct
  type t =
    { id : string -> string
    ; name : string -> string
    }
end

(* u1 a1 u2 a2[c1] t(c1) a2' u3 a3, naming each message's entry. *)
let conversation session =
  let ids = String.Table.create () in
  let names = String.Table.create () in
  let add name m =
    let (e : Session.Entry.t) = Session.append_message session m in
    Hashtbl.set ids ~key:name ~data:e.id;
    Hashtbl.set names ~key:e.id ~data:name
  in
  add "u1" (Message.user (String.make 400 'a'));
  add "a1" (assistant ~input:200 "one");
  add "u2" (Message.user "second");
  add "a2" (assistant ~input:300 ~calls:[ "c1" ] "looking");
  add "t1" (result "c1");
  add "a2'" (assistant ~input:400 "two");
  add "u3" (Message.user "third");
  add "a3" (assistant ~input:500 "three");
  { Names.id = Hashtbl.find_exn ids
  ; name = (fun id -> Option.value (Hashtbl.find names id) ~default:"?")
  }
;;

let describe (names : Names.t) (trim : Session.Trim.t) =
  sprintf
    "before %s: %d messages, ~%d tokens"
    (names.name trim.before)
    trim.messages
    trim.tokens
;;

let print_trim names session =
  print_endline
    (Option.value_map
       (Session.trim session)
       ~default:"no trim"
       ~f:(describe names))
;;

let print_result names result =
  match result with
  | Ok trim -> print_endline ("ok: " ^ describe names trim)
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let untrim session =
  Result.map (Session.append_untrim session) ~f:(fun (h : Session.Hidden.t) ->
    { Session.Trim.before =
        (match h.kind with
         | Trim_before { before } -> before
         | Trim_after { after; _ } -> after
         | Deleted { message } -> message)
    ; messages = h.messages
    ; tokens = h.tokens
    ; at = h.at
    })
;;

let with_session f =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir "sessions" in
  let session = Session.create ~dir ~cwd:t.dir () in
  f ~dir session (conversation session)
;;

let%expect_test "a trim drops what comes before its message from the context" =
  with_session
  @@ fun ~dir:_ session names ->
  List.iter
    (Trim_point.create (Session.identified_messages session))
    ~f:(fun (p : Trim_point.t) ->
      printf
        "point %d %s (%s): %d messages, ~%d tokens\n"
        p.index
        (names.name p.before)
        p.text
        p.messages
        p.tokens);
  print_s [%sexp (Session.context_tokens session : int)];
  [%expect
    {|
    point 1 a1 (one): 1 messages, ~100 tokens
    point 2 u2 (second): 2 messages, ~100 tokens
    point 3 a2 (looking): 3 messages, ~101 tokens
    point 5 a2' (two): 5 messages, ~104 tokens
    point 6 u3 (third): 6 messages, ~104 tokens
    point 7 a3 (three): 7 messages, ~105 tokens
    500
    |}];
  print_result names (Session.append_trim session ~before:(names.id "u3"));
  print_context session;
  print_trim names session;
  print_endline
    (shapes
       (List.map (Session.trimmed_messages session) ~f:(fun (_, m) ->
          m.Timed_message.message)));
  (* The latest reply's request had 500 input tokens, about 107 of them
     are gone. *)
  print_s [%sexp (Session.context_tokens session : int)];
  [%expect
    {|
    ok: before u3: 6 messages, ~104 tokens
    U(third) A(three)
    before u3: 6 messages, ~104 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two)
    396
    |}];
  print_result names (untrim session);
  print_context session;
  print_trim names session;
  print_s [%sexp (Session.context_tokens session : int)];
  print_result names (untrim session);
  [%expect
    {|
    ok: before u3: 6 messages, ~104 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    no trim
    500
    error: nothing is trimmed: /trim picks a point
    |}]
;;

let%expect_test "before a user message or a reply, never the first or a result" =
  with_session
  @@ fun ~dir:_ session names ->
  List.iter [ "u1"; "t1" ] ~f:(fun name ->
    printf "%s: " name;
    print_result names (Session.append_trim session ~before:(names.id name)));
  print_result names (Session.append_trim session ~before:"nope");
  print_trim names session;
  [%expect
    {|
    u1: error: nothing to trim: that message is the first one in the model's context
    t1: error: a tool result goes with its call: trim before the reply that made the call (/trim lists it)
    error: message nope is not in the model's context: trim before one of the messages that /trim lists
    no trim
    |}];
  (* A reply: the context starts with it. *)
  List.iter [ "a1"; "a2"; "a2'" ] ~f:(fun name ->
    printf "%s: " name;
    print_result names (Session.append_trim session ~before:(names.id name));
    print_context session;
    print_result names (untrim session));
  [%expect
    {|
    a1: ok: before a1: 1 messages, ~100 tokens
    A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    ok: before a1: 1 messages, ~100 tokens
    a2: ok: before a2: 3 messages, ~101 tokens
    A[c1] T(c1) A(two) U(third) A(three)
    ok: before a2: 3 messages, ~101 tokens
    a2': ok: before a2': 5 messages, ~104 tokens
    A(two) U(third) A(three)
    ok: before a2': 5 messages, ~104 tokens
    |}];
  (* A user message between a call and its result (a steer recorded
     there), or a reply with a call answered after it, is no place to
     cut. *)
  let (_ : Session.Entry.t) =
    Session.append_message session (assistant ~calls:[ "c2" ] "again")
  in
  let steer = Session.append_message session (Message.user "steer") in
  let late = Session.append_message session (assistant "late") in
  let (_ : Session.Entry.t) = Session.append_message session (result "c2") in
  print_result names (Session.append_trim session ~before:steer.id);
  print_result names (Session.append_trim session ~before:late.id);
  [%expect
    {|
    error: trimming there would part a tool call from its result: pick an earlier or later message
    error: trimming there would part a tool call from its result: pick an earlier or later message
    |}]
;;

let%expect_test "trims stack: undo cancels the latest" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_trim session ~before:(names.id "u2"));
  print_result names (Session.append_trim session ~before:(names.id "u3"));
  print_context session;
  print_trim names session;
  print_result names (untrim session);
  print_context session;
  print_trim names session;
  [%expect
    {|
    ok: before u2: 2 messages, ~100 tokens
    ok: before u3: 4 messages, ~4 tokens
    U(third) A(three)
    before u3: 6 messages, ~104 tokens
    ok: before u3: 4 messages, ~4 tokens
    U(second) A[c1] T(c1) A(two) U(third) A(three)
    before u2: 2 messages, ~100 tokens
    |}]
;;

let%expect_test "a trim persists, and loads again" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_trim session ~before:(names.id "u3"));
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_context reloaded;
  print_trim names reloaded;
  print_result names (untrim reloaded);
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_context reloaded;
  [%expect
    {|
    ok: before u3: 6 messages, ~104 tokens
    U(third) A(three)
    before u3: 6 messages, ~104 tokens
    ok: before u3: 6 messages, ~104 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    |}]
;;

let%expect_test "a compaction after a trim summarises only what is after it" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_trim session ~before:(names.id "u2"));
  (* [Compaction.compact] plans on [Session.messages]: the summary stands
     for u2..a2', and keeps u3 on. *)
  let (_ : Session.Entry.t) =
    Session.append_compaction session ~summary:"S1" ~kept_from:(names.id "u3")
  in
  print_context session;
  print_trim names session;
  (* Undoing the trim puts u1 a1 back ahead of the summary. *)
  print_result names (untrim session);
  print_context session;
  [%expect
    {|
    ok: before u2: 2 messages, ~100 tokens
    S(S1) U(third) A(three)
    before u2: 2 messages, ~100 tokens
    ok: before u2: 2 messages, ~100 tokens
    U(aaaaaaaaaaaa) A(one) S(S1) U(third) A(three)
    |}]
;;

let%expect_test "a trim after a compaction cuts into its kept part" =
  with_session
  @@ fun ~dir:_ session names ->
  let (_ : Session.Entry.t) =
    Session.append_compaction
      session
      ~summary:"S1"
      ~kept_from:(names.id "u2")
      ~pinned:[ { id = names.id "u1"; shortened = false } ]
  in
  print_context session;
  (* The pinned message and the summary go too. *)
  print_result names (Session.append_trim session ~before:(names.id "u3"));
  print_context session;
  print_endline
    (shapes
       (List.map (Session.trimmed_messages session) ~f:(fun (_, m) ->
          m.Timed_message.message)));
  print_result names (untrim session);
  print_context session;
  [%expect
    {|
    U(aaaaaaaaaaaa) S(S1) U(second) A[c1] T(c1) A(two) U(third) A(three)
    ok: before u3: 6 messages, ~113 tokens
    U(third) A(three)
    U(aaaaaaaaaaaa) S(S1) U(second) A[c1] T(c1) A(two)
    ok: before u3: 6 messages, ~113 tokens
    U(aaaaaaaaaaaa) S(S1) U(second) A[c1] T(c1) A(two) U(third) A(three)
    |}]
;;

let%expect_test "rewinds keep the trim while its message stays" =
  with_session
  @@ fun ~dir:_ session names ->
  print_result names (Session.append_trim session ~before:(names.id "u3"));
  let (_ : Session.Entry.t) =
    Session.append_message session (Message.user "four")
  in
  (* To the trim's message itself: still trimmed. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "u3"));
  print_context session;
  print_trim names session;
  (* To before it: the trim goes. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "u2"));
  print_context session;
  print_trim names session;
  [%expect
    {|
    ok: before u3: 6 messages, ~104 tokens
    U(third)
    before u3: 6 messages, ~104 tokens
    U(aaaaaaaaaaaa) A(one) U(second)
    no trim
    |}];
  (* Rewinding back to where the trim was undone does not bring it
     back. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "a3"));
  print_result names (Session.append_trim session ~before:(names.id "u2"));
  let after_trim = Option.value_exn (Session.head session) in
  print_result names (untrim session);
  let (_ : Session.Entry.t) =
    Session.append_message session (Message.user "five")
  in
  Or_error.ok_exn (Session.rewind session ~to_:after_trim);
  print_context session;
  print_trim names session;
  [%expect
    {|
    ok: before u2: 2 messages, ~100 tokens
    ok: before u2: 2 messages, ~100 tokens
    U(aaaaaaaaaaaa) A(one) U(second) A[c1] T(c1) A(two) U(third) A(three)
    no trim
    |}]
;;

let%expect_test "forks and clones copy the trim" =
  with_session
  @@ fun ~dir session names ->
  print_result names (Session.append_trim session ~before:(names.id "u2"));
  let clone = Or_error.ok_exn (Session.fork session ~dir) in
  print_context clone;
  print_endline
    (Option.value_map (Session.trim clone) ~default:"no trim" ~f:(fun t ->
       sprintf "%d messages, ~%d tokens" t.messages t.tokens));
  (* A fork at a message after the trim's, before the trim was made. *)
  let fork = Or_error.ok_exn (Session.fork session ~at:(names.id "t1") ~dir) in
  print_context fork;
  print_endline
    (Option.value_map (Session.trim fork) ~default:"no trim" ~f:(fun t ->
       sprintf "%d messages, ~%d tokens" t.messages t.tokens));
  let reloaded = Or_error.ok_exn (Session.load (Session.path fork)) in
  print_context reloaded;
  [%expect
    {|
    ok: before u2: 2 messages, ~100 tokens
    U(second) A[c1] T(c1) A(two) U(third) A(three)
    2 messages, ~100 tokens
    U(second) A[c1] T(c1)
    2 messages, ~100 tokens
    U(second) A[c1] T(c1)
    |}]
;;

let with_agent replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let on_request (r : Provider.Request.t) =
    let is_ask (m : Message.t) =
      match m with
      | User u -> String.is_prefix u.text ~prefix:"[prigh: compaction]"
      | Assistant _ | Tool_result _ -> false
    in
    match r.system, List.last r.messages with
    | Some system, _ when String.is_prefix system ~prefix:"You summarise" ->
      let transcript =
        match r.messages with
        | [ User u ] -> u.text
        | _ -> "?"
      in
      printf
        "summarise (transcript): %s\n"
        (String.concat
           ~sep:" | "
           (List.filter_map (String.split_lines transcript) ~f:(fun l ->
              Option.some_if
                (String.is_prefix l ~prefix:"q"
                 || String.is_prefix l ~prefix:"r")
                (String.prefix l 12))))
    | _, Some last when is_ask last ->
      printf
        "summarise: %s\n"
        (shapes (List.drop_last_exn (Provider.Request.prepare r).messages))
    | _ ->
      (* What the model receives. *)
      printf "request: %s\n" (shapes (Provider.Request.prepare r).messages)
  in
  let provider = Faux_provider.create ~on_request replies in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  f agent
;;

let run agent text =
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let user_entry agent text =
  List.find_map_exn
    (Session.active_path (Agent.session agent))
    ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message (User u) when String.equal (Message.User.body u) text ->
        Some e.id
      | _ -> None)
;;

let print_state agent =
  let state = Agent.state agent in
  printf
    "state: messages=%d context_tokens=%d trim=%s\n"
    state.message_count
    state.context_tokens
    (Option.value_map state.trim ~default:"none" ~f:(fun t ->
       sprintf "%d messages, ~%d tokens" t.messages t.tokens))
;;

let%expect_test "the next request starts at the trim; undo brings it back" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"true"}|} ()
    ; Reply.text "r1"
    ; Reply.text "r2"
    ; Reply.text "r3"
    ; Reply.text "r4"
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
  let trimmed = Agent.trim agent ~before:(user_entry agent "q2") in
  print_s [%sexp (Result.map trimmed ~f:(fun t -> t.messages) : int Or_error.t)];
  print_state agent;
  run agent "q3";
  [%expect
    {|
    state: messages=6 context_tokens=10 trim=none
    (Ok 4)
    state: messages=2 context_tokens=5 trim=4 messages, ~5 tokens
    request: U(q2) A(r2) U(q3)
    |}];
  print_s
    [%sexp
      (Agent.untrim agent |> Result.map ~f:(fun t -> t.messages)
       : int Or_error.t)];
  print_state agent;
  run agent "q4";
  [%expect
    {|
    (Ok 4)
    state: messages=8 context_tokens=15 trim=none
    request: U(q1) A[c1] T(c1) A(r1) U(q2) A(r2) U(q3) A(r3) U(q4)
    |}]
;;

let%expect_test "trimming is refused while a run is in progress" =
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
  Or_error.ok_exn (Agent.prompt agent "q2");
  print_s
    [%sexp
      (Agent.trim agent ~before:(user_entry agent "q2")
       : Session.Trim.t Or_error.t)];
  print_s [%sexp (Agent.untrim agent : Session.Hidden.t Or_error.t)];
  Agent.wait_idle agent;
  [%expect
    {|
    request: U(q1)
    request: U(q1) A(r1) U(q2)
    (Error
     "cannot trim while a run is in progress: wait for the run to end or abort it (Esc)")
    (Error
     "cannot undo a trim while a run is in progress: wait for the run to end or abort it (Esc)")
    request: U(q1) A(r1) U(q2) A[c1] T(c1)
    |}]
;;

let%expect_test "/compact after a trim summarises only what is after it" =
  let long = String.make 40_000 'x' in
  with_agent
    [ Reply.text "r1"
    ; Reply.text ("r2" ^ long)
    ; Reply.text "r3"
    ; Reply.text "SUMMARY"
    ; Reply.text "r4"
    ]
  @@ fun agent ->
  run agent "q1";
  run agent "q2";
  run agent "q3";
  ignore
    (Or_error.ok_exn (Agent.trim agent ~before:(user_entry agent "q2"))
     : Session.Trim.t);
  (* The user's earlier messages are carried over, but only those after the
     trim: q1 stays out. *)
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  run agent "q4";
  print_state agent;
  [%expect
    {|
    request: U(q1)
    request: U(q1) A(r1) U(q2)
    request: U(q1) A(r1) U(q2) A(r2xxxxxxxxxx) U(q3)
    summarise: U(q2) A(r2xxxxxxxxxx)
    (Ok SUMMARY)
    request: U(q2) S(SUMMARY) U(q3) A(r3) U(q4)
    state: messages=6 context_tokens=10 trim=2 messages, ~0 tokens
    |}];
  (* Undoing the trim puts q1 back ahead of the carried-over q2 and the
     summary. *)
  ignore (Or_error.ok_exn (Agent.untrim agent) : Session.Hidden.t);
  run agent "q5";
  [%expect
    {| request: U(q1) A(r1) U(q2) S(SUMMARY) U(q3) A(r3) U(q4) A(r4) U(q5) |}]
;;

let print_points agent =
  List.iter
    (Trim_point.create (Session.identified_messages (Agent.session agent)))
    ~f:(fun (p : Trim_point.t) ->
      printf
        "point %d (%s): %d messages, ~%d tokens\n"
        p.index
        (String.prefix p.text 12)
        p.messages
        p.tokens)
;;

let%expect_test "a trim after a compaction: its carried-over messages" =
  let long = String.make 40_000 'x' in
  with_agent
    [ Reply.text "r1"
    ; Reply.text ("r2" ^ long)
    ; Reply.text "r3"
    ; Reply.text "SUMMARY"
    ; Reply.text "r4"
    ; Reply.text "r5"
    ; Reply.text "r6"
    ]
  @@ fun agent ->
  run agent ("q1" ^ String.make 12_000 'p');
  run agent "q2";
  run agent "q3";
  print_s [%sexp (Agent.compact agent : string Or_error.t)];
  run agent "q4";
  [%expect
    {|
    request: U(q1pppppppppp)
    request: U(q1pppppppppp) A(r1) U(q2)
    request: U(q1pppppppppp) A(r1) U(q2) A(r2xxxxxxxxxx) U(q3)
    summarise: U(q1pppppppppp) A(r1) U(q2) A(r2xxxxxxxxxx)
    (Ok SUMMARY)
    request: U(q1pppppppppp) U(q2) S(SUMMARY) U(q3) A(r3) U(q4)
    |}];
  (* q1 is carried over shortened (~500 tokens, not its ~3000): a trim
     point's tokens, and a trim's, count what the model is sent. *)
  print_points agent;
  [%expect
    {|
    point 1 (q2): 1 messages, ~507 tokens
    point 3 (q3): 3 messages, ~517 tokens
    point 4 (r3): 4 messages, ~517 tokens
    point 5 (q4): 5 messages, ~517 tokens
    point 6 (r4): 6 messages, ~517 tokens
    |}];
  (* A trim at a carried-over message drops the ones before it. *)
  print_s
    [%sexp
      (Agent.trim agent ~before:(user_entry agent "q2")
       |> Result.map ~f:(fun t -> t.messages, t.tokens)
       : (int * int) Or_error.t)];
  run agent "q5";
  ignore (Or_error.ok_exn (Agent.untrim agent) : Session.Hidden.t);
  (* One in the compaction's kept part drops them all, and the summary. *)
  print_s
    [%sexp
      (Agent.trim agent ~before:(user_entry agent "q3")
       |> Result.map ~f:(fun t -> t.messages, t.tokens)
       : (int * int) Or_error.t)];
  run agent "q6";
  print_state agent;
  [%expect
    {|
    (Ok (1 507))
    request: U(q2) S(SUMMARY) U(q3) A(r3) U(q4) A(r4) U(q5)
    (Ok (3 517))
    request: U(q3) A(r3) U(q4) A(r4) U(q5) A(r5) U(q6)
    state: messages=8 context_tokens=10 trim=3 messages, ~517 tokens
    |}]
;;

let reply_entry agent text =
  List.find_map_exn
    (Session.active_path (Agent.session agent))
    ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message (Assistant a)
        when String.equal (Message.Assistant.text a) text
             || List.exists (Message.Assistant.tool_calls a) ~f:(fun c ->
               String.equal c.id text) -> Some e.id
      | _ -> None)
;;

let print_trimmed agent ~before =
  print_s
    [%sexp
      (Agent.trim agent ~before |> Result.map ~f:(fun t -> t.messages, t.tokens)
       : (int * int) Or_error.t)]
;;

let%expect_test "a trim before a reply: the model gets a user turn ahead of it" =
  with_agent
    [ Reply.text "r1"
    ; Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"true"}|} ()
    ; Reply.text "r2"
    ; Reply.text "r3"
    ; Reply.text "r4"
    ; Reply.text "r5"
    ]
  @@ fun agent ->
  run agent "q1";
  run agent "q2";
  [%expect
    {|
    request: U(q1)
    request: U(q1) A(r1) U(q2)
    request: U(q1) A(r1) U(q2) A[c1] T(c1)
    |}];
  print_points agent;
  [%expect
    {|
    point 1 (r1): 1 messages, ~0 tokens
    point 2 (q2): 2 messages, ~0 tokens
    point 3 (bash): 3 messages, ~0 tokens
    point 5 (r2): 5 messages, ~5 tokens
    |}];
  (* At the reply with the call: its result goes with it. *)
  print_trimmed agent ~before:(reply_entry agent "c1");
  print_state agent;
  run agent "q3";
  [%expect
    {|
    (Ok (3 0))
    state: messages=3 context_tokens=10 trim=3 messages, ~0 tokens
    request: U([the convers) A[c1] T(c1) A(r2) U(q3)
    |}];
  (* Trims stack; undo cancels the latest. *)
  print_trimmed agent ~before:(reply_entry agent "r2");
  run agent "q4";
  ignore (Or_error.ok_exn (Agent.untrim agent) : Session.Hidden.t);
  run agent "q5";
  ignore (Or_error.ok_exn (Agent.untrim agent) : Session.Hidden.t);
  print_state agent;
  [%expect
    {|
    (Ok (2 5))
    request: U([the convers) A(r2) U(q3) A(r3) U(q4)
    request: U([the convers) A[c1] T(c1) A(r2) U(q3) A(r3) U(q4) A(r4) U(q5)
    state: messages=12 context_tokens=10 trim=none
    |}]
;;

let%expect_test "a trim before a reply, with compactions" =
  let long = String.make 40_000 'x' in
  with_agent
    [ Reply.text "r1"
    ; Reply.text ("r2" ^ long)
    ; Reply.text "r3"
    ; Reply.text "SUMMARY"
    ; Reply.text "r4"
    ; Reply.text "r5"
    ; Reply.text "SUMMARY2"
    ; Reply.text "r6"
    ; Reply.text "r7"
    ]
  @@ fun agent ->
  run agent "q1";
  run agent "q2";
  run agent "q3";
  ignore (Or_error.ok_exn (Agent.compact agent) : string);
  run agent "q4";
  [%expect
    {|
    request: U(q1)
    request: U(q1) A(r1) U(q2)
    request: U(q1) A(r1) U(q2) A(r2xxxxxxxxxx) U(q3)
    summarise: U(q1) A(r1) U(q2) A(r2xxxxxxxxxx)
    request: U(q1) U(q2) S(SUMMARY) U(q3) A(r3) U(q4)
    |}];
  (* At a reply in the compaction's kept part: the carried-over messages and
     the summary go. *)
  print_trimmed agent ~before:(reply_entry agent "r3");
  run agent "q5";
  print_state agent;
  [%expect
    {|
    (Ok (4 10))
    request: U([the convers) A(r3) U(q4) A(r4) U(q5)
    state: messages=5 context_tokens=10 trim=4 messages, ~10 tokens
    |}];
  (* A compaction now summarises only what is after the trim (the provider
     puts the user turn ahead of it too). *)
  ignore (Or_error.ok_exn (Agent.compact agent) : string);
  run agent "q6";
  [%expect
    {|
    summarise: U([the convers) A(r3)
    request: S(SUMMARY2) U(q4) A(r4) U(q5) A(r5) U(q6)
    |}];
  (* Undone: the trimmed messages are back ahead of the new summary. *)
  ignore (Or_error.ok_exn (Agent.untrim agent) : Session.Hidden.t);
  run agent "q7";
  [%expect
    {| request: U(q1) U(q2) S(SUMMARY) U(q3) S(SUMMARY2) U(q4) A(r4) U(q5) A(r5) U(q6) A(r6) U(q7) |}]
;;

let%expect_test "a trim before a reply: rewinds, forks, reloads, masking" =
  with_session
  @@ fun ~dir session names ->
  ignore
    (Session.append_masked session ~through:(names.id "t1") : Session.Entry.t);
  print_result names (Session.append_trim session ~before:(names.id "a2"));
  print_context session;
  let reloaded = Or_error.ok_exn (Session.load (Session.path session)) in
  print_context reloaded;
  print_trim names reloaded;
  [%expect
    {|
    ok: before a2: 3 messages, ~101 tokens
    A[c1] T(c1) A(two) U(third) A(three)
    A[c1] T(c1) A(two) U(third) A(three)
    before a2: 3 messages, ~101 tokens
    |}];
  let fork = Or_error.ok_exn (Session.fork session ~at:(names.id "t1") ~dir) in
  print_context fork;
  print_endline
    (Option.value_map (Session.trim fork) ~default:"no trim" ~f:(fun t ->
       sprintf "%d messages, ~%d tokens" t.messages t.tokens));
  (* To the reply itself: still trimmed; to before it: the trim goes. *)
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "a2"));
  print_context session;
  Or_error.ok_exn (Session.rewind session ~to_:(names.id "u2"));
  print_context session;
  print_trim names session;
  [%expect
    {|
    A[c1] T(c1)
    3 messages, ~101 tokens
    A[c1]
    U(aaaaaaaaaaaa) A(one) U(second)
    no trim
    |}]
;;
