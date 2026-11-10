open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* A server whose sessions can ask each other, as [prigh]'s are set up: each
   session answers the model's requests from its own script, whichever runs
   first. *)
module World = struct
  type t =
    { sandbox : Tool_test_helpers.t
    ; server : Rpc_server.t
    ; questions : Session_questions.t
    ; scripts : Provider.t String.Table.t (** by session id *)
    ; names : string String.Table.t (** what session ids are printed as *)
    ; gate : unit Eio.Promise.t * unit Eio.Promise.u
    }

  let sessions_dir sandbox = Filename.concat sandbox.dir "sessions"

  (* [gate] keeps a run busy until the test opens it. *)
  let gate_tool (gate, _) =
    { Tool.spec =
        { Tool_spec.name = "gate"
        ; description = "Waits for the test."
        ; parameters = Tool_args.schema ~required:[] []
        ; parallel_safe = true
        ; destructive = Never
        ; on_host = false
        ; freeform = None
        }
    ; run =
        (fun context _ ->
          match
            Cancellation.protect context.cancel ~f:(fun () ->
              Eio.Promise.await gate)
          with
          | Some () -> Tool.Result.ok "the gate opened"
          | None -> Tool.Result.error "cancelled")
    }
  ;;

  let create sandbox ~sw =
    let questions = Session_questions.create () in
    let scripts = String.Table.create () in
    let gate = Eio.Promise.create () in
    let provider =
      { Provider.name = "scripts"
      ; stream =
          (fun request ~cancel ~on_event ->
            let script : Provider.t =
              Hashtbl.find_exn scripts (Option.value_exn request.cache_key)
            in
            script.stream request ~cancel ~on_event)
      }
    in
    let sessions_dir = sessions_dir sandbox in
    let new_agent ?session ~cwd () =
      let agent_ref = ref None in
      let current () =
        Option.map !agent_ref ~f:(fun agent ->
          let session = Agent.session agent in
          { Session_questions.Party.id = Session.id session
          ; name = Session.name session
          })
      in
      let agent =
        Agent.create
          ~env:sandbox.env
          ~sw
          ~provider
          ~tools:
            [ Tool_ls.tool
            ; gate_tool gate
            ; Tool_ask_session.create
                ~sessions_dir:(fun () -> sessions_dir)
                ~current
                ~questions
                ()
            ; Tool_answer_session.create ~current ~questions ()
            ]
          ~sessions_dir
          ~home:sandbox.dir
          ?session
          ~questions
          ~cwd
          ()
      in
      agent_ref := Some agent;
      agent
    in
    let server =
      Rpc_server.create
        ~env:sandbox.env
        ~sw
        ~login:(Test_rpc.login_manager sandbox ~sw)
        ~sessions_dir
        ~cwd:sandbox.dir
        ~new_agent
        ()
    in
    Rpc_server.serve_questions server questions;
    { sandbox
    ; server
    ; questions
    ; scripts
    ; names = String.Table.create ()
    ; gate
    }
  ;;

  (* What the session's model says from now on. *)
  let script t agent replies =
    Hashtbl.set
      t.scripts
      ~key:(Session.id (Agent.session agent))
      ~data:(Faux_provider.create replies)
  ;;

  (* A client in a new session called [name], which has said hello, so that
     its file exists, and goes on with [replies]. *)
  let session t name replies =
    let client = Rpc_server.connect t.server ~send:ignore in
    let agent = Rpc_server.agent_of_client t.server client in
    Hashtbl.set t.names ~key:(Session.id (Agent.session agent)) ~data:name;
    Agent.set_session_name agent name;
    script t agent [ Reply.text ("Hello from " ^ name) ];
    Or_error.ok_exn (Agent.prompt agent "hello");
    Agent.wait_idle agent;
    script t agent replies;
    client, agent
  ;;

  let named t text =
    Hashtbl.fold t.names ~init:text ~f:(fun ~key:id ~data:name text ->
      String.substr_replace_all text ~pattern:id ~with_:("<" ^ name ^ ">"))
    |> mask t.sandbox
  ;;

  (* The session's messages after its hello, as its file has them. *)
  let print_transcript t session =
    let session = Or_error.ok_exn (Session.load (Session.path session)) in
    List.iter
      (List.drop (Session.messages session) 2)
      ~f:(fun message ->
        (match message with
         | User u ->
           sprintf
             "user%s: %s"
             (match u.origin with
              | Some { source = Some source; _ } ->
                sprintf " {%s}" (Injected.Source.title source)
              | Some _ | None -> "")
             u.text
         | Assistant a ->
           sprintf
             "assistant: %s%s%s"
             (Message.Assistant.text a)
             (String.concat
                (List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
                   sprintf " [%s %s]" c.name c.arguments)))
             (match a.stop_reason with
              | Error e -> sprintf " (error: %s)" e
              | Aborted -> " (aborted)"
              | End_turn | Tool_use | Length -> "")
         | Tool_result r ->
           sprintf "result%s: %s" (if r.is_error then " (error)" else "") r.text)
        |> named t
        |> String.split_lines
        |> List.iteri ~f:(fun i line ->
          print_endline (if i = 0 then line else "  | " ^ line)))
  ;;

  let print t name agent =
    printf "--- %s ---\n" name;
    print_transcript t (Agent.session agent)
  ;;
end

let wait_until ~what f =
  let rec go n =
    if f ()
    then ()
    else if n = 0
    then failwithf "timed out waiting for %s" what ()
    else (
      Eio.Fiber.yield ();
      go (n - 1))
  in
  go 100_000
;;

(* [agent] took the answer (or anything else) and its run is over. *)
let wait_for_message ~what agent ~substring =
  wait_until ~what (fun () ->
    (not (Agent.is_running agent))
    && List.exists (Agent.messages agent) ~f:(function
      | User u -> String.is_substring u.text ~substring
      | Assistant _ | Tool_result _ -> false))
;;

let at_the_gate agent () =
  Agent.is_running agent
  &&
  match List.last (Agent.messages agent) with
  | Some (Assistant a) -> not (List.is_empty (Message.Assistant.tool_calls a))
  | Some (User _ | Tool_result _) | None -> false
;;

let with_world f =
  with_sandbox
  @@ fun sandbox ->
  Eio.Switch.run
  @@ fun sw ->
  let world = World.create sandbox ~sw in
  f world;
  Rpc_server.shutdown world.server
;;

let ask_call ?(id = "c1") target question =
  Reply.tool_call
    ~id
    ~name:"ask_session"
    ~arguments:
      (Json.to_string
         (`Object [ "session", `String target; "question", `String question ]))
    ()
;;

let answer_call ?(call = "d2") id answer =
  Reply.tool_call
    ~id:call
    ~name:"answer_session"
    ~arguments:
      (Json.to_string (`Object [ "id", `String id; "answer", `String answer ]))
    ()
;;

let id agent = Session.id (Agent.session agent)

let%expect_test
    "an idle session is asked: it works with its tools, answers with \
     answer_session, and the asker is woken with the answer"
  =
  with_world
  @@ fun w ->
  let _, b =
    World.session
      w
      "B"
      [ Reply.tool_call ~text:"Looking." ~id:"d1" ~name:"ls" ~arguments:"{}" ()
      ; answer_call "q1" "The cache is in sessions/."
      ; Reply.text "Answered."
      ]
  in
  let _, a =
    World.session
      w
      "A"
      [ ask_call (id b) "Where is the cache?"
      ; Reply.text "Asked B; waiting."
      ; Reply.text "B says the cache is in sessions/."
      ]
  in
  Or_error.ok_exn (Agent.prompt a "ask B where the cache is");
  wait_for_message ~what:"the answer" a ~substring:"answered q1";
  Agent.wait_idle b;
  World.print w "A" a;
  World.print w "B" b;
  printf
    !"open questions: %{sexp: Session_questions.Question.t list}\n"
    (Session_questions.asked w.questions ~session:(id b));
  [%expect
    {|
    --- A ---
    user: ask B where the cache is
    assistant:  [ask_session {"session":"<B>","question":"Where is the cache?"}]
    result: asked <B> (B) as q1; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    assistant: Asked B; waiting.
    user {Session B answered (q1)}: [session <B> (B) answered q1]
      |
      | The cache is in sessions/.
    assistant: B says the cache is in sessions/.
    --- B ---
    user {Session A asks (q1)}: [session <A> (A) asks q1]
      |
      | Where is the cache?
      |
      | [Answer with answer_session (id q1), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant: Looking. [ls {}]
    result: sessions/
    assistant:  [answer_session {"id":"q1","answer":"The cache is in sessions/."}]
    result: sent
    assistant: Answered.
    open questions: ()
    |}]
;;

let%expect_test
    "a busy session gets the question at its next step, answers, and carries \
     on with its task"
  =
  with_world
  @@ fun w ->
  let _, b =
    World.session
      w
      "B"
      [ Reply.tool_call
          ~text:"Refactoring."
          ~id:"d1"
          ~name:"gate"
          ~arguments:"{}"
          ()
      ; answer_call "q1" "Half way: the parser is done."
      ; Reply.text "The refactor is finished."
      ]
  in
  let _, a =
    World.session
      w
      "A"
      [ ask_call (id b) "How far is the refactor?"
      ; Reply.text "Asked."
      ; Reply.text "B is half way."
      ]
  in
  Or_error.ok_exn (Agent.prompt b "refactor the parser");
  wait_until ~what:"B to be at the gate" (at_the_gate b);
  Or_error.ok_exn (Agent.prompt a "ask B how far it is");
  Agent.wait_idle a;
  printf "B is running: %b\n" (Agent.is_running b);
  World.print w "A, before B's step ends" a;
  Eio.Promise.resolve (snd w.gate) ();
  wait_for_message ~what:"the answer" a ~substring:"answered q1";
  Agent.wait_idle b;
  World.print w "A" a;
  World.print w "B" b;
  [%expect
    {|
    B is running: true
    --- A, before B's step ends ---
    user: ask B how far it is
    assistant:  [ask_session {"session":"<B>","question":"How far is the refactor?"}]
    result: asked <B> (B) as q1; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    assistant: Asked.
    --- A ---
    user: ask B how far it is
    assistant:  [ask_session {"session":"<B>","question":"How far is the refactor?"}]
    result: asked <B> (B) as q1; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    assistant: Asked.
    user {Session B answered (q1)}: [session <B> (B) answered q1]
      |
      | Half way: the parser is done.
    assistant: B is half way.
    --- B ---
    user: refactor the parser
    assistant: Refactoring. [gate {}]
    result: the gate opened
    user {Session A asks (q1)}: [session <A> (A) asks q1]
      |
      | How far is the refactor?
      |
      | [Answer with answer_session (id q1), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant:  [answer_session {"id":"q1","answer":"Half way: the parser is done."}]
    result: sent
    assistant: The refactor is finished.
    |}]
;;

let%expect_test
    "a session that is not loaded is loaded to answer, and so is an asker that \
     was closed meanwhile"
  =
  with_world
  @@ fun w ->
  let b_client, b =
    World.session
      w
      "B"
      [ Reply.tool_call ~id:"d1" ~name:"gate" ~arguments:"{}" ()
      ; answer_call "q1" "42."
      ; Reply.text "Answered."
      ]
  in
  let b_session = Agent.session b in
  Rpc_server.disconnect w.server b_client;
  let a_client, a =
    World.session
      w
      "A"
      [ ask_call (id b) "What is the number?"
      ; Reply.text "Asked."
      ; Reply.text "It is 42."
      ]
  in
  let a_session = Agent.session a in
  Or_error.ok_exn (Agent.prompt a "ask B for the number");
  Agent.wait_idle a;
  Rpc_server.disconnect w.server a_client;
  Eio.Promise.resolve (snd w.gate) ();
  let said session text =
    match Session.load (Session.path session) with
    | Error _ -> false
    | Ok session ->
      List.exists (Session.messages session) ~f:(function
        | Assistant a -> String.equal (Message.Assistant.text a) text
        | User _ | Tool_result _ -> false)
  in
  wait_until ~what:"both sessions to finish" (fun () ->
    said a_session "It is 42." && said b_session "Answered.");
  printf "--- A ---\n";
  World.print_transcript w a_session;
  printf "--- B ---\n";
  World.print_transcript w b_session;
  [%expect
    {|
    --- A ---
    user: ask B for the number
    assistant:  [ask_session {"session":"<B>","question":"What is the number?"}]
    result: asked <B> (B) as q1; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    assistant: Asked.
    user {Session B answered (q1)}: [session <B> (B) answered q1]
      |
      | 42.
    assistant: It is 42.
    --- B ---
    user {Session A asks (q1)}: [session <A> (A) asks q1]
      |
      | What is the number?
      |
      | [Answer with answer_session (id q1), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant:  [gate {}]
    result: the gate opened
    assistant:  [answer_session {"id":"q1","answer":"42."}]
    result: sent
    assistant: Answered.
    |}]
;;

(* The second answer is there before the asker's next step, which takes it
   without a run of its own. *)
let%expect_test
    "a run that ends without answer_session answers with its last message; a \
     failed and an aborted one say so"
  =
  with_world
  @@ fun w ->
  let _, b =
    World.session
      w
      "B"
      [ Reply.text "It lives in lib/cache."
      ; Reply.error "provider down"
      ; Reply.tool_call ~id:"d1" ~name:"gate" ~arguments:"{}" ()
      ]
  in
  let _, a =
    World.session
      w
      "A"
      [ ask_call (id b) "Where does it live?"
      ; Reply.text "Asked."
      ; ask_call ~id:"c2" (id b) "And the tests?"
      ; ask_call ~id:"c3" (id b) "And the docs?"
      ; Reply.text "Asked once more."
      ; Reply.text "Enough."
      ]
  in
  Or_error.ok_exn (Agent.prompt a "ask B");
  wait_for_message ~what:"the first answer" a ~substring:"did not answer q1";
  wait_for_message ~what:"the second answer" a ~substring:"did not answer q2";
  wait_until ~what:"B to be at the gate" (at_the_gate b);
  ignore (Agent.abort b : string list);
  wait_for_message ~what:"the third answer" a ~substring:"did not answer q3";
  Agent.wait_idle b;
  World.print w "A" a;
  World.print w "B" b;
  [%expect
    {|
    --- A ---
    user: ask B
    assistant:  [ask_session {"session":"<B>","question":"Where does it live?"}]
    result: asked <B> (B) as q1; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    assistant: Asked.
    user {Session B did not answer (q1)}: [session <B> (B) did not answer q1]
      |
      | Its run ended without answer_session; its last message:
      |
      | It lives in lib/cache.
    assistant:  [ask_session {"session":"<B>","question":"And the tests?"}]
    result: asked <B> (B) as q2; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    user {Session B did not answer (q2)}: [session <B> (B) did not answer q2]
      |
      | Its run failed before it answered: provider down
    assistant:  [ask_session {"session":"<B>","question":"And the docs?"}]
    result: asked <B> (B) as q3; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    assistant: Asked once more.
    user {Session B did not answer (q3)}: [session <B> (B) did not answer q3]
      |
      | Its run was aborted before it answered.
    assistant: Enough.
    --- B ---
    user {Session A asks (q1)}: [session <A> (A) asks q1]
      |
      | Where does it live?
      |
      | [Answer with answer_session (id q1), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant: It lives in lib/cache.
    user {Session A asks (q2)}: [session <A> (A) asks q2]
      |
      | And the tests?
      |
      | [Answer with answer_session (id q2), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant:  (error: provider down)
    user {Session A asks (q3)}: [session <A> (A) asks q3]
      |
      | And the docs?
      |
      | [Answer with answer_session (id q3), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant:  [gate {}]
    result (error): cancelled
    |}]
;;

let%expect_test
    "what cannot be asked or answered: this session, unknown ids, paths, an \
     empty question, too many open questions, questions nobody asked"
  =
  with_world
  @@ fun w ->
  let _, b =
    World.session
      w
      "B"
      [ Reply.tool_call ~id:"d1" ~name:"gate" ~arguments:"{}" ()
      ; answer_call "q9" "A guess."
      ; Reply.text "Never mind."
      ]
  in
  let _, a = World.session w "A" [] in
  let ask call session question =
    ( call
    , "ask_session"
    , Json.to_string
        (`Object [ "session", `String session; "question", `String question ]) )
  in
  World.script
    w
    a
    [ Reply.tool_calls
        [ ask "s1" (id a) "Who am I?"
        ; ask "s2" "nosuch" "Anyone?"
        ; ask "s3" (Session.path (Agent.session b)) "By path?"
        ; ask "s4" (id b) " "
        ; "s5", "answer_session", {|{"id": "q1", "answer": "Nobody asked."}|}
        ]
    ; Reply.text "None of that worked."
    ; Reply.tool_calls
        (List.init (Session_questions.max_open + 1) ~f:(fun i ->
           ask (sprintf "c%d" i) (id b) (sprintf "Question %d?" i)))
    ; Reply.text "Asked."
    ; Reply.text "One came back."
    ; Reply.text "The rest came back."
    ];
  Or_error.ok_exn (Agent.prompt a "try what cannot work");
  Agent.wait_idle a;
  Or_error.ok_exn (Agent.prompt a "ask B a lot");
  wait_until ~what:"B to be at the gate" (at_the_gate b);
  Agent.wait_idle a;
  World.print w "A" a;
  [%expect
    {|
    --- A ---
    user: try what cannot work
    assistant:  [ask_session {"session":"<A>","question":"Who am I?"}] [ask_session {"session":"nosuch","question":"Anyone?"}] [ask_session {"session":"$DIR/sessions/<stamp>_<B>.jsonl","question":"By path?"}] [ask_session {"session":"<B>","question":" "}] [answer_session {"id": "q1", "answer": "Nobody asked."}]
    result (error): that is this session: its conversation is your own context, so answer from it
    result (error): no session "nosuch"; action search of the sessions tool finds sessions and their ids
    result (error): "$DIR/sessions/<stamp>_<B>.jsonl" is a path: give the session's id (action search of the sessions tool finds it)
    result (error): question is empty: say what you want to know
    result (error): no question "q1" is open: no session waits for an answer of this one
    assistant: None of that worked.
    user: ask B a lot
    assistant:  [ask_session {"session":"<B>","question":"Question 0?"}] [ask_session {"session":"<B>","question":"Question 1?"}] [ask_session {"session":"<B>","question":"Question 2?"}] [ask_session {"session":"<B>","question":"Question 3?"}] [ask_session {"session":"<B>","question":"Question 4?"}]
    result: asked <B> (B) as q1; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    result: asked <B> (B) as q2; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    result: asked <B> (B) as q3; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    result: asked <B> (B) as q4; its answer arrives later as a message: keep working on what does not depend on it, or end your turn
    result (error): <B> was not asked: 4 questions of this session are still open (q1, q2, q3, q4): wait for their answers, which arrive as messages
    assistant: Asked.
    |}];
  Eio.Promise.resolve (snd w.gate) ();
  wait_for_message ~what:"the last answer" a ~substring:"did not answer q4";
  Agent.wait_idle b;
  World.print w "B" b;
  printf "--- A's answers ---\n";
  List.iter (Agent.messages a) ~f:(function
    | User { origin = Some { source = Some source; _ }; _ } ->
      print_endline (World.named w (Injected.Source.title source))
    | User _ | Assistant _ | Tool_result _ -> ());
  [%expect
    {|
    --- B ---
    user {Session A asks (q1)}: [session <A> (A) asks q1]
      |
      | Question 0?
      |
      | [Answer with answer_session (id q1), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant:  [gate {}]
    result: the gate opened
    user {Session A asks (q2)}: [session <A> (A) asks q2]
      |
      | Question 1?
      |
      | [Answer with answer_session (id q2), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    user {Session A asks (q3)}: [session <A> (A) asks q3]
      |
      | Question 2?
      |
      | [Answer with answer_session (id q3), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    user {Session A asks (q4)}: [session <A> (A) asks q4]
      |
      | Question 3?
      |
      | [Answer with answer_session (id q4), using your tools first if it takes them: the answer goes to that session, not to the user. Then carry on with your work.]
    assistant:  [answer_session {"id":"q9","answer":"A guess."}]
    result (error): no question "q9" is open; open: q1, q2, q3, q4
    assistant: Never mind.
    --- A's answers ---
    Session B did not answer (q1)
    Session B did not answer (q2)
    Session B did not answer (q3)
    Session B did not answer (q4)
    |}]
;;

let%expect_test
    "without a server nothing is asked, a session that cannot be woken is not \
     asked, and an answer nobody can take is dropped"
  =
  let party id = { Session_questions.Party.id; name = None } in
  let show questions =
    printf
      !"open: %{sexp: Session_questions.Question.t list}, mail for b: %b, for \
        a: %b\n"
      (Session_questions.asked questions ~session:"b")
      (Session_questions.has_mail questions ~session:"b")
      (Session_questions.has_mail questions ~session:"a")
  in
  let ask questions =
    Session_questions.ask
      questions
      ~asker:(party "a")
      ~target:(party "b")
      ~question:"Well?"
    |> Or_error.map ~f:(fun (q : Session_questions.Question.t) -> q.id)
  in
  let questions = Session_questions.create () in
  print_s [%sexp (ask questions : string Or_error.t)];
  show questions;
  Session_questions.set_wake questions (fun ~session ->
    if String.equal session "b"
    then Ok ()
    else Or_error.errorf "no session %S" session);
  print_s [%sexp (ask questions : string Or_error.t)];
  show questions;
  Out_channel.flush stdout;
  print_s
    [%sexp
      (Session_questions.answer questions ~session:"b" ~id:"q2" "Fine."
       : unit Or_error.t)];
  show questions;
  Session_questions.set_wake questions (fun ~session:_ ->
    Or_error.error_string "its file is gone");
  print_s [%sexp (ask questions : string Or_error.t)];
  show questions;
  [%expect
    {|
    (Error
     "sessions cannot reach each other in this mode of prigh: read that session with the sessions tool instead")
    open: (), mail for b: false, for a: false
    (Ok q2)
    open: (((id q2) (asker ((id a) (name ()))) (target ((id b) (name ())))
      (question Well?))), mail for b: true, for a: false
    prigh: the answer to q2 of session a is dropped: no session "a"
    (Ok ())
    open: (), mail for b: true, for a: false
    (Error "its file is gone")
    open: (), mail for b: true, for a: false
    |}]
;;

let%expect_test
    "subagents neither ask nor answer other sessions, and plan mode does not \
     ask"
  =
  List.iter [ "ask_session"; "answer_session"; "sessions" ] ~f:(fun name ->
    printf
      "%s: as a subagent: %s; changes work: %b\n"
      name
      (Option.value (Tools.unavailable ~depth:1 name) ~default:"available")
      (Plan_mode.changes_work ~tool:name (`Object [])));
  [%expect
    {|
    ask_session: as a subagent: sessions ask and answer each other through the main agent, and you are a subagent: read the other session with the sessions tool, or put the question in your report; changes work: true
    answer_session: as a subagent: sessions ask and answer each other through the main agent, and you are a subagent: read the other session with the sessions tool, or put the question in your report; changes work: false
    sessions: as a subagent: available; changes work: false
    |}]
;;
