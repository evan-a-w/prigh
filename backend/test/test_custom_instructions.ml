open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let day n =
  Time_float.add Time_float.epoch (Time_float.Span.of_day (20_000. +. n))
;;

let store t = Custom_instructions.create ~home:t.dir

let print_file t =
  match In_channel.read_all (Custom_instructions.path ~home:t.dir) with
  | data -> print_string data
  | exception _ -> print_endline "(no file)"
;;

let show ci =
  match Custom_instructions.notes ci with
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
  | Ok notes ->
    print_endline
      (Option.value
         (Custom_instruction.section
            (List.map notes ~f:Custom_instructions.to_model))
         ~default:"(none)")
;;

let print_result = function
  | Ok (note : Custom_instructions.Note.t) ->
    printf
      "ok %s %s: %s\n"
      note.id
      (Option.value_map note.title ~default:"-" ~f:(sprintf "[%s]"))
      note.text
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let add ?title ci text =
  print_result (Custom_instructions.add ci ?title ~now:(day 0.) text)
;;

let%expect_test "add, edit, remove; ids are never reused; the file" =
  with_sandbox
  @@ fun t ->
  let ci = store t in
  Custom_instructions.subscribe ci ~f:(fun () -> print_endline "(changed)");
  show ci;
  print_file t;
  [%expect
    {|
    (none)
    (no file)
    |}];
  add ci "Use Australian spelling.";
  add
    ci
    ~title:"Maths"
    "Keep a single memory note on my maths ability\nup to date.";
  show ci;
  [%expect
    {|
    (changed)
    ok ci1 -: Use Australian spelling.
    (changed)
    ok ci2 [Maths]: Keep a single memory note on my maths ability
    up to date.
    Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep a single memory note on my maths ability
      up to date.
    |}];
  (* Text only, title only, the title removed; by id without case. *)
  print_result
    (Custom_instructions.edit
       ci
       ~id:"CI1"
       ~text:"Use British spelling."
       ~now:(day 1.)
       ());
  print_result
    (Custom_instructions.edit ci ~id:"ci1" ~title:"Spelling" ~now:(day 1.) ());
  print_result
    (Custom_instructions.edit ci ~id:"ci2" ~title:"" ~now:(day 2.) ());
  print_result
    (Custom_instructions.edit ci ~id:"ci9" ~text:"x" ~now:(day 2.) ());
  [%expect
    {|
    (changed)
    ok ci1 -: Use British spelling.
    (changed)
    ok ci1 [Spelling]: Use British spelling.
    (changed)
    ok ci2 -: Keep a single memory note on my maths ability
    up to date.
    error: no custom instruction "ci9"; they are ci1, ci2
    |}];
  print_result (Custom_instructions.remove ci "ci2");
  print_result (Custom_instructions.remove ci "spelling");
  print_result (Custom_instructions.remove ci "ci1");
  add ci "Answer briefly.";
  print_file t;
  [%expect
    {|
    (changed)
    ok ci2 -: Keep a single memory note on my maths ability
    up to date.
    (changed)
    ok ci1 [Spelling]: Use British spelling.
    error: no custom instruction "ci1": there are none
    (changed)
    ok ci3 -: Answer briefly.
    {
      "next": 4,
      "notes": [
        {
          "id": "ci3",
          "text": "Answer briefly.",
          "saved": "2024-10-04 00:00:00.000000Z"
        }
      ]
    }
    |}]
;;

let%expect_test "limits: text, title, count; nothing written when refused" =
  with_sandbox
  @@ fun t ->
  let ci = store t in
  print_s
    [%sexp
      { max_text : int = Custom_instructions.max_text
      ; max_title : int = Custom_instructions.max_title
      ; max_notes : int = Custom_instructions.max_notes
      }];
  add ci "  ";
  add ci (String.make 1001 'x');
  add ci ~title:(String.make 81 't') "ok";
  add ci (String.make 1000 'x');
  print_result (Custom_instructions.edit ci ~id:"ci1" ~text:"" ~now:(day 0.) ());
  [%expect
    {|
    ((max_text 1000) (max_title 80) (max_notes 50))
    error: the instruction's text is empty
    error: an instruction is limited to 1000 bytes (this one has 1001): they are in the context of every session, so keep each to the point (and what is only sometimes needed in memory)
    error: a title is limited to 80 bytes: use a few words
    ok ci1 -: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    error: the instruction's text is empty
    |}];
  for i = 2 to 50 do
    ignore
      (Or_error.ok_exn
         (Custom_instructions.add ci ~now:(day 0.) (sprintf "rule %d" i))
       : Custom_instructions.Note.t)
  done;
  add ci "one too many";
  [%expect
    {| error: there are 50 custom instructions, the most there can be: remove or merge some first |}]
;;

let%expect_test "each namespace has its own; memory's file is left alone" =
  with_sandbox
  @@ fun t ->
  let a = Custom_instructions.create ~home:(Filename.concat t.dir "a") in
  let b = Custom_instructions.create ~home:(Filename.concat t.dir "b") in
  add a "In a.";
  add b "In b.";
  show a;
  show b;
  print_s
    [%sexp
      (Sys_unix.file_exists_exn (Memory.path ~home:(Filename.concat t.dir "a"))
       : bool)];
  [%expect
    {|
    ok ci1 -: In a.
    ok ci1 -: In b.
    Custom instructions (the user's standing instructions; follow them):
    [ci1] In a.
    Custom instructions (the user's standing instructions; follow them):
    [ci1] In b.
    false
    |}]
;;

let i ?title id text = { Custom_instruction.id; title; text }

let%expect_test "the note for a session told an earlier list" =
  let show ~told now =
    print_endline
      (Option.value (Custom_instruction.note ~told now) ~default:"(nothing)")
  in
  let a = i "ci1" "Use Australian spelling."
  and b = i "ci2" ~title:"Maths" "Keep a note on my maths."
  and c = i "ci3" "Answer briefly."
  and d = i "ci4" "Prefer OCaml." in
  show ~told:[ a; b; c ] [ a; b; c ];
  (* A few changes: only they are told. *)
  show
    ~told:[ a; b; c ]
    [ a; { b with text = "Keep one note on my maths." }; c; d ];
  show ~told:[ a; b; c ] [ a; c ];
  [%expect
    {|
    (nothing)
    [Custom instructions changed (the user's standing instructions; follow them; the others are unchanged):
    Edited, now [ci2] (Maths) Keep one note on my maths.
    Added [ci4] Prefer OCaml.]
    [Custom instructions changed (the user's standing instructions; follow them; the others are unchanged):
    Removed [ci2]]
    |}];
  (* Most of it changed, or none were told: the whole list. None left. *)
  show ~told:[ a; b ] [ { a with text = "x" }; c ];
  show ~told:[] [ a ];
  show ~told:[ a; b ] [];
  [%expect
    {|
    [Custom instructions (the user's standing instructions; follow them) are now:
    [ci1] x
    [ci3] Answer briefly.
    These replace any earlier custom instructions.]
    [Custom instructions (the user's standing instructions; follow them) are now:
    [ci1] Use Australian spelling.
    These replace any earlier custom instructions.]
    [Custom instructions: the user has no standing instructions any more: none of the earlier ones apply.]
    |}];
  print_s
    [%sexp
      (Custom_instruction.apply [ a; b ] ~id:"ci2" None
       : Custom_instruction.t list)
    , (Custom_instruction.apply [ a; b ] ~id:"ci1" (Some { a with text = "y" })
       : Custom_instruction.t list)
    , (Custom_instruction.apply [ a ] ~id:"ci4" (Some d)
       : Custom_instruction.t list)];
  [%expect
    {|
    ((((id ci1) (text "Use Australian spelling.")))
     (((id ci1) (text y))
      ((id ci2) (title Maths) (text "Keep a note on my maths.")))
     (((id ci1) (text "Use Australian spelling."))
      ((id ci4) (text "Prefer OCaml."))))
    |}]
;;

let tool ?(now = fun () -> day 0.) t =
  Tool_custom_instructions.create ~now ~custom_instructions:(store t) ()
;;

let run_tool ?(emit = ignore) t tool args =
  let result =
    Tool.execute
      tool
      (Tool.Context.create ~env:t.env ~cwd:t.dir ~emit ())
      (Jsonaf.of_string args)
  in
  printf "%s%s\n" (if result.is_error then "error: " else "") result.text
;;

let%expect_test "the tool: add, edit, remove, list; refusals; what it emits" =
  with_sandbox
  @@ fun t ->
  let tool = tool t in
  let emit (e : Agent_event.t) =
    match e with
    | Custom_instruction_set { id; instruction } ->
      print_s
        [%sexp
          "emitted", (id : string), (instruction : Custom_instruction.t option)]
    | _ -> ()
  in
  List.iter
    [ {|{"action":"list"}|}
    ; {|{"action":"add","text":"Use Australian spelling."}|}
    ; {|{"action":"add","title":"Maths","text":"Keep one note on my maths."}|}
    ; {|{"action":"edit","id":"ci1","text":"Use British spelling."}|}
    ; {|{"action":"edit","id":"ci1"}|}
    ; {|{"action":"edit","id":"ci7","text":"x"}|}
    ; {|{"action":"add"}|}
    ; {|{"action":"remove","id":"ci2"}|}
    ; {|{"action":"remove"}|}
    ; {|{"action":"list"}|}
    ; {|{"action":"search"}|}
    ]
    ~f:(fun args ->
      printf "%s -> " args;
      run_tool ~emit t tool args);
  [%expect
    {|
    {"action":"list"} -> There are no custom instructions.
    {"action":"add","text":"Use Australian spelling."} -> (emitted ci1 (((id ci1) (text "Use Australian spelling."))))
    Added ci1
    {"action":"add","title":"Maths","text":"Keep one note on my maths."} -> (emitted ci2 (((id ci2) (title Maths) (text "Keep one note on my maths."))))
    Added ci2 · Maths
    {"action":"edit","id":"ci1","text":"Use British spelling."} -> (emitted ci1 (((id ci1) (text "Use British spelling."))))
    Edited ci1
    {"action":"edit","id":"ci1"} -> error: edit needs text or title (or both)
    {"action":"edit","id":"ci7","text":"x"} -> error: no custom instruction "ci7"; they are ci1, ci2
    {"action":"add"} -> error: invalid arguments: add needs text (the instruction)
    {"action":"remove","id":"ci2"} -> (emitted ci2 ())
    Removed ci2 · Maths
    {"action":"remove"} -> error: invalid arguments: remove needs id (the instruction's id)
    {"action":"list"} -> Custom instructions (the user's standing instructions; follow them):
    [ci1] Use British spelling.
    {"action":"search"} -> error: unknown action "search": use add, edit, remove or list
    |}];
  (* Off: every call is refused, nothing changes. *)
  let ci = store t in
  Or_error.ok_exn (Custom_instructions.set_enabled ci false);
  run_tool t tool {|{"action":"add","text":"x"}|};
  show ci;
  [%expect
    {|
    error: the user does not let you change their custom instructions (Settings, Custom instructions in prigh-web; /instructions on in the terminal): suggest the change to them instead.
    Custom instructions (the user's standing instructions; follow them):
    [ci1] Use British spelling.
    |}]
;;

let%expect_test "spec: backend-side, main agent only; approvals as memory's" =
  with_sandbox
  @@ fun t ->
  let spec = Tool_custom_instructions.spec in
  printf "on_host=%b parallel_safe=%b\n" spec.on_host spec.parallel_safe;
  print_endline (String.prefix spec.description 80);
  let decide ~mode ~plan tool action =
    let args = `Object [ "action", `String action ] in
    let destructive =
      Tool_spec.Destructive.call
        (if String.equal tool "memory"
         then Tool_memory.spec.destructive
         else spec.destructive)
        args
    in
    Approval.decide
      ~mode
      ~plan
      ~rules:Approval.Rules.empty
      ~session:[]
      ~cwd:t.dir
      ~home:t.dir
      ~destructive
      ~changes_work:(Plan_mode.changes_work ~tool args)
      { Approval_rule.Call.tool; command = None; paths = [] }
  in
  List.iter
    [ "memory", [ "save"; "recall"; "forget" ]
    ; "custom_instructions", [ "add"; "edit"; "remove"; "list" ]
    ]
    ~f:(fun (tool, actions) ->
      List.iter actions ~f:(fun action ->
        printf
          "%-20s %-7s %s\n"
          tool
          action
          (String.concat
             ~sep:" "
             (List.map
                [ "ask", Approval_mode.Ask, false
                ; "auto_edit", Auto_edit, false
                ; "auto", Auto, false
                ; "plan", Auto, true
                ]
                ~f:(fun (label, mode, plan) ->
                  sprintf
                    "%s=%s"
                    label
                    (match decide ~mode ~plan tool action with
                     | Allow -> "allow"
                     | Ask _ -> "ask"
                     | Deny _ -> "deny"))))));
  print_s
    [%sexp
      (List.map [ 0; 1 ] ~f:(fun depth ->
         Tools.unavailable ~depth Tool_custom_instructions.name)
       : string option list)];
  List.iter
    [ {|{"action":"add","title":"Spelling","text":"Use Australian spelling."}|}
    ; {|{"action":"add","text":"Answer briefly.\nAlways."}|}
    ; {|{"action":"edit","id":"ci2","text":"Answer very briefly."}|}
    ; {|{"action":"remove","id":"ci1"}|}
    ; {|{"action":"list"}|}
    ]
    ~f:(fun args ->
      let args = Jsonaf.of_string args in
      printf
        "%-48s | %s\n"
        (Agent_loop.tool_summary ~name:"custom_instructions" args)
        (Acp_tool_call.title ~name:"custom_instructions" ~args ~cwd:t.dir));
  [%expect
    {|
    on_host=false parallel_safe=false
    Change the user's custom instructions: standing ones in every session's context,
    memory               save    ask=ask auto_edit=ask auto=allow plan=ask
    memory               recall  ask=allow auto_edit=allow auto=allow plan=allow
    memory               forget  ask=ask auto_edit=ask auto=allow plan=ask
    custom_instructions  add     ask=ask auto_edit=ask auto=allow plan=ask
    custom_instructions  edit    ask=ask auto_edit=ask auto=allow plan=ask
    custom_instructions  remove  ask=ask auto_edit=ask auto=allow plan=ask
    custom_instructions  list    ask=allow auto_edit=allow auto=allow plan=allow
    (()
     ("it is the main agent's (the user's plan, questions, components, goal, memory and custom instructions), and you are a subagent: put what you need from the user in your report"))
    add Spelling: Use Australian spelling.           | Add a custom instruction: Spelling
    add Answer briefly. Always.                      | Add a custom instruction: Answer briefly. …
    edit Answer very briefly. ci2                    | Edit custom instruction ci2: Answer very briefly.
    remove ci1                                       | Remove custom instruction: ci1
    list                                             | List custom instructions
    |}]
;;

(* Agents over one session file, as [Test_user_info]'s: what each request's
   system prompt has of the instructions, and the note told with it. *)
let print_request requests ~text =
  let request : Provider.Request.t = Queue.last_exn requests in
  let system = Option.value request.system ~default:"" in
  (match String.substr_index system ~pattern:"Custom instructions (" with
   | Some i -> printf "system: … %s\n" (String.drop_prefix system i)
   | None -> print_endline "system: no custom instructions");
  List.iter request.messages ~f:(function
    | Message.User u when String.is_suffix u.text ~suffix:text ->
      let notes =
        Option.value_map u.origin ~default:[] ~f:(fun (i : Injected.t) ->
          List.filter i.notes ~f:(fun n ->
            match n.kind with
            | Custom_instructions _ -> true
            | _ -> false))
      in
      (match notes with
       | [] -> printf "%s: nothing told\n" text
       | notes ->
         List.iter notes ~f:(fun n ->
           assert (String.is_substring u.text ~substring:n.text);
           printf "%s: %s\n%s\n" text (Injected.Note.title n) n.text))
    | _ -> ());
  Queue.clear requests
;;

let with_agents ?(replies = List.init 20 ~f:(fun _ -> Reply.text "ok")) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create ~on_request:(Queue.enqueue requests) replies
  in
  let ci = store t in
  let make ?session () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ [ tool ~now:(fun () -> day 0.) t ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?session
      ~custom_instructions:ci
      ~offered:(fun tool ->
        (not (String.equal (Tool.name tool) Tool_custom_instructions.name))
        || Custom_instructions.enabled ci)
      ~cwd:t.dir
      ()
  in
  let restart agent =
    make
      ~session:(Or_error.ok_exn (Session.load (Agent.state agent).session_path))
      ()
  in
  let send agent text =
    Or_error.ok_exn (Agent.prompt agent text);
    Agent.wait_idle agent;
    print_request requests ~text
  in
  f t ci ~make ~restart ~send ~requests
;;

let%expect_test
    "a new session has them in its system prompt; changes are told once, with \
     the next message, also after a restart"
  =
  with_agents
  @@ fun _t ci ~make ~restart ~send ~requests:_ ->
  add ci "Use Australian spelling.";
  add ci ~title:"Maths" "Keep one note on my maths.";
  add ci "Answer briefly.";
  let agent = make () in
  send agent "one";
  send agent "two";
  [%expect
    {|
    ok ci1 -: Use Australian spelling.
    ok ci2 [Maths]: Keep one note on my maths.
    ok ci3 -: Answer briefly.
    system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep one note on my maths.
    [ci3] Answer briefly.
    one: nothing told
    system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep one note on my maths.
    [ci3] Answer briefly.
    two: nothing told
    |}];
  add ci "Prefer OCaml.";
  print_result (Custom_instructions.remove ci "ci3");
  send agent "three";
  send agent "four";
  [%expect
    {|
    ok ci4 -: Prefer OCaml.
    ok ci3 -: Answer briefly.
    system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep one note on my maths.
    [ci3] Answer briefly.
    three: Custom instructions: the model was told 1 added, 1 removed
    [Custom instructions changed (the user's standing instructions; follow them; the others are unchanged):
    Removed [ci3]
    Added [ci4] Prefer OCaml.]
    system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep one note on my maths.
    [ci3] Answer briefly.
    four: nothing told
    |}];
  (* Edited then the backend restarts before the next message. *)
  print_result
    (Custom_instructions.edit
       ci
       ~id:"ci1"
       ~text:"Use British spelling."
       ~now:(day 1.)
       ());
  let agent = restart agent in
  send agent "five";
  let agent = restart agent in
  send agent "six";
  [%expect
    {|
    ok ci1 -: Use British spelling.
    system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep one note on my maths.
    [ci3] Answer briefly.
    five: Custom instructions: the model was told 1 edited
    [Custom instructions changed (the user's standing instructions; follow them; the others are unchanged):
    Edited, now [ci1] Use British spelling.]
    system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    [ci2] (Maths) Keep one note on my maths.
    [ci3] Answer briefly.
    six: nothing told
    |}]
;;

let%expect_test
    "the session's own changes are not told back; another session's are"
  =
  with_agents
    ~replies:
      [ Reply.text "ok"
      ; Reply.tool_call
          ~id:"c1"
          ~name:"custom_instructions"
          ~arguments:{|{"action":"add","text":"Use Australian spelling."}|}
          ()
      ; Reply.text "Noted: I'll use Australian spelling from now on."
      ; Reply.text "ok"
      ; Reply.text "ok"
      ; Reply.tool_call
          ~id:"c2"
          ~name:"custom_instructions"
          ~arguments:
            {|{"action":"edit","id":"ci1","text":"Use British spelling."}|}
          ()
      ; Reply.text "Changed it."
      ; Reply.text "ok"
      ; Reply.text "ok"
      ; Reply.text "ok"
      ]
  @@ fun _t ci ~make ~restart:_ ~send ~requests:_ ->
  let agent = make () in
  let other = make () in
  send other "before";
  send agent "I'm Australian: always use our spelling.";
  send other "hello";
  [%expect
    {|
    system: no custom instructions
    before: nothing told
    system: no custom instructions
    I'm Australian: always use our spelling.: nothing told
    system: no custom instructions
    hello: Custom instructions: the model was told 1 added
    [Custom instructions (the user's standing instructions; follow them) are now:
    [ci1] Use Australian spelling.
    These replace any earlier custom instructions.]
    |}];
  send agent "two";
  send agent "Actually British spelling.";
  send other "three";
  send agent "four";
  [%expect
    {|
    system: no custom instructions
    two: nothing told
    system: no custom instructions
    Actually British spelling.: nothing told
    system: no custom instructions
    three: Custom instructions: the model was told 1 edited
    [Custom instructions (the user's standing instructions; follow them) are now:
    [ci1] Use British spelling.
    These replace any earlier custom instructions.]
    system: no custom instructions
    four: nothing told
    |}];
  (* The user's change still reaches the session that made the others. *)
  print_result (Custom_instructions.remove ci "ci1");
  send agent "five";
  [%expect
    {|
    ok ci1 -: Use British spelling.
    system: no custom instructions
    five: Custom instructions: the model was told 1 removed
    [Custom instructions: the user has no standing instructions any more: none of the earlier ones apply.]
    |}]
;;

let%expect_test "off: not offered, still in the system prompt" =
  with_agents
  @@ fun _t ci ~make ~restart:_ ~send:_ ~requests ->
  add ci "Answer briefly.";
  Or_error.ok_exn (Custom_instructions.set_enabled ci false);
  let agent = make () in
  let offered () =
    let request : Provider.Request.t = Queue.last_exn requests in
    print_s
      [%sexp
        (List.exists request.tools ~f:(fun (s : Tool_spec.t) ->
           String.equal s.name Tool_custom_instructions.name)
         : bool)];
    let system = Option.value request.system ~default:"" in
    print_s
      [%sexp
        (String.is_substring system ~substring:"[ci1] Answer briefly." : bool)
      , (String.is_substring system ~substring:"- Custom instructions:" : bool)];
    Queue.clear requests
  in
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.wait_idle agent;
  offered ();
  Or_error.ok_exn (Custom_instructions.set_enabled ci true);
  Or_error.ok_exn (Agent.prompt agent "two");
  Agent.wait_idle agent;
  offered ();
  (* A new session, offered the tool: told when to use it. *)
  let agent = make () in
  Or_error.ok_exn (Agent.prompt agent "three");
  Agent.wait_idle agent;
  offered ();
  let system = Option.value_exn (Session.system_prompt (Agent.session agent)) in
  List.iter (String.split_lines system) ~f:(fun line ->
    if String.is_prefix line ~prefix:"- Custom instructions:"
    then print_endline line);
  [%expect
    {|
    ok ci1 -: Answer briefly.
    false
    (true false)
    true
    (true false)
    true
    (true true)
    - Custom instructions: when the user states a lasting preference, fact about themselves or standing instruction for every session that they do not cover yet, add it with custom_instructions (or edit the one that covers it) and say so briefly; never for task details or one-off requests. Memory is for what is looked up only when needed.
    |}]
;;

let%expect_test "a subagent's own system prompt has them" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let ci = store t in
  add ci "Use Australian spelling.";
  let requests = Queue.create () in
  let provider =
    Routed_provider.create
      ~on_request:(fun ~key r -> Queue.enqueue requests (key, r))
      ~main:
        [ Reply.tool_call
            ~id:"c1"
            ~name:"subagent"
            ~arguments:{|{"task":"look around"}|}
            ()
        ; Reply.text "started"
        ; Reply.text "thanks"
        ]
      [ "look around", [ Reply.text "looked" ] ]
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:
        (Tools.all
         @ [ Tool_subagent.create
               ~provider
               ~custom_instructions:(fun () -> Custom_instructions.for_model ci)
               ~current_model:(fun () -> Model.default)
               ~current_thinking:(fun () -> Off)
               ~home:t.dir
               ()
           ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~custom_instructions:ci
      ~cwd:t.dir
      ()
  in
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  while Agent.has_running_subagents agent || Agent.is_running agent do
    Agent.wait_idle agent;
    Eio.Fiber.yield ()
  done;
  Queue.iter requests ~f:(fun (key, (r : Provider.Request.t)) ->
    if String.equal key "look around"
    then (
      let system = Option.value r.system ~default:"" in
      match String.substr_index system ~pattern:"Custom instructions (" with
      | Some i ->
        printf "subagent's system: … %s\n" (String.drop_prefix system i)
      | None -> print_endline "subagent's system: none"));
  [%expect
    {|
    ok ci1 -: Use Australian spelling.
    subagent's system: … Custom instructions (the user's standing instructions; follow them):
    [ci1] Use Australian spelling.
    |}];
  let note =
    { Injected.Note.kind =
        Custom_instructions { added = 1; edited = 0; removed = 0 }
    ; text = "[…]"
    }
  in
  print_endline
    (Json.to_string
       (Injected.to_json
          { Injected.empty with notes = [ note ] }
          ~text:(Injected.prefix_notes [ note ] "hello")));
  [%expect
    {| {"body":"hello","notes":[{"kind":"custom_instructions","title":"Custom instructions: the model was told 1 added","text":"[…]"}]} |}]
;;

let rec undated : Jsonaf.t -> Jsonaf.t = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         ( key
         , if String.equal key "saved" then `String "$SAVED" else undated value
         )))
  | `Array items -> `Array (List.map items ~f:undated)
  | json -> json
;;

let%expect_test "RPC: get, add, edit, remove, on and off; every client hears" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
      ()
  in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~home:t.dir
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client name =
    Rpc_server.connect server ~send:(fun json ->
      match Json.member "event" json with
      | Some (`String "custom_instructions_changed") ->
        printf "%s hears %s\n" name (Json.to_string (undated json))
      | _ -> ())
  in
  let a = client "a" in
  let b = client "b" in
  let call client meth params =
    let response =
      Rpc_server.handle
        server
        client
        (`Object [ "id", `Number "1"; "method", `String meth; "params", params ])
    in
    match Json.member "ok" response, Json.member "result" response with
    | Some `True, Some result -> print_endline (Json.to_string (undated result))
    | _ ->
      print_endline
        (Json.to_string (Option.value_exn (Json.member "error" response)))
  in
  call a "hello" (`Object []);
  call b "hello" (`Object []);
  ignore ([%expect.output] : string);
  call a "get_custom_instructions" (`Object []);
  [%expect
    {| {"enabled":true,"notes":[],"limits":{"text":1000,"title":80,"count":50}} |}];
  call
    a
    "save_custom_instruction"
    (`Object
        [ "title", `String "Spelling"
        ; "text", `String "Use Australian spelling."
        ]);
  [%expect
    {|
    a hears {"type":"event","event":"custom_instructions_changed","enabled":true,"notes":[{"id":"ci1","title":"Spelling","text":"Use Australian spelling.","saved":"$SAVED"}],"limits":{"text":1000,"title":80,"count":50}}
    b hears {"type":"event","event":"custom_instructions_changed","enabled":true,"notes":[{"id":"ci1","title":"Spelling","text":"Use Australian spelling.","saved":"$SAVED"}],"limits":{"text":1000,"title":80,"count":50}}
    {"note":{"id":"ci1","title":"Spelling","text":"Use Australian spelling.","saved":"$SAVED"}}
    |}];
  (* An edit without a title removes it. *)
  call
    b
    "save_custom_instruction"
    (`Object [ "id", `String "ci1"; "text", `String "Use British spelling." ]);
  call
    a
    "save_custom_instruction"
    (`Object [ "id", `String "ci7"; "text", `String "x" ]);
  call
    a
    "save_custom_instruction"
    (`Object [ "text", `String (String.make 1001 'x') ]);
  [%expect
    {|
    a hears {"type":"event","event":"custom_instructions_changed","enabled":true,"notes":[{"id":"ci1","text":"Use British spelling.","saved":"$SAVED"}],"limits":{"text":1000,"title":80,"count":50}}
    b hears {"type":"event","event":"custom_instructions_changed","enabled":true,"notes":[{"id":"ci1","text":"Use British spelling.","saved":"$SAVED"}],"limits":{"text":1000,"title":80,"count":50}}
    {"note":{"id":"ci1","text":"Use British spelling.","saved":"$SAVED"}}
    "no custom instruction \"ci7\"; they are ci1"
    "an instruction is limited to 1000 bytes (this one has 1001): they are in the context of every session, so keep each to the point (and what is only sometimes needed in memory)"
    |}];
  call a "set_custom_instructions_enabled" (`Object [ "enabled", `False ]);
  call a "remove_custom_instruction" (`Object [ "id", `String "ci1" ]);
  [%expect
    {|
    a hears {"type":"event","event":"custom_instructions_changed","enabled":false,"notes":[{"id":"ci1","text":"Use British spelling.","saved":"$SAVED"}],"limits":{"text":1000,"title":80,"count":50}}
    b hears {"type":"event","event":"custom_instructions_changed","enabled":false,"notes":[{"id":"ci1","text":"Use British spelling.","saved":"$SAVED"}],"limits":{"text":1000,"title":80,"count":50}}
    {"enabled":false}
    a hears {"type":"event","event":"custom_instructions_changed","enabled":false,"notes":[],"limits":{"text":1000,"title":80,"count":50}}
    b hears {"type":"event","event":"custom_instructions_changed","enabled":false,"notes":[],"limits":{"text":1000,"title":80,"count":50}}
    {"note":{"id":"ci1","text":"Use British spelling.","saved":"$SAVED"}}
    |}]
;;
