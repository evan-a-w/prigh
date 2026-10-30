open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let%expect_test "Naming.clean: one plain line, sentence case, cut at a word" =
  List.iter
    [ "fix the flaky session reload test"
    ; "**Title:** \"Add OAuth login to the CLI.\""
    ; "Session title: `Review menu keyboard navigation`"
    ; "\n\n# Port the parser to Eio\n\nBecause..."
    ; "Name: survey host naming code!"
    ; "a very long title that goes on and on about many different things until \
       it is cut"
    ; "\"\""
    ]
    ~f:(fun s -> printf "%S\n" (Naming.clean s));
  [%expect
    {|
    "Fix the flaky session reload test"
    "Add OAuth login to the CLI"
    "Review menu keyboard navigation"
    "Port the parser to Eio"
    "Survey host naming code"
    "A very long title that goes on and on about many different\226\128\166"
    ""
    |}]
;;

let%expect_test "Subagent_name.of_task: the gist of the task, not its preamble" =
  List.iter
    [ "Implement on a branch `fix/completion`: the slash-command argument \
       autocomplete must close once the user types a space. Find the logic and \
       fix it."
    ; "Review the diff of PR #12 for correctness. Report back concisely."
    ; "  \n\nsurvey how hosts are named\nmore lines"
    ; "Task: x"
    ; ""
    ]
    ~f:(fun s -> printf "%S\n" (Subagent_name.of_task s));
  [%expect
    {|
    "The slash-command argument autocomplete must\226\128\166"
    "Review the diff of PR #12 for correctness"
    "Survey how hosts are named"
    "Task: x"
    "Subagent"
    |}]
;;

let%expect_test "Subagent_name.prompt: the task and its siblings' names" =
  print_endline Subagent_name.instructions;
  print_endline "---";
  print_endline
    (Subagent_name.prompt
       ~task:"Find where hosts get their names."
       ~siblings:[ "Fix completion after a space"; "Redesign the review menu" ]);
  [%expect
    {|
    You name the subagents a coding agent starts, for the user's list of them. Write a name of two to five words saying what this subagent does, in the imperative and sentence case, e.g. "Fix completion after a space", "Review the parser changes", "Survey host naming code". Make it differ from the other subagents' names, if any are listed: say what sets this one apart. Reply with the name only: no quotes, no trailing period, no preamble.
    ---
    Task:
    Find where hosts get their names.

    Other subagents:
    - Fix completion after a space
    - Redesign the review menu
    |}]
;;

let%expect_test
    "Session_description.conversation: what was typed and replied, the first \
     request and the latest turns"
  =
  let reply text =
    Message.Assistant
      { content = [ Text text ]
      ; usage = Usage.zero
      ; stop_reason = End_turn
      ; model = "m"
      }
  in
  let long = String.concat ~sep:" " (List.init 400 ~f:(fun _ -> "word")) in
  let messages =
    [ Message.user "Please make the review menu keyboard driven"
    ; reply "I'll look at review_view.ml first."
    ]
    @ List.concat
        (List.init 30 ~f:(fun i ->
           [ Message.user (sprintf "follow-up %d: %s" i long); reply "ok" ]))
  in
  let text =
    Session_description.conversation ~current:"Review menu keys" messages
  in
  printf "%d chars\n" (String.length text);
  String.split_lines text
  |> List.filter ~f:(Fn.non String.is_empty)
  |> List.map ~f:(fun l -> String.prefix l 60)
  |> List.iter ~f:print_endline;
  [%expect
    {|
    7745 chars
    User: Please make the review menu keyboard driven
    [50 messages omitted]
    Assistant: ok
    User: follow-up 25: word word word word word word word word
    Assistant: ok
    User: follow-up 26: word word word word word word word word
    Assistant: ok
    User: follow-up 27: word word word word word word word word
    Assistant: ok
    User: follow-up 28: word word word word word word word word
    Assistant: ok
    User: follow-up 29: word word word word word word word word
    Assistant: ok
    Current title: Review menu keys
    |}]
;;

let with_naming_agent ?naming_model replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create ~on_request:(Queue.enqueue requests) replies
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~auto_describe:true
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Notice n -> print_endline ("notice: " ^ n)
    | _ -> ());
  Option.iter naming_model ~f:(fun name ->
    match
      Agent.set_config
        agent
        { (Agent.config agent) with naming_model = Some name }
    with
    | Ok () -> ()
    | Error e -> print_endline ("set_config: " ^ Error.to_string_hum e));
  let show_requests () =
    Queue.iter requests ~f:(fun (r : Provider.Request.t) ->
      let system = Option.value r.system ~default:"" in
      printf
        "%s %s\n"
        (Model.key r.model)
        (if String.is_prefix system ~prefix:"You are prigh"
         then "(the conversation)"
         else String.prefix system 40));
    Queue.clear requests
  in
  f agent show_requests
;;

let%expect_test "the naming model titles the session; unknown ones are refused" =
  with_naming_agent
    ~naming_model:"deepseek-v4-pro"
    [ Reply.text "Sure."; Reply.text "Fix the parser's error messages" ]
  @@ fun agent show_requests ->
  printf
    "naming_model: %s\n"
    (Option.value ~default:"-" (Agent.config agent).naming_model);
  (match
     Agent.set_config
       agent
       { (Agent.config agent) with naming_model = Some "no-such-model" }
   with
   | Ok () -> print_endline "accepted"
   | Error e -> print_endline (Error.to_string_hum e));
  Or_error.ok_exn (Agent.prompt agent "the parser's errors are unreadable");
  Agent.wait_idle agent;
  show_requests ();
  print_s [%sexp (Session.description (Agent.session agent) : string option)];
  [%expect
    {|
    naming_model: deepseek/deepseek-v4-pro
    naming_model: unknown model "no-such-model"; did you mean: openai/gpt-5.3-codex (GPT-5.3 Codex), openai/gpt-5.6-sol (GPT-5.6 Sol), openai/gpt-6.1-sol (GPT-6.1 Sol)
    deepseek/deepseek-flash (the conversation)
    deepseek/deepseek-v4-pro You name the sessions of a coding agent
    ("Fix the parser's error messages")
    |}]
;;

let%expect_test
    "a naming model that fails hands over to the session's, saying what to fix"
  =
  with_naming_agent
    ~naming_model:"deepseek-v4-pro"
    [ Reply.text "Sure."
    ; Reply.error "model overloaded"
    ; Reply.text "Speed up the test suite"
    ]
  @@ fun agent show_requests ->
  Or_error.ok_exn (Agent.prompt agent "the tests are slow");
  Agent.wait_idle agent;
  show_requests ();
  print_s [%sexp (Session.description (Agent.session agent) : string option)];
  [%expect
    {|
    notice: naming the session failed: model overloaded with deepseek/deepseek-v4-pro (change it with /naming model, or /naming model off); trying the session's model
    deepseek/deepseek-flash (the conversation)
    deepseek/deepseek-v4-pro You name the sessions of a coding agent
    deepseek/deepseek-flash You name the sessions of a coding agent
    ("Speed up the test suite")
    |}]
;;

let%expect_test "the session is named again once it has said more" =
  with_naming_agent
    [ Reply.text "one"
    ; Reply.text "Look at a bug"
    ; Reply.text "two"
    ; Reply.text "three"
    ; Reply.text "Fix login redirect loop"
    ; Reply.text "four"
    ]
  @@ fun agent show_requests ->
  List.iter
    [ "a bug"; "it's in login"; "it loops on redirect"; "thanks" ]
    ~f:(fun p ->
      Or_error.ok_exn (Agent.prompt agent p);
      Agent.wait_idle agent;
      print_s
        [%sexp (Session.description (Agent.session agent) : string option)]);
  show_requests ();
  [%expect
    {|
    ("Look at a bug")
    ("Look at a bug")
    ("Fix login redirect loop")
    ("Fix login redirect loop")
    deepseek/deepseek-flash (the conversation)
    deepseek/deepseek-flash You name the sessions of a coding agent
    deepseek/deepseek-flash (the conversation)
    deepseek/deepseek-flash (the conversation)
    deepseek/deepseek-flash You name the sessions of a coding agent
    deepseek/deepseek-flash (the conversation)
    |}]
;;

let%expect_test "name_subagent: by the naming model, told its siblings' names" =
  with_naming_agent
    ~naming_model:"deepseek-v4-pro"
    [ Reply.text "\"Survey host naming.\""
    ; Reply.error "down"
    ; Reply.error "down"
    ]
  @@ fun agent show_requests ->
  let name () =
    Agent.name_subagent
      agent
      ~task:"Find where hosts get their names."
      ~siblings:[ "Fix completion after a space" ]
      ~cancel:Cancellation.never
  in
  print_s [%sexp (name () : string option)];
  (* Both models fail: no name, and the caller falls back to the task's. *)
  print_s [%sexp (name () : string option)];
  show_requests ();
  [%expect
    {|
    ("Survey host naming")
    notice: naming the subagent failed: down with deepseek/deepseek-v4-pro (change it with /naming model, or /naming model off); trying the session's model
    ()
    deepseek/deepseek-v4-pro You name the subagents a coding agent st
    deepseek/deepseek-v4-pro You name the subagents a coding agent st
    deepseek/deepseek-flash You name the subagents a coding agent st
    |}]
;;
