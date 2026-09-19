open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let subagent ~provider =
  Tool_subagent.create
    ~provider
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
;;

let%expect_test "roles and tool sets" =
  print_s
    [%sexp
      (List.map Tool_subagent.Role.all ~f:(fun r ->
         ( Tool_subagent.Role.to_string r
         , List.map (Tool_subagent.Role.tools r) ~f:Tool.name ))
       : (string * string list) list)];
  [%expect
    {|
    ((explore (read ls grep find)) (worker (bash read write edit ls grep find)))
    |}]
;;

let%expect_test
    "explore subagent investigates and reports; progress is streamed; requests \
     are isolated"
  =
  with_sandbox
  @@ fun t ->
  write t "src/a.ml" "let answer = 42\n";
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        requests
        := ( List.length r.messages
           , List.map r.tools ~f:(fun s -> s.name)
           , String.prefix (Option.value r.system ~default:"") 30 )
           :: !requests)
      [ Reply.tool_call
          ~id:"s1"
          ~name:"grep"
          ~arguments:{|{"pattern":"answer"}|}
          ()
      ; Reply.text "The answer is defined in src/a.ml line 1."
      ]
  in
  let progress = ref [] in
  run
    t
    ~on_output:(fun s -> progress := s :: !progress)
    (subagent ~provider)
    {|{"role": "explore", "task": "find where answer is defined"}|};
  [%expect
    {|
    The answer is defined in src/a.ml line 1.
    [subagent used 2 turns]
    |}];
  print_s [%sexp (List.rev !progress : string list)];
  print_s [%sexp (List.rev !requests : (int * string list * string) list)];
  [%expect
    {|
    ("[explore] grep {\"pattern\":\"answer\"}\n")
    ((1 (read ls grep find) "You are a read-only research s")
     (3 (read ls grep find) "You are a read-only research s"))
    |}]
;;

let%expect_test "worker subagent errors, bad role, abort" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create [ Reply.text ~stop_reason:(Error "HTTP 401: nope") "" ]
  in
  run t (subagent ~provider) {|{"role": "worker", "task": "do it"}|};
  [%expect
    {|
    ERROR: subagent failed: HTTP 401: nope
    |}];
  run t (subagent ~provider) {|{"role": "manager", "task": "do it"}|};
  [%expect {| ERROR: invalid arguments: role must be explore or worker |}];
  let cancel = Cancellation.create () in
  Cancellation.cancel cancel;
  let provider = Faux_provider.create [ Reply.text "unreachable" ] in
  run t ~cancel (subagent ~provider) {|{"role": "worker", "task": "do it"}|};
  [%expect {| ERROR: subagent aborted |}]
;;

let%expect_test "turn limit" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      (List.init (Tool_subagent.max_turns + 5) ~f:(fun i ->
         Reply.tool_call ~id:(sprintf "c%d" i) ~name:"ls" ~arguments:"{}" ()))
  in
  run t (subagent ~provider) {|{"role": "explore", "task": "loop"}|};
  [%expect {| ERROR: subagent hit the 40-turn limit without finishing |}]
;;

let%expect_test "nested through the parent loop" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      [ Reply.tool_call
          ~id:"p1"
          ~name:"subagent"
          ~arguments:{|{"role":"explore","task":"look around"}|}
          ()
      ; Reply.text "nothing here"
      ; Reply.text "The subagent found nothing."
      ]
  in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:
        { model = Model.default
        ; thinking = Off
        ; system = None
        ; tools = Tools.all @ [ subagent ~provider ]
        ; max_turns = None
        ; max_tokens = None
        ; retries = 0
        }
      ~cwd:t.dir
      ~context:[]
      ~prompts:[ Message.user "explore this" ]
      ()
  in
  print_s
    [%sexp
      (List.map added ~f:(function
         | Message.User u -> "user: " ^ u.text
         | Assistant a -> "assistant: " ^ Message.Assistant.text a
         | Tool_result r -> "tool_result: " ^ r.text)
       : string list)];
  [%expect
    {|
    ("user: explore this" "assistant: "
      "tool_result: nothing here\
     \n[subagent used 1 turns]" "assistant: The subagent found nothing.")
    |}]
;;
