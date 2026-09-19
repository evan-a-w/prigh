open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let subagent ~provider =
  Tool_subagent.create
    ~provider
    ~current_model:(fun () -> Model.default)
    ~current_thinking:(fun () -> Off)
    ~home:"/nonexistent"
;;

let run_tool t ~tools ~emit tool args =
  let context = Tool.Context.create ~env:t.env ~cwd:t.dir ~tools ~emit () in
  Tool.execute tool context (Jsonaf.of_string args)
;;

let%expect_test "tools subset is passed to the child; unknown names error" =
  with_sandbox
  @@ fun t ->
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        requests := List.map r.tools ~f:(fun s -> s.name) :: !requests)
      [ Reply.text "read done" ]
  in
  let subagent = subagent ~provider in
  let tools = Tools.all @ [ subagent ] in
  let result =
    run_tool
      t
      ~tools
      ~emit:ignore
      subagent
      {|{"task":"read a file","tools":["read"]}|}
  in
  print_s [%sexp (result : Tool.Result.t)];
  print_s [%sexp (List.rev !requests : string list list)];
  let result =
    run_tool t ~tools ~emit:ignore subagent {|{"task":"x","tools":["nope"]}|}
  in
  print_s [%sexp (result : Tool.Result.t)];
  [%expect
    {|
    ((text  "read done\
           \n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]")
     (is_error false))
    ((read))
    ((text
      "invalid arguments: unknown tool \"nope\"; valid tools: bash, read, write, edit, ls, grep, find, subagent")
     (is_error true))
    |}]
;;

let%expect_test "model override and bad model" =
  with_sandbox
  @@ fun t ->
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r -> requests := r.model.id :: !requests)
      [ Reply.text "ok" ]
  in
  let subagent = subagent ~provider in
  let tools = Tools.all @ [ subagent ] in
  let (_ : Tool.Result.t) =
    run_tool t ~tools ~emit:ignore subagent {|{"task":"x","model":"gpt-4"}|}
  in
  print_s
    [%sexp (Model.default.id : string), (List.rev !requests : string list)];
  let result =
    run_tool t ~tools ~emit:ignore subagent {|{"task":"x","model":"gpt-9"}|}
  in
  print_s [%sexp (result : Tool.Result.t)];
  [%expect
    {|
    (deepseek-flash (gpt-4))
    ((text
      "invalid arguments: unknown model \"gpt-9\"; did you mean: openai/gpt-4 (GPT-4), openai/gpt-5 (GPT-5), openai/gpt-4o (GPT-4o)")
     (is_error true))
    |}]
;;

let%expect_test "a subagent may delegate once; the grandchild cannot" =
  with_sandbox
  @@ fun t ->
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        requests := List.map r.tools ~f:(fun s -> s.name) :: !requests)
      [ Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"inner"}|}
          ()
      ; Reply.tool_call
          ~id:"c2"
          ~name:"subagent"
          ~arguments:{|{"task":"innermost"}|}
          ()
      ; Reply.text "grandchild done"
      ; Reply.text "child done"
      ; Reply.text "main done"
      ]
  in
  let subagent = subagent ~provider in
  let ends = ref [] in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:
        { model = Model.default
        ; thinking = Off
        ; system = None
        ; tools = Tools.all @ [ subagent ]
        ; max_turns = None
        ; max_tokens = None
        ; retries = 0
        }
      ~cwd:t.dir
      ~emit:(fun event ->
        match event with
        | Agent_event.Subagent_end { agent_id; usage; turns; _ } ->
          ends := (agent_id, usage, turns) :: !ends
        | _ -> ())
      ~context:[]
      ~prompts:[ Message.user "go" ]
      ()
  in
  print_s [%sexp (List.rev !requests : string list list)];
  print_s [%sexp (List.length added : int)];
  (* Only the depth-1 child's [Subagent_end] reaches the top level; its usage
     already includes the grandchild's. *)
  print_s [%sexp (List.rev !ends : (string * Usage.t * int) list)];
  [%expect
    {|
    ((bash read write edit ls grep find subagent)
     (bash read write edit ls grep find subagent)
     (bash read write edit ls grep find)
     (bash read write edit ls grep find subagent)
     (bash read write edit ls grep find subagent))
    4
    ((c1 ((input 40) (output 18) (cache_read 5)) 2))
    |}]
;;

let%expect_test "Agent.state rolls up subagent usage and cost" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    Faux_provider.create
      [ Reply.tool_call
          ~id:"p1"
          ~name:"subagent"
          ~arguments:{|{"task":"look around"}|}
          ()
      ; Reply.text "child report"
      ; Reply.text "main report"
      ]
  in
  let subagent = subagent ~provider in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ [ subagent ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(fun event ->
    match event with
    | Loop (Subagent_start { agent_id; task; model; tools; _ }) ->
      print_s
        [%sexp
          (agent_id : string)
        , (task : string)
        , (model : string)
        , (tools : string list)]
    | Loop (Subagent_end { agent_id; usage; turns; cost_usd; _ }) ->
      print_s
        [%sexp
          (agent_id : string)
        , (usage : Usage.t)
        , (turns : int)
        , (cost_usd : float)]
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  let state = Agent.state agent in
  print_s [%sexp (state.usage : Usage.t), (state.cost_usd : float)];
  [%expect
    {|
    (p1 "look around" deepseek-flash
     (bash read write edit ls grep find subagent))
    (p1 ((input 10) (output 5) (cache_read 0)) 1 9E-06)
    (((input 40) (output 18) (cache_read 5)) 3.213E-05)
    |}]
;;

let%expect_test "Rpc_json encodes nested subagent events" =
  let call : Content.Tool_call.t =
    { id = "c1"; name = "read"; arguments = "{}" }
  in
  let nested =
    Agent_event.Subagent
      { call_id = "p"
      ; agent_id = "p"
      ; event =
          Agent_event.Subagent
            { call_id = "c1"
            ; agent_id = "p/c1"
            ; event = Agent_event.Tool_start call
            }
      }
  in
  print_endline (Json.to_string (Rpc_json.event (Agent.Event.Loop nested)));
  let start =
    Agent_event.Subagent_start
      { call_id = "p"
      ; agent_id = "p"
      ; task = "go"
      ; model = "m"
      ; tools = [ "read"; "ls" ]
      }
  in
  print_endline (Json.to_string (Rpc_json.event (Agent.Event.Loop start)));
  let ended =
    Agent_event.Subagent_end
      { call_id = "p"
      ; agent_id = "p"
      ; usage = { input = 3; output = 4; cache_read = 1 }
      ; turns = 2
      ; cost_usd = 0.001
      ; result = Tool.Result.ok "done"
      }
  in
  print_endline (Json.to_string (Rpc_json.event (Agent.Event.Loop ended)));
  [%expect
    {|
    {"type":"event","event":"subagent","call_id":"p","agent_id":"p","inner":{"type":"event","event":"subagent","call_id":"c1","agent_id":"p/c1","inner":{"type":"event","event":"tool_start","call":{"id":"c1","name":"read","arguments":"{}"}}}}
    {"type":"event","event":"subagent_start","call_id":"p","agent_id":"p","task":"go","model":"m","tools":["read","ls"]}
    {"type":"event","event":"subagent_end","call_id":"p","agent_id":"p","usage":{"input":3,"output":4,"cache_read":1},"turns":2,"cost_usd":0.001,"result":{"text":"done","is_error":false}}
    |}]
;;
