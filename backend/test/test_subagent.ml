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
    ()
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
      "invalid arguments: unknown tool \"nope\"; valid tools: bash, read, write, edit, ls, grep, find, web_fetch, subagent")
     (is_error true))
    |}]
;;

let%expect_test
    "the child's system prompt says how to get tools when the host has nix"
  =
  with_sandbox
  @@ fun t ->
  List.iter [ true; false ] ~f:(fun nix ->
    let system = ref "" in
    let provider =
      Faux_provider.create
        ~on_request:(fun r -> system := Option.value r.system ~default:"")
        [ Reply.text "done" ]
    in
    let subagent = subagent ~provider in
    with_nix_on_path t ~nix (fun () ->
      let (_ : Tool.Result.t) =
        run_tool
          t
          ~tools:(Tools.all @ [ subagent ])
          ~emit:ignore
          subagent
          {|{"task":"x"}|}
      in
      ());
    printf
      "nix on PATH: %b, prompt mentions nixpkgs: %b\n"
      nix
      (String.is_substring !system ~substring:"nix shell nixpkgs#"));
  [%expect
    {|
    nix on PATH: true, prompt mentions nixpkgs: true
    nix on PATH: false, prompt mentions nixpkgs: false
    |}]
;;

let%expect_test
    "the child gets the file editor of its model, whichever editor is named"
  =
  with_sandbox
  @@ fun t ->
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        requests
        := (r.model.id, List.map r.tools ~f:(fun s -> s.name)) :: !requests)
      [ Reply.text "a"; Reply.text "b"; Reply.text "c" ]
  in
  let subagent = subagent ~provider in
  List.iter
    [ Tools.all, {|{"task":"x","model":"openai/gpt-5","tools":["read","edit"]}|}
    ; ( Tools.for_model (Option.value_exn (Model.find "openai/gpt-5")) Tools.all
      , {|{"task":"x","model":"anthropic/claude-opus-4-6","tools":["apply_patch"]}|}
      )
    ; Tools.all, {|{"task":"x","model":"openai/gpt-5"}|}
    ]
    ~f:(fun (tools, args) ->
      let (_ : Tool.Result.t) =
        run_tool t ~tools:(tools @ [ subagent ]) ~emit:ignore subagent args
      in
      ());
  print_s [%sexp (List.rev !requests : (string * string list) list)];
  [%expect
    {|
    ((gpt-5 (read apply_patch)) (claude-opus-4-6 (edit))
     (gpt-5
      (bash read write apply_patch ls grep find web_fetch web_search subagent)))
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
        ; cache_key = None
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
    ((bash read write edit ls grep find web_fetch web_search subagent)
     (bash read write edit ls grep find web_fetch subagent)
     (bash read write edit ls grep find web_fetch)
     (bash read write edit ls grep find web_fetch subagent)
     (bash read write edit ls grep find web_fetch web_search subagent))
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
    Routed_provider.create
      ~main:
        [ Reply.tool_call
            ~id:"p1"
            ~name:"subagent"
            ~arguments:{|{"task":"look around"}|}
            ()
        ; Reply.text "main report"
        ; Reply.text "got the report"
        ]
      [ "look around", [ Reply.text "child report" ] ]
  in
  let subagent = subagent ~provider in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:(Tools.all @ (subagent :: Tool_subagent.control_tools))
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
    (a1 "look around" deepseek-flash
     (bash read write edit ls grep find web_fetch subagent))
    (a1 ((input 10) (output 5) (cache_read 0)) 1 9E-06)
    (((input 50) (output 23) (cache_read 5)) 4.113E-05)
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

let%expect_test
    "Rpc_json: nested messages carry the event's time, as do get_subagent's"
  =
  let message_end =
    Agent_event.Subagent
      { call_id = "p"; agent_id = "p"; event = Message_end (Message.user "go") }
  in
  print_endline
    (Json.to_string
       (Rpc_json.event ~now:1791210730.25 (Agent.Event.Loop message_end)));
  let log = Subagent_log.create () in
  Subagent_log.record
    log
    ~now:1791210700.
    (Subagent_start
       { call_id = "p"; agent_id = "p"; task = "go"; model = "m"; tools = [] });
  Subagent_log.record log ~now:1791210730.25 message_end;
  print_endline
    (Json.to_string
       (match Subagent_log.find log "p" with
        | Some (summary, messages) ->
          (match Rpc_json.subagent (summary, messages) with
           | `Object fields ->
             List.Assoc.find_exn fields "messages" ~equal:String.equal
           | json -> json)
        | None -> `Null));
  [%expect
    {|
    {"type":"event","event":"subagent","call_id":"p","agent_id":"p","inner":{"type":"event","event":"message_end","message":{"role":"user","text":"go","at":1791210730250}}}
    [{"role":"user","text":"go","at":1791210730250}]
    |}]
;;

(* What the child sees after its previous turn: the tool results and any
   delivered reports. *)
let print_since_last_turn (r : Provider.Request.t) =
  let rec after_last_assistant acc = function
    | [] -> acc
    | Message.Assistant _ :: _ -> acc
    | m :: rest -> after_last_assistant (m :: acc) rest
  in
  List.iter
    (after_last_assistant [] (List.rev r.messages))
    ~f:(function
      | Message.Tool_result { tool_name; text; _ } ->
        printf "  %s -> %s\n" tool_name (String.prefix text 200)
      | User u -> printf "  user: %s\n" (String.prefix u.text 200)
      | Assistant _ -> ());
  print_endline "--"
;;

let%expect_test
    "a subagent that ends its turn while its job runs is woken by its report \
     and finishes only then"
  =
  with_sandbox
  @@ fun t ->
  let first_tools = ref None in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        if Option.is_none !first_tools
        then first_tools := Some (List.map r.tools ~f:(fun s -> s.name));
        print_since_last_turn r)
      [ Reply.tool_call
          ~id:"c1"
          ~name:"bash"
          ~arguments:{|{"command":"sleep 0.2; echo built","background":true}|}
          ()
      ; Reply.text "waiting for the build"
      ; Reply.text "built it"
      ]
  in
  let subagent = subagent ~provider in
  let result =
    run_tool
      t
      ~tools:(Tools.all @ Tool_jobs.tools @ [ subagent ])
      ~emit:ignore
      subagent
      {|{"task":"build"}|}
  in
  print_s [%sexp (!first_tools : string list option)];
  print_s [%sexp (result : Tool.Result.t)];
  [%expect
    {|
      user: build
    --
      bash -> started job j1: sleep 0.2; echo built; its output is being recorded; you will be notified when it exits, even after you end your turn (you finish only once your jobs have exited). Use job_output to pe
    --
      user: [job j1 exited 0] sleep 0.2; echo built
    built
    --
    ((bash read write edit ls grep find web_fetch job_status job_output job_input
      job_kill subagent))
    ((text  "built it\
           \n[subagent: 3 turns, 40 in / 18 out tokens, $0.0000]")
     (is_error false))
    |}]
;;

let%expect_test
    "a job's report reaches the subagent between turns; jobs left running are \
     killed when it runs out of turns"
  =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      ~on_request:print_since_last_turn
      [ Reply.tool_call
          ~id:"c1"
          ~name:"bash"
          ~arguments:{|{"command":"echo quick","background":true}|}
          ()
      ; Reply.tool_call
          ~id:"c2"
          ~name:"bash"
          ~arguments:{|{"command":"sleep 0.3","background":false}|}
          ()
      ; Reply.tool_call
          ~id:"c3"
          ~name:"bash"
          ~arguments:{|{"command":"sleep 60","background":true}|}
          ()
      ; Reply.text "done early"
      ]
  in
  let subagent = subagent ~provider in
  let start = Time_float.now () in
  let result =
    run_tool
      t
      ~tools:(Tools.all @ Tool_jobs.tools @ [ subagent ])
      ~emit:ignore
      subagent
      {|{"task":"x","max_turns":4}|}
  in
  print_s [%sexp (result : Tool.Result.t)];
  print_s
    [%sexp
      (Time_float.Span.( < )
         (Time_float.diff (Time_float.now ()) start)
         (Time_float.Span.of_sec 20.)
       : bool)];
  [%expect
    {|
      user: x
    --
      bash -> started job j1: echo quick; its output is being recorded; you will be notified when it exits, even after you end your turn (you finish only once your jobs have exited). Use job_output to peek, job_kil
    --
      bash ->
      user: [job j1 exited 0] echo quick
    quick
    --
      bash -> started job j2: sleep 60; its output is being recorded; you will be notified when it exits, even after you end your turn (you finish only once your jobs have exited). Use job_output to peek, job_kill
    --
    ((text
       "done early\
      \n[killed when the subagent finished, still running: job j2 (sleep 60)]\
      \n[subagent: 4 turns, 70 in / 29 out tokens, $0.0001]")
     (is_error false))
    true
    |}]
;;

let%expect_test
    "a subagent cancelled while woken only by its jobs kills them and stops"
  =
  with_sandbox
  @@ fun t ->
  let cancel = Cancellation.create () in
  let ended, ended_turn = Eio.Promise.create () in
  let provider =
    Faux_provider.create
      ~on_request:print_since_last_turn
      [ Reply.tool_call
          ~id:"c1"
          ~name:"bash"
          ~arguments:{|{"command":"sleep 60","background":true}|}
          ()
      ; Reply.text "waiting for it"
      ]
  in
  let subagent = subagent ~provider in
  let emit (event : Agent_event.t) =
    match event with
    | Subagent { event = Turn_end { assistant; _ }; _ }
      when String.equal (Message.Assistant.text assistant) "waiting for it" ->
      Eio.Promise.resolve ended_turn ()
    | _ -> ()
  in
  let start = Time_float.now () in
  let result, () =
    Eio.Fiber.pair
      (fun () ->
         let context =
           Tool.Context.create
             ~cancel
             ~env:t.env
             ~cwd:t.dir
             ~tools:(Tools.all @ Tool_jobs.tools @ [ subagent ])
             ~emit
             ()
         in
         Tool.execute subagent context (Jsonaf.of_string {|{"task":"x"}|}))
      (fun () ->
         Eio.Promise.await ended;
         print_endline "--- cancel";
         Cancellation.cancel cancel)
  in
  print_s [%sexp (result : Tool.Result.t)];
  print_s
    [%sexp
      (Time_float.Span.( < )
         (Time_float.diff (Time_float.now ()) start)
         (Time_float.Span.of_sec 20.)
       : bool)];
  [%expect
    {|
      user: x
    --
      bash -> started job j1: sleep 60; its output is being recorded; you will be notified when it exits, even after you end your turn (you finish only once your jobs have exited). Use job_output to peek, job_kill
    --
    --- cancel
    ((text
       "waiting for it\
      \n[killed when the subagent finished, still running: job j1 (sleep 60)]\
      \n[subagent: 2 turns, 30 in / 13 out tokens, $0.0000]")
     (is_error false))
    true
    |}]
;;
