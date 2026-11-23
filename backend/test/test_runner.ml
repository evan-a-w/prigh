open! Core
open! Prigh
open Tool_test_helpers

(* An engine that answers every run with one scripted reply, printing what it
   was given. *)
module Scripted = struct
  type t = { reply : string }

  let engine = Engine_id.Composer

  let capabilities (_ : t) =
    { Capabilities.harness with
      fork = By_handoff
    ; rewind = Unsupported
    ; compact = Auto_only
    ; subagents = Observe
    }
  ;;

  let run_turn
        t
        (services : Engine_intf.Services.t)
        ~cancel:_
        (input : Engine_intf.Turn.Input.t)
    =
    let text m =
      match (m : Message.t) with
      | User u -> Message.User.body u
      | Assistant a -> Message.Assistant.text a
      | Tool_result r -> r.text
    in
    print_s
      [%message
        "run_turn"
          ~context:(List.map input.context ~f:text : string list)
          ~prompts:(List.map input.prompts ~f:text : string list)
          ~chat:(input.chat : bool)];
    services.notice "scripted engine at work";
    let reply =
      Message.Assistant
        { content = [ Text t.reply ]
        ; stop_reason = End_turn
        ; usage = { input = 3; output = 2; cache_read = 0 }
        ; model = "scripted/model"
        }
    in
    services.emit (Message_start reply);
    services.emit (Message_end reply);
    [ reply ]
  ;;
end

let with_agent ?runner f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ?runner
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Loop (Message_end (Assistant a)) ->
      print_endline ("assistant: " ^ Message.Assistant.text a)
    | Notice n -> print_endline ("notice: " ^ n)
    | State_changed s ->
      printf "state: running=%b messages=%d\n" s.running s.message_count
    | _ -> ());
  f agent
;;

let print_engine agent =
  print_s
    [%message
      ""
        ~engine:(Agent.engine agent : Engine_id.t)
        ~capabilities:(Agent.capabilities agent : Capabilities.t)]
;;

let%expect_test "an agent's runs go to its runner" =
  let runner = Runner.create (module Scripted) { reply = "done it" } in
  with_agent ~runner
  @@ fun agent ->
  print_engine agent;
  [%expect
    {|
    ((engine Composer)
     (capabilities
      ((fork By_handoff) (rewind Unsupported) (context_edit true)
       (compact Auto_only) (steer In_run) (plan_mode true) (btw true)
       (subagents Observe) (model_fallback true) (images true) (ask_user true))))
    |}];
  Or_error.ok_exn (Agent.prompt agent "first");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "second");
  Agent.wait_idle agent;
  print_s [%sexp ((Agent.state agent).message_count : int)];
  [%expect
    {|
    state: running=true messages=0
    (run_turn (context ()) (prompts (first)) (chat false))
    notice: scripted engine at work
    assistant: done it
    state: running=false messages=2
    state: running=true messages=2
    (run_turn (context (first "done it")) (prompts (second)) (chat false))
    notice: scripted engine at work
    assistant: done it
    state: running=false messages=4
    4
    |}]
;;

let%expect_test "without a runner, the agent is the harness" =
  with_agent
  @@ fun agent ->
  print_engine agent;
  [%expect
    {|
    ((engine Harness)
     (capabilities
      ((fork Native) (rewind Native) (context_edit true) (compact Native)
       (steer In_run) (plan_mode true) (btw true) (subagents Full)
       (model_fallback true) (images true) (ask_user true))))
    |}]
;;
