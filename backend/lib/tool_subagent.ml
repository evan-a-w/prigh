open! Core
open! Import

let max_turns = 50

let instructions =
  "You are a subagent. Complete the task you were given, using the tools \
   available to you. You may delegate a self-contained piece of work to \
   another subagent, but a subagent you start cannot delegate any further. \
   When you are done, reply with a concise report of what you found or changed \
   and anything the caller must know."
;;

let spec =
  { Tool_spec.name = "subagent"
  ; parallel_safe = true
  ; description =
      "Delegate a self-contained task to a subagent with its own context. The \
       subagent sees only the task (and any context you pass), so include \
       everything it needs. Give it a subset of your tools, a different model, \
       a working directory or extra context as needed. The subagent may \
       delegate once more, but not deeper. Returns its final report."
  ; parameters =
      Tool_args.schema
        ~required:[ "task" ]
        [ ( "task"
          , `String
          , "Complete description of the task, including relevant paths and \
             constraints" )
        ; ( "tools"
          , `Array (`Object [ "type", `String "string" ])
          , "Subset of this agent's tool names to give the subagent (default: \
             all of them)" )
        ; ( "model"
          , `String
          , "Model key, id, name or unambiguous prefix (default: this agent's \
             model)" )
        ; ( "thinking"
          , `String
          , "Thinking level: off|on|low|high|max (default: this agent's)" )
        ; "cwd", `String, "Working directory (default: this agent's)"
        ; ( "max_turns"
          , `Integer
          , sprintf "Maximum number of turns (default %d)" max_turns )
        ; ( "context"
          , `String
          , "Extra text supplied to the subagent as a system block, e.g. a plan"
          )
        ]
  }
;;

let create ~provider ~current_model ~current_thinking ~home =
  let run (context : Tool.Context.t) args =
    let task = Tool_args.string args "task" in
    let only = Tool_args.string_list_opt args "tools" in
    let model =
      match Tool_args.string_opt args "model" with
      | None -> current_model ()
      | Some name ->
        (match Model.resolve name with
         | Ok model -> model
         | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e)))
    in
    let thinking =
      match Tool_args.string_opt args "thinking" with
      | None -> current_thinking ()
      | Some name ->
        (match Thinking.of_string name with
         | Ok thinking -> thinking
         | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e)))
    in
    let cwd =
      Option.value (Tool_args.string_opt args "cwd") ~default:context.cwd
    in
    let turn_limit =
      Option.value (Tool_args.int_opt args "max_turns") ~default:max_turns
    in
    let extra_context = Tool_args.string_opt args "context" in
    let depth = context.depth + 1 in
    let tools =
      match Tools.for_context ~parent:context.tools ~depth ?only () with
      | Ok tools -> tools
      | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e))
    in
    let call_id = context.call_id in
    let agent_id =
      match context.agent_id with
      | None -> call_id
      | Some parent -> parent ^ "/" ^ call_id
    in
    let system =
      let base =
        match extra_context with
        | Some extra -> extra ^ "\n\n" ^ instructions
        | None -> instructions
      in
      Some
        (base
         ^ "\n\n"
         ^ System_prompt.build ~cwd ~home ~tools:(Tools.specs tools) ())
    in
    let config =
      { Agent_loop.Config.model
      ; thinking
      ; system
      ; tools
      ; max_turns = Some turn_limit
      ; max_tokens = None
      ; retries = Agent_loop.Config.default_retries
      }
    in
    context.emit
      (Agent_event.Subagent_start
         { call_id
         ; agent_id
         ; task
         ; model = model.id
         ; tools = List.map tools ~f:Tool.name
         });
    let turns = ref 0 in
    let nested_usage = ref Usage.zero in
    let nested_cost_usd = ref 0. in
    let emit (event : Agent_event.t) =
      (match event with
       | Turn_start -> incr turns
       | Subagent_end { usage; cost_usd; _ } ->
         nested_usage := Usage.add !nested_usage usage;
         nested_cost_usd := !nested_cost_usd +. cost_usd
       | _ -> ());
      context.emit (Agent_event.Subagent { call_id; agent_id; event })
    in
    let added =
      Agent_loop.run
        ~env:context.env
        ~provider
        ~config
        ~cwd
        ~cancel:context.cancel
        ~depth
        ~agent_id
        ~emit
        ~context:[]
        ~prompts:[ Message.user task ]
        ()
    in
    let local_usage =
      List.fold added ~init:Usage.zero ~f:(fun acc message ->
        match message with
        | Message.Assistant assistant -> Usage.add acc assistant.usage
        | User _ | Tool_result _ -> acc)
    in
    let usage = Usage.add local_usage !nested_usage in
    let cost_usd = Model.cost_usd model local_usage +. !nested_cost_usd in
    let last_assistant =
      List.rev added
      |> List.find_map ~f:(function
        | Message.Assistant assistant -> Some assistant
        | User _ | Tool_result _ -> None)
    in
    let trailer =
      sprintf
        "[subagent: %d turns, %d in / %d out tokens, $%.4f]"
        !turns
        usage.input
        usage.output
        cost_usd
    in
    let result =
      match last_assistant with
      | None -> Tool.Result.error trailer
      | Some assistant ->
        let report = Message.Assistant.text assistant in
        let body =
          if String.is_empty report then trailer else report ^ "\n" ^ trailer
        in
        (match assistant.stop_reason with
         | Error e ->
           Tool.Result.error (sprintf "subagent failed: %s\n%s" e body)
         | Aborted ->
           if Cancellation.is_cancelled context.cancel
           then Tool.Result.error ("[cancelled]\n" ^ body)
           else Tool.Result.error ("subagent aborted\n" ^ body)
         | End_turn | Tool_use | Length ->
           if
             !turns >= turn_limit
             && not (List.is_empty (Message.Assistant.tool_calls assistant))
           then
             Tool.Result.error
               (sprintf
                  "subagent hit the %d-turn limit without finishing\n%s"
                  turn_limit
                  body)
           else Tool.Result.ok body)
    in
    context.emit
      (Agent_event.Subagent_end
         { call_id; agent_id; usage; turns = !turns; cost_usd; result });
    result
  in
  { Tool.spec; run }
;;
