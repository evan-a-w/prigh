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

let background_description =
  "Start a subagent on a self-contained task, in the background. It sees only \
   the task (and any context you pass), so include everything it needs. Give \
   it a subset of your tools, a different model, a working directory or extra \
   context as needed. Returns at once with the agent id; its final report \
   arrives later as a message starting with [subagent <id> finished]. Keep \
   working or end your turn meanwhile; use subagent_wait only when you need \
   the result before you can continue."
;;

let sync_description =
  "Delegate a self-contained task to a subagent with its own context. The \
   subagent sees only the task (and any context you pass), so include \
   everything it needs. Give it a subset of your tools, a different model, a \
   working directory or extra context as needed. Blocks until it finishes and \
   returns its final report; it cannot delegate any further."
;;

let spec =
  { Tool_spec.name = "subagent"
  ; parallel_safe = true
  ; on_host = false
  ; destructive = false
  ; description = background_description
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

module Prepared = struct
  type t =
    { task : string
    ; model : Model.t
    ; thinking : Thinking.t
    ; cwd : string
    ; turn_limit : int
    ; extra_context : string option
    ; tools : Tool.t list
    ; depth : int
    }
end

let prepare ~current_model ~current_thinking (context : Tool.Context.t) args =
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
  let depth = context.depth + 1 in
  let tools =
    match Tools.for_context ~parent:context.tools ~depth ?only () with
    | Ok tools ->
      List.map tools ~f:(fun (tool : Tool.t) ->
        if String.equal (Tool.name tool) spec.name
        then
          { tool with spec = { tool.spec with description = sync_description } }
        else tool)
    | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e))
  in
  { Prepared.task
  ; model
  ; thinking
  ; cwd
  ; turn_limit
  ; extra_context = Tool_args.string_opt args "context"
  ; tools
  ; depth
  }
;;

let execute
      ~provider
      ~home
      (context : Tool.Context.t)
      (p : Prepared.t)
      ~agent_id
  =
  let call_id = context.call_id in
  let system =
    let base =
      match p.extra_context with
      | Some extra -> extra ^ "\n\n" ^ instructions
      | None -> instructions
    in
    let instructions =
      Host_ops.instructions_of_result
        (Tool.execute_via
           { context with cwd = p.cwd; on_output = ignore }
           Host_ops.instructions_tool
           (`Object [ "home", `String home ]))
    in
    Some
      (base
       ^ "\n\n"
       ^ System_prompt.build
           ~instructions
           ~cwd:p.cwd
           ~home
           ~tools:(Tools.specs p.tools)
           ())
  in
  let config =
    { Agent_loop.Config.model = p.model
    ; thinking = p.thinking
    ; system
    ; tools = p.tools
    ; max_turns = Some p.turn_limit
    ; max_tokens = None
    ; retries = Agent_loop.Config.default_retries
    }
  in
  context.emit
    (Agent_event.Subagent_start
       { call_id
       ; agent_id
       ; task = p.task
       ; model = p.model.id
       ; tools = List.map p.tools ~f:Tool.name
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
      ~cwd:p.cwd
      ~cancel:context.cancel
      ~execute:context.execute
      ~depth:p.depth
      ~agent_id
      ~emit
      ~context:[]
      ~prompts:[ Message.user p.task ]
      ()
  in
  let local_usage =
    List.fold added ~init:Usage.zero ~f:(fun acc message ->
      match message with
      | Message.Assistant assistant -> Usage.add acc assistant.usage
      | User _ | Tool_result _ -> acc)
  in
  let usage = Usage.add local_usage !nested_usage in
  let cost_usd = Model.cost_usd p.model local_usage +. !nested_cost_usd in
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
       | Error e -> Tool.Result.error (sprintf "subagent failed: %s\n%s" e body)
       | Aborted ->
         if Cancellation.is_cancelled context.cancel
         then Tool.Result.error ("[cancelled]\n" ^ body)
         else Tool.Result.error ("subagent aborted\n" ^ body)
       | End_turn | Tool_use | Length ->
         if
           !turns >= p.turn_limit
           && not (List.is_empty (Message.Assistant.tool_calls assistant))
         then
           Tool.Result.error
             (sprintf
                "subagent hit the %d-turn limit without finishing\n%s"
                p.turn_limit
                body)
         else Tool.Result.ok body)
  in
  context.emit
    (Agent_event.Subagent_end
       { call_id; agent_id; usage; turns = !turns; cost_usd; result });
  result
;;

let create ~provider ~current_model ~current_thinking ~home =
  let run (context : Tool.Context.t) args =
    let prepared = prepare ~current_model ~current_thinking context args in
    match context.background with
    | Some background when context.depth = 0 ->
      let id =
        Background_tasks.spawn
          background
          ~kind:Subagent
          ~label:prepared.task
          ~run:(fun ~id ~cancel ~emit ~on_output:_ ->
            let result =
              execute
                ~provider
                ~home
                { context with cancel; emit; on_output = ignore }
                prepared
                ~agent_id:id
            in
            { Background_tasks.Outcome.status =
                (if result.is_error then "failed" else "finished")
            ; body = result.text
            ; is_error = result.is_error
            })
      in
      Tool.Result.ok
        (sprintf
           "started agent %s (%s); its result will be delivered to you when it \
            finishes; use subagent_wait to block on it"
           id
           (Background_tasks.short_task prepared.task))
    | _ ->
      let agent_id =
        match context.agent_id with
        | None -> context.call_id
        | Some parent -> parent ^ "/" ^ context.call_id
      in
      execute ~provider ~home context prepared ~agent_id
  in
  { Tool.spec; run }
;;

let background_of (context : Tool.Context.t) =
  match context.background with
  | Some background -> background
  | None ->
    raise (Tool_args.Invalid "background subagents are not available here")
;;

let ok_or_invalid = function
  | Ok x -> x
  | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e))
;;

let control_spec ~name ~description parameters =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; destructive = false
  ; description
  ; parameters
  }
;;

let wait_tool =
  let spec =
    control_spec
      ~name:"subagent_wait"
      ~description:
        "Block until background subagents finish and return their reports \
         (they are then not delivered again). By default waits for all of \
         them; pass ids to pick some, all=false to return as soon as one \
         finishes, timeout to give up after some seconds."
      (Tool_args.schema
         [ ( "ids"
           , `Array (`Object [ "type", `String "string" ])
           , "Agent ids (default: every running or undelivered one)" )
         ; ( "all"
           , `Boolean
           , "Wait for all of them (default) or only the first to finish" )
         ; "timeout", `Integer, "Give up after this many seconds"
         ])
  in
  let run (context : Tool.Context.t) args =
    let background = background_of context in
    let ids = Tool_args.string_list_opt args "ids" in
    let all = Option.value (Tool_args.bool_opt args "all") ~default:true in
    let timeout =
      Option.map (Tool_args.int_opt args "timeout") ~f:Float.of_int
    in
    let { Background_tasks.Wait_result.finished; running; timed_out } =
      ok_or_invalid
        (Background_tasks.wait
           background
           ~kind:Subagent
           ~ids
           ~all
           ~timeout
           ~cancel:context.cancel)
    in
    let reports = List.map finished ~f:Background_tasks.Task.report in
    let still =
      match running with
      | [] -> []
      | running ->
        [ sprintf
            "%sstill running: %s"
            (if timed_out then "timed out; " else "")
            (String.concat
               ~sep:", "
               (List.map running ~f:(fun job ->
                  sprintf
                    "%s (%s)"
                    (Background_tasks.Task.id job)
                    (Background_tasks.short_task
                       (Background_tasks.Task.label job)))))
        ]
    in
    match reports @ still with
    | [] -> Tool.Result.ok "no subagents to wait for"
    | parts -> Tool.Result.ok (String.concat ~sep:"\n\n" parts)
  in
  { Tool.spec; run }
;;

let status_tool =
  let spec =
    control_spec
      ~name:"subagent_status"
      ~description:
        "List background subagents: id, state, elapsed time, task and last \
         activity."
      (Tool_args.schema [])
  in
  let run context _args =
    Tool.Result.ok
      (Background_tasks.status_text (background_of context) ~kind:Subagent)
  in
  { Tool.spec; run }
;;

let cancel_tool =
  let spec =
    control_spec
      ~name:"subagent_cancel"
      ~description:
        "Cancel a background subagent and return whatever it reported."
      (Tool_args.schema ~required:[ "id" ] [ "id", `String, "Agent id" ])
  in
  let run (context : Tool.Context.t) args =
    let id = Tool_args.string args "id" in
    let task =
      ok_or_invalid
        (Background_tasks.cancel_and_wait
           (background_of context)
           ~kind:Subagent
           id
           ~cancel:context.cancel)
    in
    Tool.Result.ok (Background_tasks.Task.report task)
  in
  { Tool.spec; run }
;;

let control_tools = [ wait_tool; status_tool; cancel_tool ]
