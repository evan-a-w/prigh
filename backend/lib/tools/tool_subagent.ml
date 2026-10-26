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
   working on what does not depend on it, or end your turn: you are woken by \
   the report."
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
  ; freeform = None
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
        ; ( "worktree"
          , `String
          , "Run it in a fresh git worktree of the working directory's \
             repository, on a new branch: a branch name, or \"auto\" for one \
             named after the task. Use it for changes to review and merge \
             separately, or to keep parallel work apart; its report says where \
             the branch is (default: none, it works in the working directory)" )
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
    ; worktree : [ `Auto | `Branch of string ] option (** asked for *)
    ; created : Worktree.Created.t option (** made, [cwd] is in it *)
    }
end

let worktree_arg args =
  match Json.member "worktree" args with
  | None | Some `Null | Some `False -> None
  | Some `True -> Some `Auto
  | Some (`String s) ->
    (match String.strip s with
     | "" | "false" | "no" | "none" -> None
     | "auto" | "true" | "yes" -> Some `Auto
     | branch -> Some (`Branch branch))
  | Some _ ->
    raise
      (Tool_args.Invalid "worktree must be a branch name or \"auto\" (or true)")
;;

(* [agent/<the task's first words>]. *)
let auto_branch task =
  let words =
    String.split_on_chars (String.lowercase task) ~on:[ ' '; '\n'; '\t' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> fun words -> List.take words 5
  in
  let slug = Worktree.slug (String.concat ~sep:"-" words) in
  "agent/" ^ String.prefix slug 40 |> String.rstrip ~drop:(Char.equal '-')
;;

let prepare
      ~models
      ~current_model
      ~current_thinking
      (context : Tool.Context.t)
      args
  =
  let task = Tool_args.string args "task" in
  let only = Tool_args.string_list_opt args "tools" in
  let model =
    match Tool_args.string_opt args "model" with
    | None -> current_model ()
    | Some name ->
      (match Model_registry.resolve models name with
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
    match Tool_args.string_opt args "cwd" with
    | None -> context.cwd
    | Some cwd -> Tool.resolve_path context cwd
  in
  let turn_limit =
    Option.value (Tool_args.int_opt args "max_turns") ~default:max_turns
  in
  let depth = context.depth + 1 in
  let tools =
    match
      Tools.for_context
        ~parent:(Tools.for_model model context.tools)
        ~depth
        ?only
        ()
    with
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
  ; worktree = worktree_arg args
  ; created = None
  }
;;

(* The worktree [p] asks for, made on the tool host; [p] then works in it. *)
let make_worktree ~worktrees_dir (context : Tool.Context.t) (p : Prepared.t) =
  match p.worktree with
  | None -> Ok p
  | Some wanted ->
    let branch, fresh =
      match wanted with
      | `Auto -> auto_branch p.task, true
      | `Branch branch -> branch, false
    in
    Tool.execute_via
      { context with cwd = p.cwd; on_output = ignore }
      Worktree.create_tool
      (Worktree.create_args
         ~fresh
         ~dir:(worktrees_dir ())
         ~branch
         ~base:None
         ())
    |> Worktree.created_of_result
    |> Or_error.map ~f:(fun (created : Worktree.Created.t) ->
      { p with cwd = created.cwd; created = Some created })
;;

(* A subagent's jobs are its own: their reports arrive between its turns,
   and after it ends its turn while some still run (so it finishes only
   with none running). Those still running when it stops otherwise
   (cancelled, out of turns) are killed and returned. *)
let with_jobs (context : Tool.Context.t) ~emit f =
  Eio.Switch.run
  @@ fun sw ->
  let jobs =
    Background_tasks.create
      ~env:context.env
      ~sw
      ~first_subagent_id:1
      ~first_job_id:1
      ()
  in
  Background_tasks.connect jobs ~emit ~on_change:ignore;
  let result = f jobs in
  let left =
    List.filter
      (Background_tasks.tasks jobs ~kind:Job)
      ~f:Background_tasks.Task.running
  in
  Background_tasks.cancel_all ~discard:true jobs;
  Background_tasks.wait_all jobs;
  result, left
;;

let deliveries = function
  | [] -> []
  | tasks -> [ Background_tasks.delivery_message tasks ]
;;

(* The loop's [steer] and [wake]: finished jobs' reports and the inbox's
   messages. When the model ends its turn, it carries on with whatever is
   queued, or waits for a job (or a message) while jobs run; the inbox is
   closed when it stops, with nothing in between that could take a
   message. *)
let feeds ~(context : Tool.Context.t) ~jobs inbox =
  let steers () =
    Option.value_map inbox ~default:[] ~f:Subagent_inbox.take_steers
  in
  let all () = Option.value_map inbox ~default:[] ~f:Subagent_inbox.take_all in
  let steer () = deliveries (Background_tasks.take_undelivered jobs) @ steers () in
  let rec wake () =
    match all () with
    | _ :: _ as messages -> messages
    | [] ->
      (match
         Background_tasks.take_next
           jobs
           ~cancel:context.cancel
           ?interrupt:(Option.map inbox ~f:(fun inbox () -> Subagent_inbox.await inbox))
       with
       | _ :: _ as tasks -> deliveries tasks @ all ()
       | [] ->
         (match inbox with
          | Some inbox
            when (not (Cancellation.is_cancelled context.cancel))
                 && not (Subagent_inbox.close_if_empty inbox) -> wake ()
          | Some _ | None -> []))
  in
  steer, wake
;;

let execute
      ?inbox
      ~provider
      ~home
      (context : Tool.Context.t)
      (p : Prepared.t)
      ~agent_id
  =
  let call_id = context.call_id in
  let worktree =
    Option.map p.created ~f:(fun (c : Worktree.Created.t) ->
      { Agent_worktree.agent = agent_id
      ; task = p.task
      ; path = c.worktree.path
      ; branch = Option.value c.worktree.branch ~default:"HEAD"
      ; root = c.root
      })
  in
  let system =
    let base =
      match p.extra_context with
      | Some extra -> extra ^ "\n\n" ^ instructions
      | None -> instructions
    in
    let { Host_ops.Instructions.files; nix; skills } =
      Host_ops.instructions_of_result
        (Tool.execute_via
           { context with cwd = p.cwd; on_output = ignore }
           Host_ops.instructions_tool
           (Host_ops.instructions_args ~home))
    in
    Some
      (base
       ^ "\n\n"
       ^ System_prompt.build
           ~instructions:files
           ~nix
           ~skills
           ~role:Subagent
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
    ; cache_key =
        Some
          (match context.cache_key with
           | Some parent -> parent ^ "/" ^ agent_id
           | None -> agent_id)
    }
  in
  context.emit
    (Agent_event.Subagent_start
       { call_id
       ; agent_id
       ; task = p.task
       ; model = p.model.id
       ; tools = List.map p.tools ~f:Tool.name
       ; worktree
       });
  let turns = ref 0 in
  let nested_usage = ref Usage.zero in
  let nested_cost_usd = ref 0. in
  let emit (event : Agent_event.t) =
    (match event with
     | Turn_start -> incr turns
     | Subagent_end { usage; cost_usd; _ } | Compacted { usage; cost_usd; _ } ->
       nested_usage := Usage.add !nested_usage usage;
       nested_cost_usd := !nested_cost_usd +. cost_usd
     | _ -> ());
    context.emit (Agent_event.Subagent { call_id; agent_id; event })
  in
  let added, left =
    with_jobs context ~emit
    @@ fun jobs ->
    let steer, wake = feeds ~context ~jobs inbox in
    match
      Agent_loop.run
        ~env:context.env
        ~background:jobs
        ~steer
        ~wake
        ~provider
        ~config
        ~cwd:p.cwd
        ~cancel:context.cancel
        ~execute:context.execute
        ~approve:context.approve
        ~depth:p.depth
        ~agent_id
        ~emit
        ~halt:context.halt
        ~context:[]
        ~prompts:[ Message.user p.task ]
        ()
    with
    | added -> added
    | exception exn ->
      (* The start always gets an end, or frontends would show the subagent
         running forever. *)
      let text =
        match exn with
        | Eio.Cancel.Cancelled _ -> "[cancelled]"
        | exn -> "subagent failed: " ^ Exn.to_string exn
      in
      Eio.Cancel.protect (fun () ->
        context.emit
          (Agent_event.Subagent_end
             { call_id
             ; agent_id
             ; usage = !nested_usage
             ; turns = !turns
             ; cost_usd = !nested_cost_usd
             ; result = Tool.Result.error text
             }));
      raise exn
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
    (match left with
     | [] -> ""
     | left ->
       sprintf
         "[killed when the subagent finished, still running: %s]\n"
         (String.concat
            ~sep:", "
            (List.map left ~f:(fun task ->
               sprintf
                 "job %s (%s)"
                 (Background_tasks.Task.id task)
                 (Background_tasks.short_task
                    (Background_tasks.Task.label task))))))
    ^ sprintf
        "[subagent: %d turns, %d in / %d out tokens, $%.4f]"
        !turns
        usage.input
        usage.output
        cost_usd
    ^ Option.value_map worktree ~default:"" ~f:(fun w ->
      "\n" ^ Agent_worktree.report_line w)
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
       | Tool_use when Cancellation.is_cancelled context.cancel ->
         (* Cancelled between its tool results and its next turn. *)
         Tool.Result.error ("[cancelled]\n" ^ body)
       | End_turn | Tool_use | Length ->
         let unfinished =
           not (List.is_empty (Message.Assistant.tool_calls assistant))
         in
         (match context.halt () with
          | Some reason when unfinished ->
            Tool.Result.error (sprintf "subagent stopped: %s\n%s" reason body)
          | Some _ | None ->
            if
              !turns >= p.turn_limit
              && not (List.is_empty (Message.Assistant.tool_calls assistant))
            then
              Tool.Result.error
                (sprintf
                   "subagent hit the %d-turn limit without finishing\n%s"
                   p.turn_limit
                   body)
            else Tool.Result.ok body))
  in
  context.emit
    (Agent_event.Subagent_end
       { call_id; agent_id; usage; turns = !turns; cost_usd; result });
  result
;;

let create
      ?(models = Model_registry.builtin ())
      ?(worktrees_dir = fun () -> "~/.prigh/worktrees")
      ~provider
      ~current_model
      ~current_thinking
      ~home
      ()
  =
  let run (context : Tool.Context.t) args =
    let prepared =
      prepare ~models ~current_model ~current_thinking context args
    in
    match make_worktree ~worktrees_dir context prepared with
    | Error e ->
      Tool.Result.error
        (sprintf
           "could not make the subagent's worktree: %s"
           (Error.to_string_hum e))
    | Ok prepared ->
      (match context.background with
       | Some background when context.depth = 0 ->
         let inbox = Subagent_inbox.create () in
         let id =
           Background_tasks.spawn
             background
             ~inbox
             ~kind:Subagent
             ~label:prepared.task
             ~run:(fun ~id ~cancel ~emit ~on_output:_ ->
               let result =
                 execute
                   ~inbox
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
              "started agent %s (%s); its result will be delivered to you when \
               it finishes: keep working or end your turn meanwhile"
              id
              (Background_tasks.short_task prepared.task))
       | _ ->
         let agent_id =
           match context.agent_id with
           | None -> context.call_id
           | Some parent -> parent ^ "/" ^ context.call_id
         in
         execute ~provider ~home context prepared ~agent_id)
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
  ; freeform = None
  ; destructive = false
  ; description
  ; parameters
  }
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

let message_tool =
  let spec =
    control_spec
      ~name:"subagent_message"
      ~description:
        "Send a message to a running background subagent, e.g. to correct its \
         course or give it what it needs: it gets it at its next step (after \
         its current tool calls), as from its caller, and carries on. Its \
         report still arrives as usual."
      (Tool_args.schema
         ~required:[ "id"; "text" ]
         [ "id", `String, "Agent id"; "text", `String, "The message" ])
  in
  let run (context : Tool.Context.t) args =
    let id = Tool_args.string args "id" in
    ok_or_invalid
      (Background_tasks.send
         (background_of context)
         ~id
         Steer
         ~from:Caller
         (Tool_args.string args "text"));
    Tool.Result.ok
      (sprintf "sent to %s: it gets the message at its next step" id)
  in
  { Tool.spec; run }
;;

let control_tools = [ status_tool; cancel_tool; message_tool ]
