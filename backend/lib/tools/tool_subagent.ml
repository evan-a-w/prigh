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

let continue_name = "subagent_continue"

let fork_advice =
  "Set fork to start it from your conversation so far rather than a blank one: \
   it then knows everything you have read and decided, so it needs little \
   exploration and a short task. Fork for most implementation work that builds \
   on what you explored. Do not fork when your context has nothing useful for \
   the task, or when the subagent should be independent of your thinking (a \
   reviewer, a second opinion, a fresh investigation). Without fork it sees \
   only the task (and any context you pass), so include everything it needs."
;;

let background_description =
  "Start a subagent on a task, in the background. "
  ^ fork_advice
  ^ " Give it a subset of your tools, a different model, a working directory \
     or extra context as needed. Returns at once with the agent id; its final \
     report arrives later as a message starting with [subagent <id> finished]. \
     Keep working on what does not depend on it, or end your turn: you are \
     woken by the report. One that runs out of turns can be given more with \
     subagent_continue."
;;

let sync_description =
  "Delegate a task to a subagent. "
  ^ fork_advice
  ^ " Give it a subset of your tools, a different model, a working directory \
     or extra context as needed. Blocks until it finishes and returns its \
     final report; it cannot delegate any further."
;;

let spec =
  { Tool_spec.name = "subagent"
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description = background_description
  ; parameters =
      Tool_args.schema
        ~required:[ "task" ]
        [ ( "task"
          , `String
          , "Complete description of the task, including relevant paths and \
             constraints (with fork, it already knows your conversation: say \
             what to do)" )
        ; ( "fork"
          , `Boolean
          , "Start it with your conversation so far (default false: a blank \
             context). Fork when it builds on what you explored; not for \
             independent work such as a review or a second opinion" )
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

module Fork = struct
  type t =
    { conversation : Conversation.t
    ; shared : bool
      (** with its caller's system prompt and tools, for the prompt cache *)
    ; refused : string list (** of those tools *)
    }
end

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
    ; fork : Fork.t option
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
  let conversation =
    match Tool_args.bool_opt args "fork", context.conversation with
    | (None | Some false), _ -> None
    | Some true, Some conversation -> Some conversation
    | Some true, None ->
      raise
        (Tool_args.Invalid
           "fork is not available here (there is no conversation to fork): \
            call subagent without it, with a self-contained task")
  in
  let model =
    match Tool_args.string_opt args "model", conversation with
    | None, Some conversation -> conversation.model
    | None, None -> current_model ()
    | Some name, _ ->
      (match Model_registry.resolve models name with
       | Ok model -> model
       | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e)))
  in
  let thinking =
    match Tool_args.string_opt args "thinking", conversation with
    | None, Some conversation -> conversation.thinking
    | None, None -> current_thinking ()
    | Some name, _ ->
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
  let shared =
    match conversation with
    | Some conversation ->
      Option.is_none only
      && String.equal (Model.key model) (Model.key conversation.model)
    | None -> false
  in
  let tools, refused =
    if shared
    then Subagent_fork.shared_tools context.tools ~depth
    else (
      match
        Tools.for_context
          ~parent:(Tools.for_model model context.tools)
          ~depth
          ?only
          ()
      with
      | Ok tools ->
        ( List.map tools ~f:(fun (tool : Tool.t) ->
            if String.equal (Tool.name tool) spec.name
            then
              { tool with
                spec = { tool.spec with description = sync_description }
              }
            else tool)
        , [] )
      | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e)))
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
  ; fork =
      Option.map conversation ~f:(fun conversation ->
        { Fork.conversation; shared; refused })
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
  let steer () =
    deliveries (Background_tasks.take_undelivered jobs) @ steers ()
  in
  let rec wake () =
    match all () with
    | _ :: _ as messages -> messages
    | [] ->
      (match
         Background_tasks.take_next
           jobs
           ~cancel:context.cancel
           ?interrupt:
             (Option.map inbox ~f:(fun inbox () -> Subagent_inbox.await inbox))
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

(* A subagent across its runs: it runs again from [messages] when continued
   after running out of turns. Its totals cover every run. *)
module State = struct
  type t =
    { context : Tool.Context.t
      (** its call's, for what outlives a run (each run has its own
          [cancel] and [emit]) *)
    ; prepared : Prepared.t
    ; agent_id : string
    ; config : Agent_loop.Config.t
    ; worktree : Agent_worktree.t option
    ; mutable messages : Message.t list
    ; mutable turns : int
    ; mutable local_usage : Usage.t (** its own replies' *)
    ; mutable compaction_usage : Usage.t
    ; mutable compaction_cost_usd : float
    ; children : (Usage.t * float) String.Table.t
      (** its subagents' totals, by id *)
    }

  let usage t =
    Hashtbl.fold
      t.children
      ~init:(Usage.add t.local_usage t.compaction_usage)
      ~f:(fun ~key:_ ~data:(usage, _) acc -> Usage.add acc usage)
  ;;

  let cost_usd t =
    Hashtbl.fold
      t.children
      ~init:
        (Model.cost_usd t.prepared.model t.local_usage +. t.compaction_cost_usd)
      ~f:(fun ~key:_ ~data:(_, cost) acc -> acc +. cost)
  ;;
end

(* Whether one that runs out of turns can be continued: a background
   subagent always; a synchronous one when its caller can continue it. *)
module Resumable = struct
  type t =
    | Background
    | Sync of Background_tasks.t
    | No
end

let resumable (context : Tool.Context.t) : Resumable.t =
  match context.background with
  | Some _ when context.depth = 0 -> Background
  | Some tasks
    when List.exists context.tools ~f:(fun t ->
           String.equal (Tool.name t) continue_name) -> Sync tasks
  | Some _ | None -> No
;;

let out_of_turns_hint ~(resumable : Resumable.t) ~agent_id ~turn_limit =
  let continue_ =
    sprintf
      "call %s with id %s (optionally max_turns, default %d, and a message) to \
       give it more turns: it carries on from where it stopped"
      continue_name
      agent_id
      turn_limit
  in
  match resumable with
  | Background ->
    Some
      (sprintf
         "[out of turns: %s; or subagent_cancel it, or leave it]"
         continue_)
  | Sync _ ->
    Some
      (sprintf
         "[out of turns: %s; or leave it (it is dropped when you finish)]"
         continue_)
  | No -> None
;;

(* One run of the subagent: from [state.messages] and [prompts], at most
   [turn_limit] turns. Returns its result, and whether it stopped at its
   turn limit (and so can be continued). *)
let run_once
      ?inbox
      ~provider
      ~resumable
      ~continued
      (state : State.t)
      (context : Tool.Context.t)
      ~turn_limit
      ~prompts
  =
  let p = state.prepared in
  let call_id = context.call_id in
  let agent_id = state.agent_id in
  (* Only a background run has an inbox. It goes under the turn that started
     (or continued) it, which may be over by the time the run ends. *)
  let background = Option.is_some inbox in
  Trace.span
    "subagent"
    ?under:(Option.some_if background "turn")
    ~track:("subagent " ^ agent_id)
    ~attrs:
      [ "agent_id", String agent_id
      ; "model", String (Model.key p.model)
      ; "background", Bool background
      ; "forked", Bool (Option.is_some p.fork)
      ; "continued", Bool continued
      ]
  @@ fun () ->
  context.emit
    (Agent_event.Subagent_start
       { call_id
       ; agent_id
       ; task = p.task
       ; model = p.model.id
       ; tools = List.map p.tools ~f:Tool.name
       ; worktree = state.worktree
       ; forked = Option.is_some p.fork
       ; continued
       });
  let run_turns = ref 0 in
  let emit (event : Agent_event.t) =
    (match event with
     | Turn_start ->
       incr run_turns;
       state.turns <- state.turns + 1
     | Subagent_end { agent_id; usage; cost_usd; _ } ->
       Hashtbl.set state.children ~key:agent_id ~data:(usage, cost_usd)
     | Compacted { usage; cost_usd; _ } ->
       state.compaction_usage <- Usage.add state.compaction_usage usage;
       state.compaction_cost_usd <- state.compaction_cost_usd +. cost_usd
     | _ -> ());
    context.emit (Agent_event.Subagent { call_id; agent_id; event })
  in
  let end_ result =
    context.emit
      (Agent_event.Subagent_end
         { call_id
         ; agent_id
         ; usage = State.usage state
         ; turns = state.turns
         ; cost_usd = State.cost_usd state
         ; result
         })
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
        ~config:{ state.config with max_turns = Some turn_limit }
        ~cwd:p.cwd
        ~cancel:context.cancel
        ~execute:context.execute
        ~approve:context.approve
        ~depth:p.depth
        ~agent_id
        ~emit
        ~halt:context.halt
        ~on_context:(fun messages -> state.messages <- messages)
        ~context:state.messages
        ~prompts
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
      Eio.Cancel.protect (fun () -> end_ (Tool.Result.error text));
      raise exn
  in
  List.iter added ~f:(function
    | Message.Assistant assistant ->
      state.local_usage <- Usage.add state.local_usage assistant.usage
    | User _ | Tool_result _ -> ());
  let usage = State.usage state in
  let last_assistant =
    List.rev added
    |> List.find_map ~f:(function
      | Message.Assistant assistant -> Some assistant
      | User _ | Tool_result _ -> None)
  in
  let out_of_turns =
    match last_assistant with
    | Some ({ stop_reason = End_turn | Tool_use | Length; _ } as assistant) ->
      !run_turns >= turn_limit
      && (not (List.is_empty (Message.Assistant.tool_calls assistant)))
      && (not (Cancellation.is_cancelled context.cancel))
      && Option.is_none (context.halt ())
    | Some _ | None -> false
  in
  let hint =
    if out_of_turns
    then out_of_turns_hint ~resumable ~agent_id ~turn_limit:p.turn_limit
    else None
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
        state.turns
        usage.input
        usage.output
        (State.cost_usd state)
    ^ Option.value_map state.worktree ~default:"" ~f:(fun w ->
      "\n" ^ Agent_worktree.report_line w)
    ^ Option.value_map hint ~default:"" ~f:(fun hint -> "\n" ^ hint)
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
            if out_of_turns
            then
              Tool.Result.error
                (sprintf
                   "subagent hit the %d-turn limit without finishing\n%s"
                   turn_limit
                   body)
            else Tool.Result.ok body))
  in
  end_ result;
  result, out_of_turns && Option.is_some hint
;;

let execute
      ?inbox
      ~provider
      ~compaction
      ~home
      ~resumable
      (context : Tool.Context.t)
      (p : Prepared.t)
      ~agent_id
  =
  let worktree =
    Option.map p.created ~f:(fun (c : Worktree.Created.t) ->
      { Agent_worktree.agent = agent_id
      ; task = p.task
      ; path = c.worktree.path
      ; branch = Option.value c.worktree.branch ~default:"HEAD"
      ; root = c.root
      })
  in
  let own_system () =
    let base =
      match p.extra_context with
      | Some extra when Option.is_none p.fork -> extra ^ "\n\n" ^ instructions
      | Some _ | None -> instructions
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
  let system, cache_key =
    match p.fork with
    | Some { shared = true; conversation; _ } ->
      conversation.system, context.cache_key
    | Some { shared = false; _ } | None ->
      ( own_system ()
      , Some
          (match context.cache_key with
           | Some parent -> parent ^ "/" ^ agent_id
           | None -> agent_id) )
  in
  let config =
    { Agent_loop.Config.model = p.model
    ; thinking = p.thinking
    ; system
    ; tools = p.tools
    ; max_turns = Some p.turn_limit
    ; max_tokens = None
    ; retries = Agent_loop.Config.default_retries
    ; compaction
    ; cache_key
    }
  in
  let messages, prompt =
    match p.fork with
    | None -> [], Message.user p.task
    | Some { conversation; shared; refused } ->
      ( Subagent_fork.context conversation ~call_id:context.call_id
      , Subagent_fork.prompt
          { task = p.task
          ; extra_context = p.extra_context
          ; parent_cwd = context.cwd
          ; cwd = p.cwd
          ; worktree
          ; refused
          ; sync_subagents =
              shared
              && p.depth = 1
              && List.exists p.tools ~f:(fun t ->
                String.equal (Tool.name t) spec.name)
              && not (List.mem refused spec.name ~equal:String.equal)
          } )
  in
  let state =
    { State.context
    ; prepared = p
    ; agent_id
    ; config
    ; worktree
    ; messages
    ; turns = 0
    ; local_usage = Usage.zero
    ; compaction_usage = Usage.zero
    ; compaction_cost_usd = 0.
    ; children = String.Table.create ()
    }
  in
  let result, out_of_turns =
    run_once
      ?inbox
      ~provider
      ~resumable
      ~continued:false
      state
      context
      ~turn_limit:p.turn_limit
      ~prompts:[ prompt ]
  in
  result, Option.some_if out_of_turns state
;;

(* What it is told when continued with a message: like a [subagent_message]
   steer. *)
let caller_message text =
  let note = Injected.Note.steer ~from_user:false ~follow_up:false in
  Message.user
    ~origin:{ Injected.empty with notes = [ note ] }
    (Injected.prefix_notes [ note ] text)
;;

let continue_run
      ?inbox
      ~provider
      ~resumable
      (state : State.t)
      (context : Tool.Context.t)
      ~max_turns
      ~message
  =
  let result, out_of_turns =
    run_once
      ?inbox
      ~provider
      ~resumable
      ~continued:true
      state
      context
      ~turn_limit:(Option.value max_turns ~default:state.prepared.turn_limit)
      ~prompts:(Option.to_list (Option.map message ~f:caller_message))
  in
  result, Option.some_if out_of_turns state
;;

let outcome_of (result : Tool.Result.t) ~parked =
  { Background_tasks.Outcome.status =
      (if parked
       then "out of turns"
       else if result.is_error
       then "failed"
       else "finished")
  ; body = result.text
  ; is_error = result.is_error
  }
;;

(* Parks a background subagent that ran out of turns under its task's
   id. *)
let rec background_outcome ~provider background ~id (result, state) =
  Option.iter state ~f:(fun state ->
    Background_tasks.park
      background
      id
      (fun ~max_turns ~message ~inbox ~call_id:_ ~cancel ~emit ->
         background_outcome
           ~provider
           background
           ~id
           (continue_run
              ?inbox
              ~provider
              ~resumable:Background
              state
              { state.context with cancel; emit; on_output = ignore }
              ~max_turns
              ~message)));
  outcome_of result ~parked:(Option.is_some state)
;;

(* A synchronous one is parked in its caller's tasks, which run it again
   from [subagent_continue] (in the caller's run, with that call's
   [cancel] and [emit]). *)
let rec park_sync ~provider tasks (state : State.t) =
  Background_tasks.park
    tasks
    state.agent_id
    (fun ~max_turns ~message ~inbox:_ ~call_id ~cancel ~emit ->
       let call_id = Option.value call_id ~default:state.context.call_id in
       let result, state =
         continue_run
           ~provider
           ~resumable:(Sync tasks)
           state
           { state.context with cancel; emit; call_id }
           ~max_turns
           ~message
       in
       Option.iter state ~f:(park_sync ~provider tasks);
       outcome_of result ~parked:(Option.is_some state))
;;

let create
      ?(models = Model_registry.builtin ())
      ?(worktrees_dir = fun () -> "~/.prigh/worktrees")
      ?(compaction = fun () -> Compaction_settings.window_only)
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
      let compaction = compaction () in
      (match context.background, resumable context with
       | Some background, Background ->
         let inbox = Subagent_inbox.create () in
         let id =
           Background_tasks.spawn
             background
             ~inbox
             ~kind:Subagent
             ~label:prepared.task
             ~run:(fun ~id ~cancel ~emit ~on_output:_ ->
               background_outcome
                 ~provider
                 background
                 ~id
                 (execute
                    ~inbox
                    ~provider
                    ~compaction
                    ~home
                    ~resumable:Background
                    { context with cancel; emit; on_output = ignore }
                    prepared
                    ~agent_id:id))
         in
         Tool.Result.ok
           (sprintf
              "started agent %s (%s%s); its result will be delivered to you \
               when it finishes: keep working or end your turn meanwhile"
              id
              (if Option.is_some prepared.fork then "forked: " else "")
              (Background_tasks.short_task prepared.task))
       | _, resumable ->
         let agent_id =
           match context.agent_id with
           | None -> context.call_id
           | Some parent -> parent ^ "/" ^ context.call_id
         in
         let result, state =
           execute
             ~provider
             ~compaction
             ~home
             ~resumable
             context
             prepared
             ~agent_id
         in
         (match resumable, state with
          | Sync tasks, Some state -> park_sync ~provider tasks state
          | (Background | Sync _ | No), _ -> ());
         result)
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
  ; destructive = Never
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

let continue_tool =
  let spec =
    control_spec
      ~name:continue_name
      ~description:
        "Give a subagent that ran out of turns more turns: it carries on from \
         where it stopped, with its context, under the same id. A background \
         one runs in the background again and its report arrives as usual; \
         your own synchronous subagent's runs now and returns its report."
      (Tool_args.schema
         ~required:[ "id" ]
         [ "id", `String, "Agent id (its out-of-turns report names it)"
         ; ( "max_turns"
           , `Integer
           , "How many more turns (default: its limit when it started)" )
         ; ( "message"
           , `String
           , "Something to tell it first, as from its caller (e.g. what to \
              focus on)" )
         ])
  in
  let run (context : Tool.Context.t) args =
    let id = Tool_args.string args "id" in
    let max_turns = Tool_args.int_opt args "max_turns" in
    Option.iter max_turns ~f:(fun n ->
      if n < 1 then raise (Tool_args.Invalid "max_turns must be at least 1"));
    let message =
      Option.filter (Tool_args.string_opt args "message") ~f:(fun text ->
        not (String.is_empty (String.strip text)))
    in
    match
      ok_or_invalid
        (Background_tasks.continue_
           (background_of context)
           id
           ~max_turns
           ~message)
    with
    | `Restarted ->
      Tool.Result.ok
        (sprintf
           "continuing %s%s; its report will be delivered to you when it \
            finishes: keep working or end your turn meanwhile"
           id
           (Option.value_map max_turns ~default:"" ~f:(fun n ->
              sprintf " with %d more turns" n)))
    | `Run_here resume ->
      let outcome =
        resume
          ~max_turns
          ~message
          ~inbox:None
          ~call_id:(Some context.call_id)
          ~cancel:context.cancel
          ~emit:context.emit
      in
      { Tool.Result.text = outcome.body
      ; is_error = outcome.is_error
      ; images = []
      }
  in
  { Tool.spec; run }
;;

let control_tools = [ status_tool; cancel_tool; message_tool; continue_tool ]
