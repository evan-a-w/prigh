open! Core
open! Import

let max_turns = 50
let name = "subagent"

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
  ^ " Defaults to this session's engine, model and tool host. Only pass \
     engine, host or a different model when the user explicitly asks for that \
     engine, machine or model. Give it a subset of your tools, a working \
     directory or extra context as needed. Returns at once with the agent id; \
     its final report arrives later as a message starting with [subagent <id> \
     finished]. Keep working on what does not depend on it, or end your turn: \
     you are woken by the report. One that runs out of turns can be given more \
     with subagent_continue."
;;

let sync_description =
  "Delegate a task to a subagent. "
  ^ fork_advice
  ^ " Defaults to this agent's engine, model and tool host. Only pass engine, \
     host or a different model when the user explicitly asked for it. Give it \
     a subset of your tools, a working directory or extra context as needed. \
     Blocks until it finishes and returns its final report; it cannot delegate \
     any further."
;;

let spec =
  { Tool_spec.name
  ; parallel_safe = true
  ; on_host = false
  ; freeform = None
  ; native = None
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
        ; ( "name"
          , `String
          , "A short name for it, two to five words saying what it does and \
             how it differs from your other subagents, e.g. \"Fix completion \
             after a space\" (shown to the user wherever subagents are listed)"
          )
        ; ( "fork"
          , `Boolean
          , "Start it with your conversation so far (default false: a blank \
             context). Fork when it builds on what you explored; not for \
             independent work such as a review or a second opinion" )
        ; ( "tools"
          , `Array (`Object [ "type", `String "string" ])
          , "Subset of this agent's tool names to give the subagent (default: \
             all of them)" )
        ; ( "engine"
          , `String
          , "Engine or adapter to run the subagent on (e.g. \"harness\", \
             \"codex\", \"claude\", \"acp:antigravity\"). Defaults to this \
             agent's engine. Only set this when the user explicitly asked for \
             that engine or adapter" )
        ; ( "model"
          , `String
          , "Model for the subagent: on the harness, a model key, id, name or \
             unambiguous prefix; on an external engine, one of that engine's \
             model ids or names (default: this agent's model on the same \
             engine, else the target engine's default). Only pass a different \
             model when the user explicitly asked for it" )
        ; ( "thinking"
          , `String
          , "Thinking: off, default (the provider's) or an effort level the \
             model takes (minimal|low|medium|high|xhigh|max) (default: this \
             agent's)" )
        ; ( "host"
          , `String
          , "A connected tool host (by name or id) for it to work on: all its \
             tools run there (default: where yours run). Only set this when \
             the user explicitly asked for work on that machine" )
        ; ( "cwd"
          , `String
          , "Working directory (default: this agent's; with host, that host's \
             home, and a relative cwd is from there)" )
        ; ( "worktree"
          , `String
          , "Run it in a fresh git worktree of the working directory's \
             repository, on a new branch: a branch name, or \"auto\" for one \
             named after its name. Use it for changes to review and merge \
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

module External = struct
  type run_result =
    { messages : Message.t list
    ; cost_usd : float
    }

  type t =
    { deployed : Engines.t
    ; current : unit -> Engine_choice.t
    ; harness_model : unit -> Model.t
    ; external_models : host:string -> Adapter_id.t -> Model.t list option
    ; parent_messages : unit -> Message.t list
    ; approval_mode : unit -> Approval_mode.t
    ; plan_mode : unit -> bool
    ; add_spend : usage:Usage.t -> cost_usd:float -> unit
    ; run :
        adapter:Adapter_id.t
        -> host_id:string
        -> host_name:string
        -> cwd:string
        -> model:string option
        -> thinking:Thinking.t
        -> approval:Approval_mode.t
        -> plan:bool
        -> developer_instructions:string option
        -> resume:string option
        -> handoff_from:Engine_choice.t option
        -> session:Session.t
        -> on_thread:(string -> unit)
        -> depth:int
        -> agent_id:string
        -> home:(unit -> string option)
        -> tools:Tool.t list
        -> context:Tool.Context.t
        -> emit:(Agent_event.t -> unit)
        -> steer:(unit -> Message.t list)
        -> await_steer:(unit -> unit)
        -> wake:(unit -> Message.t list)
        -> cancel:Cancellation.t
        -> prompts:Message.t list
        -> run_result
    }
end

module Fork = struct
  type source =
    | Conversation of Conversation.t
    | Messages of Message.t list

  type t =
    { source : source
    ; shared : bool
      (** with its caller's system prompt and tools, for the prompt cache *)
    ; refused : string list (** of those tools *)
    }
end

module Target = struct
  type t =
    | Harness of { model : Model.t }
    | External of
        { adapter : Adapter_id.t
        ; model : string option
        ; display_model : string
        ; host_id : string
        ; host_name : string
        }

  let display_model = function
    | Harness { model } -> model.id
    | External { display_model; _ } -> display_model
  ;;

  let trace_model = function
    | Harness { model } -> Model.key model
    | External { display_model; _ } -> display_model
  ;;
end

module Prepared = struct
  type t =
    { task : string
    ; name : string
    ; named : bool (** [name] was given *)
    ; choice : Engine_choice.t
    ; session_engine : Engine_choice.t
    ; engine_label : string option
      (** [Some (Engine_choice.key choice)] when different from the session's
          engine *)
    ; target : Target.t
    ; thinking : Thinking.t
    ; cwd : string
    ; turn_limit : int
    ; extra_context : string option
    ; tools : Tool.t list
    ; depth : int
    ; host : Tool_on_host.Host.t option
      (** where its tools run, when not the session's active host *)
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

let wants_worktree args =
  match worktree_arg args with
  | Some _ -> true
  | None -> false
  | exception Tool_args.Invalid _ -> false
;;

let wanted_engine args =
  match Tool_args.string_opt args "engine" with
  | Some s ->
    let s = String.strip s in
    Option.some_if (not (String.is_empty s)) s
  | None -> None
  | exception Tool_args.Invalid _ -> None
;;

(* [agent/<the name's first words>]. *)
let auto_branch name =
  let words =
    String.split_on_chars (String.lowercase name) ~on:[ ' '; '\n'; '\t' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> fun words -> List.take words 5
  in
  let slug = Worktree.slug (String.concat ~sep:"-" words) in
  "agent/" ^ String.prefix slug 40 |> String.rstrip ~drop:(Char.equal '-')
;;

let or_invalid = function
  | Ok x -> x
  | Error message -> raise (Tool_args.Invalid message)
;;

let or_invalid_e = function
  | Ok x -> x
  | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e))
;;

(* The host it works on ([None]: the session's active one), and whether that
   is another host than its caller's. *)
let host_of ~hosts (context : Tool.Context.t) args =
  let current = context.host in
  match Tool_args.string_opt args "host" with
  | None ->
    ( Option.map current ~f:(fun id -> or_invalid (Tool_on_host.find hosts id))
    , false )
  | Some key ->
    let host = or_invalid (Tool_on_host.find hosts key) in
    let moves =
      match current with
      | Some id -> not (String.equal id host.id)
      | None -> not host.active
    in
    Option.some_if (not host.active) host, moves
;;

let resolve_engine ~engines_opt ~default_engine args =
  match wanted_engine args with
  | None -> default_engine
  | Some raw ->
    (match engines_opt with
     | Some (ext : External.t) ->
       let choice = or_invalid_e (Engines.resolve ext.deployed raw) in
       (match Engines.unavailable ext.deployed choice with
        | Some why -> raise (Tool_args.Invalid why)
        | None -> choice)
     | None ->
       if
         String.Caseless.equal raw "harness"
         || String.Caseless.equal raw "prigh harness"
       then Engine_choice.harness
       else
         raise
           (Tool_args.Invalid
              (sprintf
                 "no engine %S here: this backend runs harness only (start \
                  prigh serve with -engines harness,composer)"
                 raw)))
;;

let is_harness_choice (c : Engine_choice.t) =
  match c with
  | { engine = Harness; _ } | { engine = Composer; adapter = Some Prigh } ->
    true
  | { engine = Composer
    ; adapter = Some (Codex | Claude | Acp _ | Backend _) | None
    } -> false
;;

let prepare
      ~models
      ~hosts
      ~engines_opt
      ~default_engine
      ~current_model
      ~current_thinking
      ~make_nested
      (context : Tool.Context.t)
      args
  =
  let task = Tool_args.string args "task" in
  let given =
    Option.bind (Tool_args.string_opt args "name") ~f:Subagent_name.of_given
  in
  let only = Tool_args.string_list_opt args "tools" in
  let session_engine =
    Option.value_map
      engines_opt
      ~default:Engine_choice.harness
      ~f:(fun (e : External.t) -> e.current ())
  in
  let default_engine = Option.value default_engine ~default:session_engine in
  let choice = resolve_engine ~engines_opt ~default_engine args in
  let same_engine = Engine_choice.equal choice session_engine in
  let engine_label =
    if same_engine then None else Some (Engine_choice.key choice)
  in
  let fork_source =
    match Tool_args.bool_opt args "fork", context.conversation, engines_opt with
    | (None | Some false), _, _ -> None
    | Some true, Some conversation, _ -> Some (Fork.Conversation conversation)
    | Some true, None, Some (ext : External.t) ->
      (match ext.parent_messages () with
       | [] ->
         raise
           (Tool_args.Invalid
              "fork is not available here (there is no conversation to fork): \
               call subagent without it, with a self-contained task")
       | msgs -> Some (Fork.Messages msgs))
    | Some true, None, None ->
      raise
        (Tool_args.Invalid
           "fork is not available here (there is no conversation to fork): \
            call subagent without it, with a self-contained task")
  in
  let all_hosts = hosts () in
  let host, moves = host_of ~hosts:all_hosts context args in
  let cwd =
    match Tool_args.string_opt args "cwd", host with
    | cwd, Some host when moves -> or_invalid (Tool_on_host.cwd_on host cwd)
    | cwd, None when moves ->
      (* back to the session's active host *)
      or_invalid
        (Tool_on_host.cwd_on
           (List.find_exn all_hosts ~f:(fun (h : Tool_on_host.Host.t) ->
              h.active))
           cwd)
    | None, _ -> context.cwd
    | Some cwd, _ -> Tool.resolve_path context cwd
  in
  let target, thinking =
    if is_harness_choice choice
    then (
      let fallback_model () =
        match engines_opt with
        | Some (ext : External.t) -> ext.harness_model ()
        | None -> current_model ()
      in
      let model =
        match Tool_args.string_opt args "model", fork_source with
        | None, Some (Conversation conv)
          when not (External_model.is_external (Model.key conv.model)) ->
          conv.model
        | None, _ ->
          let cur = current_model () in
          if same_engine && not (External_model.is_external (Model.key cur))
          then cur
          else fallback_model ()
        | Some name, _ ->
          (match Model_registry.resolve models name with
           | Ok model -> model
           | Error e ->
             let hint =
               match engines_opt, String.lsplit2 name ~on:'/' with
               | Some ext, Some (prefix, rest) ->
                 (match Engines.resolve ext.deployed prefix with
                  | Ok c when not (is_harness_choice c) ->
                    sprintf
                      " (to run on %s, pass engine: %S and model: %S)"
                      (Engine_choice.display_name c)
                      (Engine_choice.key c)
                      rest
                  | Ok _ | Error _ -> "")
               | _ -> ""
             in
             raise (Tool_args.Invalid (Error.to_string_hum e ^ hint)))
      in
      let thinking =
        match Tool_args.string_opt args "thinking", fork_source with
        | None, Some (Conversation conv) when same_engine ->
          Model.clamp_thinking model conv.thinking
        | None, _ ->
          let base =
            if same_engine then current_thinking () else Thinking.default
          in
          Model.clamp_thinking model base
        | Some name, _ ->
          or_invalid_e
            (Or_error.bind
               (Thinking.of_string name)
               ~f:(Model.check_thinking model))
      in
      Target.Harness { model }, thinking)
    else (
      let adapter =
        match choice.adapter with
        | Some a -> a
        | None ->
          raise
            (Tool_args.Invalid
               "the composer engine needs an adapter (e.g. codex, claude, \
                acp:antigravity)")
      in
      let ext =
        match engines_opt with
        | Some ext -> ext
        | None ->
          raise
            (Tool_args.Invalid
               (sprintf
                  "cannot run %s here: external engines are not configured"
                  (Adapter_id.label adapter)))
      in
      let host_id, host_name =
        match host with
        | Some h -> h.id, h.name
        | None ->
          (match
             List.find all_hosts ~f:(fun (h : Tool_on_host.Host.t) -> h.active)
           with
           | Some h -> h.id, h.name
           | None -> "backend", "local")
      in
      let adapter_name = Adapter_id.to_string adapter in
      let catalog_models = ext.external_models ~host:host_id adapter in
      let ext_model =
        match Tool_args.string_opt args "model" with
        | None ->
          if same_engine && not moves
          then External_model.native_id (current_model ())
          else None
        | Some query ->
          Some
            (or_invalid_e
               (External_model.resolve
                  ~adapter:adapter_name
                  ~models:catalog_models
                  query))
      in
      let matched_model =
        match ext_model, catalog_models with
        | Some id, Some ms ->
          List.find ms ~f:(fun (m : Model.t) -> String.equal m.id id)
        | None, Some ms ->
          List.find ms ~f:(Fn.non External_model.is_placeholder)
        | _, None -> None
      in
      let display_model =
        match ext_model, matched_model with
        | Some id, _ -> External_model.name ~adapter:adapter_name (Some id)
        | None, Some m -> External_model.name ~adapter:adapter_name (Some m.id)
        | None, None -> External_model.name ~adapter:adapter_name None
      in
      let thinking =
        match Tool_args.string_opt args "thinking" with
        | None -> if same_engine then current_thinking () else Thinking.default
        | Some name ->
          let parsed = or_invalid_e (Thinking.of_string name) in
          (match matched_model with
           | Some m when not (List.is_empty m.thinking_levels) ->
             or_invalid_e (Model.check_thinking m parsed)
           | Some _ | None -> parsed)
      in
      ( Target.External
          { adapter; model = ext_model; display_model; host_id; host_name }
      , thinking ))
  in
  let turn_limit =
    Option.value (Tool_args.int_opt args "max_turns") ~default:max_turns
  in
  let depth = context.depth + 1 in
  let shared =
    match fork_source, target with
    | Some (Conversation conversation), Harness { model } ->
      Option.is_none only
      && (not moves)
      && same_engine
      && String.equal (Model.key model) (Model.key conversation.model)
    | _ -> false
  in
  let tools, refused =
    if shared
    then Subagent_fork.shared_tools context.tools ~depth
    else (
      let parent_tools =
        match target with
        | Harness { model } -> Tools.for_model model context.tools
        | External _ -> context.tools
      in
      match Tools.for_context ~parent:parent_tools ~depth ?only () with
      | Ok tools ->
        ( List.map tools ~f:(fun (tool : Tool.t) ->
            if String.equal (Tool.name tool) spec.name
            then make_nested ~default_engine:choice
            else tool)
        , [] )
      | Error e -> raise (Tool_args.Invalid (Error.to_string_hum e)))
  in
  (* The computer is the active host's screen. *)
  let tools =
    match host with
    | None -> tools
    | Some _ ->
      List.filter tools ~f:(fun t ->
        not (String.equal (Tool.name t) Tool_computer.name))
  in
  { Prepared.task
  ; name =
      Option.value_or_thunk given ~default:(fun () ->
        Subagent_name.of_task task)
  ; named = Option.is_some given
  ; choice
  ; session_engine
  ; engine_label
  ; target
  ; thinking
  ; cwd
  ; turn_limit
  ; extra_context = Tool_args.string_opt args "context"
  ; tools
  ; depth
  ; host
  ; worktree = worktree_arg args
  ; created = None
  ; fork =
      Option.map fork_source ~f:(fun source -> { Fork.source; shared; refused })
  }
;;

(* The worktree [p] asks for, made on the tool host; [p] then works in it. *)
let make_worktree ~worktrees_dir (context : Tool.Context.t) (p : Prepared.t) =
  match p.worktree with
  | None -> Ok p
  | Some wanted ->
    let branch, fresh =
      match wanted with
      | `Auto -> auto_branch p.name, true
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

(* [steer], [await_steer] and [wake] for an external harness subagent (no prigh shell jobs). *)
let ext_feeds ~(context : Tool.Context.t) inbox =
  let steer () =
    Option.value_map inbox ~default:[] ~f:Subagent_inbox.take_steers
  in
  let await_steer () =
    match inbox with
    | Some inbox -> Subagent_inbox.await_steer inbox
    | None -> Fiber.await_cancel ()
  in
  let rec wake () =
    match Option.value_map inbox ~default:[] ~f:Subagent_inbox.take_all with
    | _ :: _ as messages -> messages
    | [] ->
      (match inbox with
       | Some inbox
         when (not (Cancellation.is_cancelled context.cancel))
              && not (Subagent_inbox.close_if_empty inbox) -> wake ()
       | Some _ | None -> [])
  in
  steer, await_steer, wake
;;

module Ext_state = struct
  type t =
    { external_ : External.t
    ; session : Session.t
    ; developer_instructions : string option
    ; mutable thread_id : string option
    ; mutable handoff_from : Engine_choice.t option
    }
end

(* A subagent across its runs: it runs again from [messages] (or its
   external thread) when continued after running out of turns or chatted
   with by the user. Its totals cover every run. *)
module State = struct
  type t =
    { context : Tool.Context.t
      (** its call's, for what outlives a run (each run has its own
          [cancel] and [emit]) *)
    ; prepared : Prepared.t
    ; agent_id : string
    ; config : Agent_loop.Config.t option
    ; ext : Ext_state.t option
    ; worktree : Agent_worktree.t option
    ; mutable messages : Message.t list
    ; mutable turns : int
    ; mutable local_usage : Usage.t (** its own replies' *)
    ; mutable local_cost_usd : float
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
      ~init:(t.local_cost_usd +. t.compaction_cost_usd)
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
      ; "model", String (Target.trace_model p.target)
      ; "engine", String (Engine_choice.key p.choice)
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
       ; name = p.name
       ; model = Target.display_model p.target
       ; thinking = p.thinking
       ; tools =
           (match p.target with
            | Harness _ -> List.map p.tools ~f:Tool.name
            | External _ -> [])
       ; host = Option.map p.host ~f:(fun h -> h.name)
       ; engine = p.engine_label
       ; cwd = p.cwd
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
  let added, left, ext_cost =
    match p.target, state.config, state.ext with
    | Harness _, Some config, _ ->
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
            ~config:{ config with max_turns = Some turn_limit }
            ~cwd:p.cwd
            ~cancel:context.cancel
            ~chat:context.chat
            ?host:context.host
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
      added, left, 0.
    | External { adapter; model; host_id; host_name; _ }, _, Some ext_state ->
      let steer, await_steer, wake = ext_feeds ~context inbox in
      let { External.messages; cost_usd } =
        match
          ext_state.external_.run
            ~adapter
            ~host_id
            ~host_name
            ~cwd:p.cwd
            ~model
            ~thinking:p.thinking
            ~approval:(ext_state.external_.approval_mode ())
            ~plan:(ext_state.external_.plan_mode ())
            ~developer_instructions:ext_state.developer_instructions
            ~resume:ext_state.thread_id
            ~handoff_from:ext_state.handoff_from
            ~session:ext_state.session
            ~on_thread:(fun tid ->
              ext_state.thread_id <- Some tid;
              ext_state.handoff_from <- None)
            ~depth:p.depth
            ~agent_id
            ~home:context.home
            ~tools:p.tools
            ~context
            ~emit
            ~steer
            ~await_steer
            ~wake
            ~cancel:context.cancel
            ~prompts
        with
        | result -> result
        | exception exn ->
          let text =
            match exn with
            | Eio.Cancel.Cancelled _ -> "[cancelled]"
            | exn -> "subagent failed: " ^ Exn.to_string exn
          in
          Eio.Cancel.protect (fun () -> end_ (Tool.Result.error text));
          raise exn
      in
      messages, [], cost_usd
    | _ -> assert false
  in
  (match p.target with
   | Harness { model } ->
     List.iter added ~f:(function
       | Message.Assistant assistant ->
         state.local_usage <- Usage.add state.local_usage assistant.usage;
         state.local_cost_usd
         <- state.local_cost_usd +. Model.cost_usd model assistant.usage
       | User _ | Tool_result _ -> ())
   | External _ ->
     List.iter added ~f:(function
       | Message.Assistant assistant ->
         state.local_usage <- Usage.add state.local_usage assistant.usage
       | User _ | Tool_result _ -> ());
     state.local_cost_usd <- state.local_cost_usd +. ext_cost);
  let usage = State.usage state in
  let last_assistant =
    List.rev added
    |> List.find_map ~f:(function
      | Message.Assistant assistant -> Some assistant
      | User _ | Tool_result _ -> None)
  in
  let out_of_turns =
    match p.target, last_assistant with
    | ( Harness _
      , Some ({ stop_reason = End_turn | Tool_use | Length; _ } as assistant) )
      ->
      !run_turns >= turn_limit
      && (not (List.is_empty (Message.Assistant.tool_calls assistant)))
      && (not (Cancellation.is_cancelled context.cancel))
      && Option.is_none (context.halt ())
    | External _, _ | Harness _, (Some _ | None) -> false
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
  ( result
  , out_of_turns && Option.is_some hint
  , Cancellation.is_cancelled context.cancel )
;;

let fork_messages (source : Fork.source) ~call_id =
  match source with
  | Conversation conv -> Subagent_fork.context conv ~call_id
  | Messages msgs -> msgs
;;

let execute
      ?inbox
      ~provider
      ~engines_opt
      ~compaction
      ~home
      ~user_info
      ~custom_instructions
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
      ; host = Option.map p.host ~f:(fun h -> h.name)
      })
  in
  let home =
    Option.value (Option.bind p.host ~f:(fun h -> h.home)) ~default:home
  in
  let config, ext, messages, prompt =
    match p.target with
    | Harness { model } ->
      let own_system () =
        let base =
          match p.extra_context with
          | Some extra when Option.is_none p.fork ->
            extra ^ "\n\n" ^ instructions
          | Some _ | None -> instructions
        in
        let { Host_ops.Instructions.files; nix; skills } =
          if context.chat
          then { Host_ops.Instructions.files = []; nix = false; skills = [] }
          else
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
               ~chat:context.chat
               ~instructions:files
               ~nix
               ~skills
               ~role:Subagent
               ?user_info:(user_info ())
               ~custom_instructions:(custom_instructions ())
               ?host:(Option.map p.host ~f:(fun h -> h.name))
               ~cwd:p.cwd
               ~home
               ~tools:(Tools.specs p.tools)
               ())
      in
      let system, cache_key =
        match p.fork with
        | Some { shared = true; source = Conversation conversation; _ } ->
          conversation.system, context.cache_key
        | Some _ | None ->
          ( own_system ()
          , Some
              (match context.cache_key with
               | Some parent -> parent ^ "/" ^ agent_id
               | None -> agent_id) )
      in
      let config =
        { Agent_loop.Config.model
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
        | Some { source; shared; refused } ->
          ( fork_messages source ~call_id:context.call_id
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
      Some config, None, messages, prompt
    | External _ ->
      let external_ = Option.value_exn engines_opt in
      let session = Session.create ~dir:p.cwd ~cwd:p.cwd ~temporary:true () in
      let handoff_from, prompt =
        match p.fork with
        | None -> None, Message.user p.task
        | Some { source; _ } ->
          List.iter (fork_messages source ~call_id:context.call_id) ~f:(fun m ->
            ignore (Session.append_message session m : Session.Entry.t));
          ( Some p.session_engine
          , Subagent_fork.prompt
              { task = p.task
              ; extra_context = p.extra_context
              ; parent_cwd = context.cwd
              ; cwd = p.cwd
              ; worktree
              ; refused = []
              ; sync_subagents = false
              } )
      in
      let developer_instructions =
        let parts =
          [ Some instructions
          ; (match p.extra_context with
             | Some extra when Option.is_none p.fork -> Some extra
             | Some _ | None -> None)
          ; Option.map worktree ~f:(fun w ->
              sprintf
                "You work in a git worktree made for you at %s on branch %s."
                w.path
                w.branch)
          ; System_prompt.user_sections
              ?user_info:(user_info ())
              ~custom_instructions:(custom_instructions ())
              ()
          ]
        in
        Some (String.concat ~sep:"\n\n" (List.filter_opt parts))
      in
      ( None
      , Some
          { Ext_state.external_
          ; session
          ; developer_instructions
          ; thread_id = None
          ; handoff_from
          }
      , []
      , prompt )
  in
  let state =
    { State.context
    ; prepared = p
    ; agent_id
    ; config
    ; ext
    ; worktree
    ; messages
    ; turns = 0
    ; local_usage = Usage.zero
    ; local_cost_usd = 0.
    ; compaction_usage = Usage.zero
    ; compaction_cost_usd = 0.
    ; children = String.Table.create ()
    }
  in
  let result, out_of_turns, cancelled =
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
  result, state, out_of_turns, cancelled
;;

(* What it is told when continued with a message: like a [subagent_message]
   steer, or the user's. *)
let continue_message ((from : Subagent_inbox.From.t), text) =
  let note =
    Injected.Note.steer
      ~from_user:(Subagent_inbox.From.equal from User)
      ~follow_up:false
  in
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
  let needs_prompt =
    Option.is_some state.ext
    ||
    match List.last state.messages with
    | Some (Message.Tool_result _) -> false
    | Some (User _ | Assistant _) | None -> true
  in
  let prompts =
    match message with
    | Some m -> [ continue_message m ]
    | None when needs_prompt -> [ continue_message (Caller, "Continue.") ]
    | None -> []
  in
  let result, out_of_turns, cancelled =
    run_once
      ?inbox
      ~provider
      ~resumable
      ~continued:true
      state
      context
      ~turn_limit:(Option.value max_turns ~default:state.prepared.turn_limit)
      ~prompts
  in
  result, state, out_of_turns, cancelled
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

(* Parks a non-cancelled background subagent under its task's id when it ends,
   for [subagent_continue] (or the user to carry on with). *)
let rec background_outcome
          ~provider
          background
          ~id
          (result, state, out_of_turns, cancelled)
  =
  if not cancelled
  then
    Background_tasks.park
      background
      id
      ~out_of_turns
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
              ~message));
  outcome_of result ~parked:out_of_turns
;;

(* A non-cancelled synchronous subagent is parked in its caller's tasks, which
   run it again from [subagent_continue] (in the caller's run, with that call's
   [cancel] and [emit]). *)
let rec park_sync ~provider tasks ~out_of_turns (state : State.t) =
  Background_tasks.park
    tasks
    state.agent_id
    ~out_of_turns
    (fun ~max_turns ~message ~inbox:_ ~call_id ~cancel ~emit ->
       let call_id = Option.value call_id ~default:state.context.call_id in
       let result, state, out_of_turns, cancelled =
         continue_run
           ~provider
           ~resumable:(Sync tasks)
           state
           { state.context with cancel; emit; call_id }
           ~max_turns
           ~message
       in
       if not cancelled then park_sync ~provider tasks ~out_of_turns state;
       outcome_of result ~parked:out_of_turns)
;;

let rec create_with
          ?name_task
          ~models
          ~worktrees_dir
          ~compaction
          ~user_info
          ~custom_instructions
          ~hosts
          ~engines
          ~default_engine
          ~(spec : Tool_spec.t)
          ~provider
          ~current_model
          ~current_thinking
          ~home
          ()
  =
  let make_nested ~default_engine =
    create_with
      ?name_task
      ~models
      ~worktrees_dir
      ~compaction
      ~user_info
      ~custom_instructions
      ~hosts
      ~engines
      ~default_engine:(Some default_engine)
      ~spec:{ spec with description = sync_description }
      ~provider
      ~current_model
      ~current_thinking
      ~home
      ()
  in
  let run (context : Tool.Context.t) args =
    let engines_opt = engines () in
    let prepared =
      prepare
        ~models
        ~hosts
        ~engines_opt
        ~default_engine
        ~current_model
        ~current_thinking
        ~make_nested
        context
        args
    in
    (* From here on, its host's: its worktree, instructions and tools. *)
    let context =
      match prepared.host with
      | None -> { context with host = None }
      | Some host ->
        { context with host = Some host.id; home = (fun () -> host.home) }
    in
    let prepared =
      match name_task, prepared.named with
      | None, _ | _, true -> prepared
      | Some name_task, false ->
        let siblings =
          Option.value_map context.background ~default:[] ~f:(fun background ->
            Background_tasks.summaries background ~kind:Subagent
            |> List.filter_map ~f:(fun (s : Background_tasks.Summary.t) ->
              s.name))
        in
        (match
           name_task ~task:prepared.task ~siblings ~cancel:context.cancel
         with
         | Some name -> { prepared with name }
         | None -> prepared)
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
             ~name:prepared.name
             ?host:(Option.map prepared.host ~f:(fun h -> h.name))
             ~label:prepared.task
             ~run:(fun ~id ~cancel ~emit ~on_output:_ ->
               background_outcome
                 ~provider
                 background
                 ~id
                 (execute
                    ~inbox
                    ~provider
                    ~engines_opt
                    ~compaction
                    ~home
                    ~user_info
                    ~custom_instructions
                    ~resumable:Background
                    { context with cancel; emit; on_output = ignore }
                    prepared
                    ~agent_id:id))
         in
         let where =
           List.filter_opt
             [ Option.map prepared.engine_label ~f:(fun _ ->
                 Engine_choice.display_name prepared.choice)
             ; Option.map prepared.host ~f:(fun h -> h.name)
             ]
         in
         let where_text =
           match where with
           | [] -> ""
           | parts -> " on " ^ String.concat ~sep:", " parts
         in
         Tool.Result.ok
           (sprintf
              "started agent %s%s (%s%s); its result will be delivered to you \
               when it finishes: keep working or end your turn meanwhile"
              id
              where_text
              (if Option.is_some prepared.fork then "forked: " else "")
              (Background_tasks.short_task prepared.task))
       | _, resumable ->
         let agent_id =
           match context.agent_id with
           | None -> context.call_id
           | Some parent -> parent ^ "/" ^ context.call_id
         in
         let result, state, out_of_turns, cancelled =
           execute
             ~provider
             ~engines_opt
             ~compaction
             ~home
             ~user_info
             ~custom_instructions
             ~resumable
             context
             prepared
             ~agent_id
         in
         (match resumable with
          | Sync tasks when not cancelled ->
            park_sync ~provider tasks ~out_of_turns state
          | Background | Sync _ | No -> ());
         result)
  in
  { Tool.spec; run }
;;

let create
      ?name_task
      ?(models = Model_registry.builtin ())
      ?(worktrees_dir = fun () -> "~/.prigh/worktrees")
      ?(compaction = fun () -> Compaction_settings.window_only)
      ?(user_info = fun () -> None)
      ?(custom_instructions = fun () -> [])
      ?(hosts = fun () -> [])
      ?(engines = fun () -> None)
      ~provider
      ~current_model
      ~current_thinking
      ~home
      ()
  =
  create_with
    ?name_task
    ~models
    ~worktrees_dir
    ~compaction
    ~user_info
    ~custom_instructions
    ~hosts
    ~engines
    ~default_engine:None
    ~spec
    ~provider
    ~current_model
    ~current_thinking
    ~home
    ()
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
  ; native = None
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
        "Continue a finished, failed, or out-of-turns subagent: it carries on \
         from where it stopped, with its context (or external thread), under \
         the same id. A background one runs in the background again and its \
         report arrives as usual; your own synchronous subagent runs now and \
         returns its report."
      (Tool_args.schema
         ~required:[ "id" ]
         [ "id", `String, "Agent id"
         ; ( "max_turns"
           , `Integer
           , "How many more turns (default: its limit when it started)" )
         ; ( "message"
           , `String
           , "Follow-up message to send it, as from its caller (e.g. what to \
              do next or focus on)" )
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
          ~message:
            (Option.map message ~f:(fun m -> Subagent_inbox.From.Caller, m))
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
