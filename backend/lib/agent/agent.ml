open! Core
open! Import

module Queued = struct
  type t =
    { text : string (** as typed *)
    ; expanded : string option
      (** what the model gets instead: a skill's or a command's prompt *)
    ; source : Injected.Source.t option
    ; attachments : string list
    ; images : Image.t list
    }
  [@@deriving sexp_of]
end

module Pending_confirm = struct
  type t =
    { call_id : string
    ; agent_id : string option
    ; name : string
    ; summary : string
    ; rules : string list
    }
  [@@deriving sexp_of]

  let key ~call_id ~agent_id =
    match agent_id with
    | None -> call_id
    | Some agent -> agent ^ "/" ^ call_id
  ;;
end

module Confirm_answer = struct
  type t =
    | Allow
    | Always
    | Deny of string option
  [@@deriving sexp_of]
end

module Pending_question = struct
  type t =
    { call_id : string
    ; questions : User_question.t list
    ; deadline : float option
    }
  [@@deriving sexp_of]
end

(* A confirmation as the agent keeps it: [recheck] says whether the call
   would now run without asking (after a rule or mode change). *)
module Waiting_confirm = struct
  type t =
    { pending : Pending_confirm.t
    ; resolver : Confirm_answer.t Promise.u
    ; recheck : unit -> bool
    }
end

module Host = struct
  type t =
    { id : string
    ; name : string
    ; cwd : string
    ; session_id : string option
    ; session_name : string option
    ; exec_input : bool [@sexp.bool]
    ; git_location : bool [@sexp.bool]
    }
  [@@deriving sexp_of, equal]

  let backend_id = "backend"
end

module State = struct
  type t =
    { session_id : string
    ; session_path : string
    ; session_name : string option
    ; session_description : string option
    ; cwd : string
    ; git_branch : string option
    ; model : Model.t
    ; thinking : Thinking.t
    ; running : bool
    ; message_count : int
    ; usage : Usage.t
    ; cost_usd : float
    ; context_tokens : int
    ; active_host : string
    ; hosts : Host.t list
    ; subagents : Background_tasks.Summary.t list
    ; jobs : Background_tasks.Summary.t list
    ; budget_usd : float option
    ; goal : Goal.t option
    ; goal_max_continuations : int
    ; reminders : Reminder.t list
    ; reminder_stops : int
    ; resume_at : float option
    ; approval_mode : Approval_mode.t
    ; plan_mode : bool
    ; plan : Plan.t option
    ; session_rules : string list
    ; home : Session.Home.t option
    }
  [@@deriving sexp_of]
end

module Session_stats = struct
  type t =
    { message_count : int
    ; turns : int
    ; tool_calls : (string * int) list
    ; usage : Usage.t
    ; cost_usd : float
    ; context_percent : float
    ; model_changes : int
    ; compactions : int
    ; duration_seconds : float
    }
  [@@deriving sexp_of]
end

module Event = struct
  type t =
    | Loop of Agent_event.t
    | State_changed of State.t
    | Compacted of { summary : string }
    | Notice of string
    | Config_changed of Config.t
    | Review_changed of Review_state.t
    | Queue_update of
        { steer : string list
        ; follow_up : string list
        }
    | Subagent_queues of Subagent_inbox.Listing.t list
    | Tool_exec of
        { host : string
        ; exec_id : string
        ; call_id : string
        ; name : string
        ; arguments : Json.t
        ; cwd : string
        ; stdin : bool
          (** the process gets a stdin pipe fed by [Tool_exec_input] *)
        }
    | Tool_exec_cancel of
        { host : string
        ; exec_id : string
        }
    | Tool_exec_input of
        { host : string
        ; exec_id : string
        ; data : string
        ; close : bool (** close stdin after [data] *)
        }
  [@@deriving sexp_of]
end

module Pending_exec = struct
  type t =
    { on_output : string -> unit
    ; resolver : Tool_result.t Promise.u
    }
end

module Run = struct
  type t =
    { cancel : Cancellation.t
    ; finished : unit Promise.t
    }
end

module Resume = struct
  type t =
    { at : float
    ; cancel : Cancellation.t
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; provider : Provider.t
  ; tools : Tool.t list
  ; sessions_dir : string
  ; home : string
  ; models : Model_registry.t
  ; backend_host_enabled : bool
  ; mutable session : Session.t
  ; mutable cwd : string
  ; mutable git_branch : string option
  ; mutable repo_asked : (string * string) option
    (** the client host and directory last asked where it is in git *)
  ; mutable model : Model.t
  ; mutable thinking : Thinking.t
  ; mutable run : Run.t option
  ; mutable describing : unit Promise.t option
  ; auto_describe : bool
  ; queue : Queued.t Steer_queue.t
  ; mutable subscribers : (Event.t -> unit) list
  ; mutable config : Config.t
  ; pending_confirms : Waiting_confirm.t String.Table.t
  ; pending_questions :
      (Pending_question.t * User_question.Reply.t Promise.u) String.Table.t
  ; mutable approval_mode : Approval_mode.t
  ; mutable plan_mode : bool
  ; mutable session_rules : Approval_rule.t list
  ; mutable plan : Plan.t option
  ; mutable shell_seq : int
  ; mutable hosts : Host.t list
    (** every connected client able to run tools, set by the server *)
  ; host_cwds : string String.Table.t
    (** where this session last was on each client host, connected or not,
        overriding [Host.cwd] *)
  ; mutable backend_cwd : string (** the backend host's own cwd *)
  ; mutable active_host : string
  ; mutable active_host_name : string (** as last seen, for when it is gone *)
  ; mutable host_pinned : bool (** chosen explicitly via [set_active_host] *)
  ; pending_execs : Pending_exec.t String.Table.t
  ; mutable exec_seq : int
  ; mutable told : Told_environment.t
    (** what the model was last told, as [Session.told_environment] records
        it; the difference to now is told with the next user message *)
  ; background : Background_tasks.t
  ; subagent_log : Subagent_log.t
  ; mcp : Mcp_hub.t option
    (** the backend's own servers; [None] turns MCP off on every host *)
  ; mutable mcp_tools : Tool.t list (** as of the latest run's start *)
  ; mutable mcp_noticed : String.Set.t
    (** MCP problems already reported, so each is told once *)
  ; max_budget_usd : float option
    (** the budget of sessions that set none, overriding the config's *)
  ; mutable goal : Goal.t option
  ; mutable review : Review_state.t (** as [Session.review] reads it *)
  ; mutable reminders : Reminder.t list
  ; reminder_tracker : Reminder.Tracker.t
  ; mutable resume : Resume.t option
    (** a run stopped by a usage limit, resuming when it resets *)
  ; mutable past_resets : int
    (** usage limits in a row whose reset time had already passed *)
  ; sleep_until : float -> unit
  }

let path_messages session =
  List.filter_map (Session.active_path session) ~f:(fun (e : Session.Entry.t) ->
    match e.payload with
    | Message m -> Some m
    | _ -> None)
;;

(* The approval mode, plan mode, the session's rules and its plan, as the
   session recorded them; a session without an approval entry starts in the
   config's mode. *)
let restore_approvals t =
  (match Session.approval t.session with
   | Some (mode, plan) ->
     t.approval_mode
     <- Result.ok (Approval_mode.of_string mode)
        |> Option.value ~default:t.config.approval_mode;
     t.plan_mode <- plan
   | None ->
     t.approval_mode <- t.config.approval_mode;
     t.plan_mode <- false);
  t.session_rules
  <- List.filter_map (Session.allow_rules t.session) ~f:(fun rule ->
       Result.ok (Approval_rule.of_string rule));
  t.plan <- Plan.latest (path_messages t.session)
;;

let restore_settings t =
  t.goal <- Session.goal t.session;
  t.review <- Session.review t.session;
  t.reminders <- Session.reminders t.session;
  (match Session.model t.session with
   | Some (model_id, thinking) ->
     Option.iter (Model_registry.find t.models model_id) ~f:(fun m ->
       t.model <- m);
     t.thinking <- thinking
   | None -> ());
  restore_approvals t
;;

let backend_host t =
  { Host.id = Host.backend_id
  ; name = Core_unix.gethostname ()
  ; cwd = t.backend_cwd
  ; session_id = None
  ; session_name = None
  ; exec_input = false (* runs in this process: no messages *)
  ; git_location = true
  }
;;

let hosts t =
  let clients =
    List.map t.hosts ~f:(fun h ->
      match Hashtbl.find t.host_cwds h.id with
      | Some cwd -> { h with cwd }
      | None -> h)
  in
  if t.backend_host_enabled then backend_host t :: clients else clients
;;

let find_branch t ~cwd =
  if t.backend_host_enabled
  then Option.bind (Git_location.find ~cwd) ~f:(fun l -> l.branch)
  else None
;;

let active_host t = t.active_host
let subscribe t ~f = t.subscribers <- f :: t.subscribers

(* A subagent's worktree, at any depth. *)
let rec started_worktree (e : Agent_event.t) =
  match e with
  | Subagent_start { worktree; _ } -> worktree
  | Subagent { event; _ } -> started_worktree event
  | _ -> None
;;

(* [Worktrees_changed] at any depth, with the id of the subagent whose it
   is. *)
let rec worktrees_changed ?agent (e : Agent_event.t) =
  match e with
  | Worktrees_changed { added; removed } -> Some (agent, added, removed)
  | Subagent { agent_id; event; _ } -> worktrees_changed ~agent:agent_id event
  | _ -> None
;;

let record_worktrees t ~agent ~added ~removed =
  let recorded path =
    List.exists (Session.worktrees t.session) ~f:(fun w ->
      String.equal w.path path)
  in
  List.iter added ~f:(fun (w : Session_worktree.t) ->
    if not (recorded w.path)
    then (
      let w =
        match agent, w.source with
        | Some agent, Created ->
          let task =
            Option.value_map
              (Subagent_log.find t.subagent_log agent)
              ~default:""
              ~f:(fun ((s : Subagent_log.Summary.t), _) -> s.task)
          in
          { w with source = Agent { agent; task } }
        | _ -> w
      in
      ignore (Session.add_worktree t.session w : Session.Entry.t)));
  List.iter removed ~f:(fun path ->
    if recorded path
    then ignore (Session.worktree_removed t.session ~path : Session.Entry.t))
;;

let broadcast t (event : Event.t) =
  match event with
  | Loop e when Option.is_some (worktrees_changed e) ->
    Option.iter (worktrees_changed e) ~f:(fun (agent, added, removed) ->
      record_worktrees t ~agent ~added ~removed)
  | _ ->
    (match event with
     | Loop e ->
       Subagent_log.record
         t.subagent_log
         ~now:(Eio.Time.now (Eio.Stdenv.clock t.env))
         e;
       Option.iter (started_worktree e) ~f:(fun w ->
         ignore (Session.add_agent_worktree t.session w : Session.Entry.t))
     | _ -> ());
    List.iter (List.rev t.subscribers) ~f:(fun f -> f event)
;;

let session t = t.session
let messages t = Session.messages t.session
let is_running t = Option.is_some t.run

(* The model that wrote [a], for its price. *)
let model_of t (a : Message.Assistant.t) =
  if String.equal a.model t.model.id || String.equal a.model (Model.key t.model)
  then t.model
  else Option.value (Model_registry.find t.models a.model) ~default:t.model
;;

(* Every response in the session file (all branches, also those a compaction
   summarised), each at its own model's price, and the recorded spend of
   everything else (subagents, compactions, [btw]). *)
let session_spend t =
  List.fold
    (Session.entries t.session)
    ~init:(Session.spend t.session)
    ~f:(fun ((usage, cost) as acc) (e : Session.Entry.t) ->
      match e.payload with
      | Message (Assistant a) ->
        Usage.add usage a.usage, cost +. Model.cost_usd (model_of t a) a.usage
      | _ -> acc)
;;

let spent_usd t = snd (session_spend t)

let budget t =
  match Session.budget t.session with
  | Some usd -> usd
  | None -> Option.first_some t.max_budget_usd t.config.max_budget_usd
;;

let money usd = Float.to_string_hum ~decimals:2 usd

(* Why no further turn may start: the session's budget is spent. *)
let budget_exhausted t =
  match budget t with
  | Some budget ->
    let spent = spent_usd t in
    if Float.( >= ) spent budget
    then
      Some
        (sprintf
           "the session's budget of $%s is spent ($%s so far): /budget %s \
            raises it, /budget off removes it"
           (money budget)
           (money spent)
           (Int.to_string (Float.iround_up_exn (spent +. budget))))
    else None
  | None -> None
;;

let state t =
  let messages = messages t in
  let assistant_messages =
    List.filter_map messages ~f:(function
      | Message.Assistant a -> Some a
      | User _ | Tool_result _ -> None)
  in
  let assistant_usage, assistant_cost = session_spend t in
  let usage = assistant_usage in
  let context_tokens =
    Option.value_map (List.last assistant_messages) ~default:0 ~f:(fun a ->
      a.usage.input)
  in
  { State.session_id = Session.id t.session
  ; session_path = Session.path t.session
  ; session_name = Session.name t.session
  ; session_description = Session.description t.session
  ; cwd = t.cwd
  ; git_branch = t.git_branch
  ; model = t.model
  ; thinking = t.thinking
  ; running = is_running t
  ; message_count = List.length messages
  ; usage
  ; cost_usd = assistant_cost
  ; context_tokens
  ; active_host = t.active_host
  ; hosts = hosts t
  ; subagents = Background_tasks.summaries t.background ~kind:Subagent
  ; jobs = Background_tasks.summaries t.background ~kind:Job
  ; budget_usd = budget t
  ; goal = t.goal
  ; goal_max_continuations = t.config.goal_max_continuations
  ; reminders = t.reminders
  ; reminder_stops = Reminder.Tracker.stops t.reminder_tracker
  ; resume_at = Option.map t.resume ~f:(fun r -> r.at)
  ; approval_mode = t.approval_mode
  ; plan_mode = t.plan_mode
  ; plan = t.plan
  ; session_rules = List.map t.session_rules ~f:Approval_rule.to_string
  ; home = Session.home t.session
  }
;;

let state_changed t = broadcast t (State_changed (state t))

let queued_texts t =
  let texts kind =
    List.map (Steer_queue.items t.queue kind) ~f:(fun (q : Queued.t) -> q.text)
  in
  texts Steer, texts Follow_up
;;

let queue_update t =
  let steer, follow_up = queued_texts t in
  broadcast t (Queue_update { steer; follow_up })
;;

(* ---- approvals, plan mode and questions --------------------------------- *)

let record_approval t =
  ignore
    (Session.set_approval
       t.session
       ~mode:(Approval_mode.to_string t.approval_mode)
       ~plan:t.plan_mode
     : Session.Entry.t)
;;

(* Confirmations whose call would now run without asking (a rule was added,
   the mode changed) are allowed. *)
let recheck_confirms t =
  Hashtbl.filter t.pending_confirms ~f:(fun (w : Waiting_confirm.t) ->
    w.recheck ())
  |> Hashtbl.iteri ~f:(fun ~key ~data:(w : Waiting_confirm.t) ->
    Hashtbl.remove t.pending_confirms key;
    Promise.resolve w.resolver Confirm_answer.Allow)
;;

let environment t =
  { Told_environment.host = t.active_host
  ; cwd = t.cwd
  ; tools =
      List.map (Tools.for_model t.model t.tools) ~f:Tool.name
      |> List.dedup_and_sort ~compare:String.compare
  ; plan_mode = t.plan_mode
  }
;;

let record_told t =
  if
    not
      (Option.equal
         Told_environment.equal
         (Session.told_environment t.session)
         (Some t.told))
  then ignore (Session.set_told_environment t.session t.told : Session.Entry.t)
;;

(* A session from before [told] was recorded starts from its environment as
   loaded: whatever it was not told then is lost. *)
let restore_told t =
  t.told
  <- (match Session.told_environment t.session with
      | Some told -> told
      | None -> environment t)
;;

(* Before a change, so that a session that has not recorded what the model
   was told yet (a new or an old one) still tells it after being loaded
   again. Before the system prompt is built only plan mode can need telling:
   the prompt describes the rest. *)
let environment_changed t =
  if
    Option.is_some (Session.system_prompt t.session)
    || not (Bool.equal t.plan_mode t.told.plan_mode)
  then record_told t
;;

let tell t told =
  t.told <- told;
  record_told t
;;

let switch_mode ?approval ?plan t =
  let approval = Option.value approval ~default:t.approval_mode in
  let plan = Option.value plan ~default:t.plan_mode in
  if
    not
      (Approval_mode.equal approval t.approval_mode
       && Bool.equal plan t.plan_mode)
  then (
    t.approval_mode <- approval;
    t.plan_mode <- plan;
    environment_changed t;
    record_approval t;
    recheck_confirms t;
    state_changed t)
;;

let set_approval_mode t mode = switch_mode t ~approval:mode

(* The note for the model when plan mode changed since it was last told. *)
let plan_mode_note t =
  if Bool.equal t.plan_mode t.told.plan_mode
  then None
  else (
    tell t { t.told with plan_mode = t.plan_mode };
    Some
      { Injected.Note.kind = Plan_mode { on = t.plan_mode }
      ; text = System_prompt.plan_mode_note ~on:t.plan_mode
      })
;;

let confirm t (context : Tool.Context.t) ~name ~summary ~rules ~recheck =
  let call_id = context.call_id in
  let agent_id = context.agent_id in
  let key = Pending_confirm.key ~call_id ~agent_id in
  let rules = List.map rules ~f:Approval_rule.to_string in
  let promise, resolver = Promise.create () in
  Hashtbl.set
    t.pending_confirms
    ~key
    ~data:
      { Waiting_confirm.pending =
          { Pending_confirm.call_id; agent_id; name; summary; rules }
      ; resolver
      ; recheck
      };
  broadcast t (Loop (Tool_confirm { call_id; agent_id; name; summary; rules }));
  let answer =
    Cancellation.protect context.cancel ~f:(fun () -> Promise.await promise)
  in
  Hashtbl.remove t.pending_confirms key;
  broadcast t (Loop (Tool_confirm_done { call_id; agent_id }));
  answer
;;

let respond_confirm ?agent_id t ~call_id (answer : Confirm_answer.t) =
  let key = Pending_confirm.key ~call_id ~agent_id in
  match Hashtbl.find t.pending_confirms key with
  | None -> Or_error.errorf "no pending confirmation for tool call %S" call_id
  | Some w ->
    Hashtbl.remove t.pending_confirms key;
    (match answer with
     | Always ->
       let added =
         List.filter_map w.pending.rules ~f:(fun rule ->
           match Approval_rule.of_string rule with
           | Ok rule
             when not (List.mem t.session_rules rule ~equal:Approval_rule.equal)
             -> Some rule
           | Ok _ | Error _ -> None)
       in
       List.iter added ~f:(fun rule ->
         ignore
           (Session.add_allow_rule
              t.session
              ~rule:(Approval_rule.to_string rule)
            : Session.Entry.t));
       t.session_rules <- t.session_rules @ added;
       Promise.resolve w.resolver answer;
       recheck_confirms t;
       if not (List.is_empty added) then state_changed t
     | Allow | Deny _ -> Promise.resolve w.resolver answer);
    Ok ()
;;

let denied_text = function
  | None -> "[denied by user]"
  | Some reason -> sprintf "[denied by user] Their reason: %s" reason
;;

let exit_plan_mode t context args =
  if not t.plan_mode
  then
    Tool.Result.error
      "plan mode is not on, so there is no plan to approve: go ahead"
  else (
    match
      confirm
        t
        context
        ~name:Tool_exit_plan_mode.name
        ~summary:(Tool_exit_plan_mode.plan args)
        ~rules:[]
        ~recheck:(fun () -> not t.plan_mode)
    with
    | None -> Tool.Result.error "[cancelled]"
    | Some (Allow | Always) when not t.plan_mode ->
      (* Released by the user switching plan mode off, not by an answer. *)
      tell t { t.told with plan_mode = false };
      Tool.Result.ok
        (sprintf
           "The user turned plan mode off without answering about this plan \
            (approval mode: %s): you can change files now. Implement the plan \
            unless they said otherwise."
           (Approval_mode.to_string t.approval_mode))
    | Some (Allow | Always) ->
      tell t { t.told with plan_mode = false };
      switch_mode t ~plan:false;
      Tool.Result.ok
        (sprintf
           "The user approved the plan. Plan mode is off (approval mode: %s), \
            so you can change files now: implement the plan."
           (Approval_mode.to_string t.approval_mode))
    | Some (Deny reason) ->
      Tool.Result.error
        (sprintf
           "[the user did not approve the plan%s] Stay in plan mode: revise \
            the plan and call exit_plan_mode again when it is ready."
           (match reason with
            | None -> ""
            | Some reason -> "; their feedback: " ^ reason)))
;;

let approve t : Tool.approver =
  fun context tool args ->
  let name = Tool.name tool in
  if String.equal name Tool_exit_plan_mode.name
  then Some (exit_plan_mode t context args)
  else (
    let call = Approval_rule.Call.create ~cwd:context.cwd ~tool:name args in
    let decide () =
      Approval.decide
        ~mode:t.approval_mode
        ~plan:t.plan_mode
        ~rules:t.config.permissions
        ~session:t.session_rules
        ~cwd:t.cwd
        ~home:(Tool.expand_home "~")
        ~destructive:(Tool_spec.Destructive.call tool.spec.destructive args)
        call
    in
    match decide () with
    | Allow -> None
    | Deny text -> Some (Tool.Result.error text)
    | Ask { rules } ->
      let recheck () =
        match decide () with
        | Allow -> true
        | Ask _ | Deny _ -> false
      in
      (match
         confirm
           t
           context
           ~name
           ~summary:(Agent_loop.tool_summary ~name args)
           ~rules
           ~recheck
       with
       | None -> Some (Tool.Result.error "[cancelled]")
       | Some (Deny reason) -> Some (Tool.Result.error (denied_text reason))
       | Some (Allow | Always) ->
         (* Plan mode may have started while the user was deciding. *)
         (match decide () with
          | Deny text -> Some (Tool.Result.error text)
          | Allow | Ask _ -> None)))
;;

let ask_user t ~cancel ~call_id ~timeout_seconds questions =
  let timeout =
    User_question.timeout
      ~config:(Config.ask_user_timeout t.config)
      ~requested:timeout_seconds
  in
  let deadline =
    Option.map timeout ~f:(fun after ->
      Eio.Time.now (Eio.Stdenv.clock t.env) +. after)
  in
  let promise, resolver = Promise.create () in
  Hashtbl.set
    t.pending_questions
    ~key:call_id
    ~data:({ Pending_question.call_id; questions; deadline }, resolver);
  broadcast t (Loop (Ask_user { call_id; questions; deadline }));
  let answered () = Promise.await promise in
  let wait =
    match timeout, deadline with
    | Some after, Some at ->
      fun () ->
        Fiber.first answered (fun () ->
          t.sleep_until at;
          (* An answer that came in meanwhile still counts. *)
          match Hashtbl.find t.pending_questions call_id with
          | Some (_, r) when phys_equal r resolver ->
            User_question.Reply.Timed_out after
          | Some _ | None -> answered ())
    | _ -> answered
  in
  let reply =
    match Cancellation.protect cancel ~f:wait with
    | None -> User_question.Reply.Cancelled
    | Some reply -> reply
  in
  Hashtbl.remove t.pending_questions call_id;
  let timed_out =
    match reply with
    | Timed_out _ -> true
    | Answered _ | Declined | Cancelled | Unavailable _ -> false
  in
  broadcast t (Loop (Ask_user_done { call_id; timed_out }));
  (match reply with
   | Timed_out after ->
     broadcast
       t
       (Notice
          (sprintf
             "the question was not answered within %s; the agent carries on \
              without it"
             (User_question.duration_hum after)))
   | Answered _ | Declined | Cancelled | Unavailable _ -> ());
  reply
;;

let answer_question t ~call_id (reply : User_question.Reply.t) =
  match Hashtbl.find t.pending_questions call_id with
  | None ->
    Or_error.errorf
      "no pending question for tool call %S: it was answered or withdrawn"
      call_id
  | Some ({ questions; _ }, resolver) ->
    let checked =
      match reply with
      | Answered answers -> User_question.check_answers questions answers
      | Declined | Cancelled | Timed_out _ | Unavailable _ -> Ok ()
    in
    Or_error.map checked ~f:(fun () ->
      Hashtbl.remove t.pending_questions call_id;
      Promise.resolve resolver reply)
;;

let pending_questions t =
  Hashtbl.data t.pending_questions
  |> List.map ~f:fst
  |> List.sort ~compare:(fun (a : Pending_question.t) b ->
    String.compare a.call_id b.call_id)
;;

let track_plan t (event : Agent_event.t) =
  match event with
  | Tool_end { call; result }
    when String.equal call.name Plan.tool_name && not result.is_error ->
    (match
       Or_error.bind
         (Content.Tool_call.parse_arguments call)
         ~f:Plan.of_arguments
     with
     | Ok plan ->
       t.plan <- plan;
       state_changed t
     | Error _ -> ())
  | _ -> ()
;;

let config t = t.config

(* Fallback models are stored as keys, so a name or prefix that resolves
   now keeps meaning the same model. *)
let set_config t (config : Config.t) =
  let open Or_error.Let_syntax in
  let%bind fallback_models =
    List.map config.fallback_models ~f:(fun name ->
      Model_registry.resolve t.models name
      |> Or_error.map ~f:Model.key
      |> Result.map_error ~f:(fun e ->
        Error.createf "fallback_models: %s" (Error.to_string_hum e)))
    |> Or_error.all
  in
  let config =
    { config with
      fallback_models =
        List.stable_dedup fallback_models ~compare:String.compare
    }
  in
  let%bind () =
    match config.default_cwd with
    | Some dir when not (Filename.is_absolute (Tool.expand_home dir)) ->
      Or_error.errorf "default_cwd: %s must be absolute (or start with ~/)" dir
    | Some dir
      when t.backend_host_enabled
           && not (Sys_unix.is_directory_exn (Tool.expand_home dir)) ->
      Or_error.errorf
        "default_cwd: %s is not a directory on the backend (%s)"
        dir
        (Core_unix.gethostname ())
    | Some _ | None -> Ok ()
  in
  let%map () = Config.save ~home:t.home config in
  let mode_changed =
    not (Approval_mode.equal config.approval_mode t.config.approval_mode)
  in
  t.config <- config;
  broadcast t (Config_changed config);
  (* [/confirm on|off] changes the default and the session at once. *)
  if mode_changed then set_approval_mode t config.approval_mode
;;

let save_as_default t =
  Or_error.bind (Config.load ~home:t.home) ~f:(fun config ->
    set_config
      t
      { config with
        default_model = Some (Model.key t.model)
      ; default_thinking = Some t.thinking
      })
;;

(* ---- tool hosts ------------------------------------------------------- *)

let find_host t id = List.find (hosts t) ~f:(fun h -> String.equal h.id id)
let active_host_connected t = Option.is_some (find_host t t.active_host)

(* A tool host predating a tool answers that it does not know it. *)
let older_host_note (host : Host.t) ~name (result : Tool.Result.t) =
  if
    result.is_error
    && String.equal result.text (sprintf "unknown host tool %S" name)
  then
    Tool.Result.error
      (sprintf
         "%s: tool host %S runs an older prigh without it; update prigh there \
          and restart its TUI or tool-host"
         result.text
         host.name)
  else result
;;

(* Runs [name] on [host]: in-process when that is the backend, otherwise
   through a [Tool_exec] round trip with the client. *)
let host_exec_with_stdin
      t
      (host : Host.t)
      ?stdin
      ~cancel
      ~on_output
      ~call_id
      ~cwd
      ~name
      ~arguments
      ()
  =
  if String.equal host.id Host.backend_id && t.backend_host_enabled
  then
    Host_ops.execute
      ~mcp:t.mcp
      ~env:t.env
      ?stdin
      ~cancel
      ~on_output
      ~cwd
      ~name
      ~arguments
      ()
  else (
    let exec_id =
      sprintf "%s/%s-%d" (Session.id t.session) call_id t.exec_seq
    in
    t.exec_seq <- t.exec_seq + 1;
    let promise, resolver = Promise.create () in
    Hashtbl.set
      t.pending_execs
      ~key:exec_id
      ~data:{ Pending_exec.on_output; resolver };
    let stdin =
      match stdin with
      | Some feed when not host.exec_input ->
        Stdin_feed.refuse
          feed
          (sprintf
             "tool host %S runs an older prigh that cannot pass input to jobs: \
              update prigh there and restart its TUI or tool-host, or start \
              the job again with its input in the command (e.g. a \
              here-document)"
             host.name);
        None
      | stdin -> stdin
    in
    let forward_input sw =
      Option.iter stdin ~f:(fun feed ->
        Fiber.fork_daemon ~sw (fun () ->
          let rec loop () =
            match Stdin_feed.take feed with
            | Data data ->
              broadcast
                t
                (Tool_exec_input
                   { host = host.id; exec_id; data; close = false });
              loop ()
            | Close ->
              broadcast
                t
                (Tool_exec_input
                   { host = host.id; exec_id; data = ""; close = true });
              `Stop_daemon
          in
          loop ()))
    in
    broadcast
      t
      (Tool_exec
         { host = host.id
         ; exec_id
         ; call_id
         ; name
         ; arguments
         ; cwd
         ; stdin = Option.is_some stdin
         });
    let result =
      Switch.run (fun sw ->
        forward_input sw;
        Cancellation.protect cancel ~f:(fun () -> Promise.await promise))
    in
    Option.iter stdin ~f:Stdin_feed.finish;
    match result with
    | Some result -> older_host_note host ~name result
    | None ->
      Hashtbl.remove t.pending_execs exec_id;
      broadcast t (Tool_exec_cancel { host = host.id; exec_id });
      Tool.Result.error "[cancelled]")
;;

let location t =
  if String.is_empty t.active_host
  then None
  else
    Some
      ( { Session.Host.id = t.active_host
        ; name = t.active_host_name
        ; pinned = t.host_pinned
        }
      , t.cwd )
;;

(* The host is recorded with the cwd, so that the session resumes there when
   it is loaded again (evicted, or after a backend restart): see
   [restore_host]. *)
let record_location t =
  let host = Option.map (location t) ~f:fst in
  if
    not
      (String.equal (Session.cwd t.session) t.cwd
       && Option.equal Session.Host.equal (Session.host t.session) host)
  then ignore (Session.set_cwd t.session ?host ~cwd:t.cwd () : Session.Entry.t)
;;

(* Where the cwd is in git, recorded in the session for the sessions list
   ({!Session.repo}): read from the backend's own files at once (the callers
   then announce the state), asked of a client host that can tell in the
   background, once per host and directory ([notify]: its answer announces
   it). *)
let record_repo ?(notify = false) t session ~cwd repo =
  if
    String.equal (Session.cwd session) cwd
    && not
         ([%equal: Git_location.t option option]
            (Session.repo session)
            (Some repo))
  then (
    ignore (Session.set_repo session ~cwd repo : Session.Entry.t);
    if notify && phys_equal session t.session then state_changed t)
;;

let refresh_location t =
  t.git_branch <- find_branch t ~cwd:t.cwd;
  match location t with
  | None -> ()
  | Some (host, cwd) when String.equal host.id Host.backend_id ->
    if t.backend_host_enabled
    then record_repo t t.session ~cwd (Git_location.find ~cwd)
  | Some (host, cwd) ->
    let place = host.id, cwd in
    (match find_host t host.id with
     | Some host
       when host.git_location
            && not
                 (Option.equal
                    [%equal: string * string]
                    t.repo_asked
                    (Some place)) ->
       t.repo_asked <- Some place;
       let session = t.session in
       (* A daemon: a host that never answers must not keep the agent's
          switch from ending. *)
       Fiber.fork_daemon ~sw:t.sw (fun () ->
         Guarded.run ~what:"finding the session's repository" (fun () ->
           match
             Git_location.of_result
               (host_exec_with_stdin
                  t
                  host
                  ~cancel:Cancellation.never
                  ~on_output:ignore
                  ~call_id:"git_location"
                  ~cwd
                  ~name:Git_location.op
                  ~arguments:(`Object [])
                  ())
           with
           | Ok repo -> record_repo ~notify:true t session ~cwd repo
           | Error _ -> (* a host predating [$git_location] *) ());
         `Stop_daemon)
     | Some _ | None -> ())
;;

(* A session that recorded no host (or the backend's, when it is disabled)
   is where a new one would be. *)
let restore_host t =
  Hashtbl.clear t.host_cwds;
  Map.iteri (Session.host_cwds t.session) ~f:(fun ~key ~data ->
    if String.equal key Host.backend_id
    then t.backend_cwd <- data
    else Hashtbl.set t.host_cwds ~key ~data);
  match Session.host t.session with
  | Some host
    when t.backend_host_enabled || not (String.equal host.id Host.backend_id) ->
    t.active_host <- host.id;
    t.active_host_name <- host.name;
    t.host_pinned <- host.pinned
  | Some _ | None ->
    t.active_host <- (if t.backend_host_enabled then Host.backend_id else "");
    t.active_host_name <- "";
    t.host_pinned <- false;
    if t.backend_host_enabled then t.backend_cwd <- t.cwd
;;

let set_host_cwd t (host : Host.t) ~cwd =
  if not (String.equal t.cwd cwd)
  then (
    t.cwd <- cwd;
    environment_changed t);
  record_location t;
  refresh_location t;
  if String.equal host.id Host.backend_id
  then t.backend_cwd <- cwd
  else Hashtbl.set t.host_cwds ~key:host.id ~data:cwd
;;

let activate_host t (host : Host.t) ~cwd ~pinned =
  if not (String.equal t.active_host host.id) then environment_changed t;
  t.active_host <- host.id;
  t.active_host_name <- host.name;
  t.host_pinned <- pinned;
  set_host_cwd t host ~cwd;
  broadcast t (Notice (sprintf "tools now run on %s in %s" host.name cwd));
  state_changed t
;;

let no_host_message =
  "no tool host connected: connect one with `prigh tool-host -connect ...` or \
   a TUI, then pick it with /host"
;;

let host_unavailable_message t =
  if String.is_empty t.active_host
  then no_host_message
  else
    sprintf
      "waiting for tool host %S to reconnect; /host picks another"
      t.active_host_name
;;

(* A session stays on its host when the host goes away, and resumes there,
   in the same cwd, when it comes back: only [set_active_host] moves it.
   Without the backend host, a session that never had a host adopts the
   first one that connects. *)
let set_hosts t clients =
  if not (List.equal Host.equal t.hosts clients)
  then (
    let was_connected = active_host_connected t in
    t.hosts <- clients;
    match find_host t t.active_host, List.hd (hosts t) with
    | None, Some host when String.is_empty t.active_host ->
      activate_host t host ~cwd:host.cwd ~pinned:false
    | Some host, _ when not was_connected ->
      t.active_host_name <- host.name;
      set_host_cwd t host ~cwd:host.cwd;
      broadcast
        t
        (Notice (sprintf "tools run on %s again in %s" host.name t.cwd));
      state_changed t
    | Some host, _ ->
      t.active_host_name <- host.name;
      state_changed t
    | None, _ ->
      if was_connected then broadcast t (Notice (host_unavailable_message t));
      state_changed t)
;;

(* Tools default to the frontend: a host attaching takes over a session that
   runs on the backend by default or has no host yet, never one whose host is
   only disconnected. *)
let prefer_host t id =
  match find_host t id with
  | None -> ()
  | Some host ->
    let take_over =
      String.is_empty t.active_host
      || ((not t.host_pinned) && String.equal t.active_host Host.backend_id)
    in
    if take_over then activate_host t host ~cwd:host.cwd ~pinned:false
;;

let tool_exec_output t ~exec_id ~chunk =
  match Hashtbl.find t.pending_execs exec_id with
  | None -> Or_error.errorf "no tool execution %S" exec_id
  | Some e ->
    e.on_output chunk;
    Ok ()
;;

let env t = t.env

let tool_exec_result t ~exec_id result =
  match Hashtbl.find t.pending_execs exec_id with
  | None -> Or_error.errorf "no tool execution %S" exec_id
  | Some e ->
    Hashtbl.remove t.pending_execs exec_id;
    Promise.resolve e.resolver result;
    Ok ()
;;

let host_exec_on t host ~cancel ~on_output ~call_id ~cwd ~name ~arguments =
  host_exec_with_stdin
    t
    host
    ~cancel
    ~on_output
    ~call_id
    ~cwd
    ~name
    ~arguments
    ()
;;

let active_host_exec
      t
      ?stdin
      ~cancel
      ~on_output
      ~call_id
      ~cwd
      ~name
      ~arguments
      ()
  =
  match find_host t t.active_host with
  | None -> Tool.Result.error (host_unavailable_message t)
  | Some host ->
    host_exec_with_stdin
      t
      host
      ?stdin
      ~cancel
      ~on_output
      ~call_id
      ~cwd
      ~name
      ~arguments
      ()
;;

let host_exec t ~cancel ~on_output ~call_id ~cwd ~name ~arguments =
  active_host_exec t ~cancel ~on_output ~call_id ~cwd ~name ~arguments ()
;;

let resolve_dir_on t (host : Host.t) path =
  match
    host_exec_on
      t
      host
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"cd"
      ~cwd:host.cwd
      ~name:Host_ops.resolve_dir_op
      ~arguments:(`Object [ "path", `String path ])
  with
  | { is_error = true; text; _ } -> Or_error.errorf "%s: %s" host.name text
  | { is_error = false; text; _ } -> Ok text
;;

let make_dir_on t (host : Host.t) path ~check =
  let result =
    host_exec_on
      t
      host
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"cd"
      ~cwd:host.cwd
      ~name:Dir_change.op
      ~arguments:(Dir_change.args ~path ~check)
  in
  match Dir_change.older_host result ~host:host.name ~path, result with
  | Some message, _ -> Or_error.error_string message
  | None, { is_error = true; text; _ } ->
    Or_error.errorf "%s: %s" host.name text
  | None, { is_error = false; text; _ } -> Ok text
;;

(* Where [path] would take the session on [host], creating it or asking
   first as [if_missing] says. *)
let change_dir_on t host path ~(if_missing : Dir_change.If_missing.t)
  : Dir_change.Outcome.t Or_error.t
  =
  match resolve_dir_on t host path, if_missing with
  | Ok cwd, _ -> Ok (Changed { cwd; created = false })
  | (Error _ as error), Fail -> error
  | Error _, Ask ->
    Or_error.map (make_dir_on t host path ~check:true) ~f:(fun path ->
      Dir_change.Outcome.Missing path)
  | Error _, Create ->
    Or_error.map (make_dir_on t host path ~check:false) ~f:(fun cwd ->
      Dir_change.Outcome.Changed { cwd; created = true })
;;

let apply_dir_change (outcome : Dir_change.Outcome.t) ~f =
  (match outcome with
   | Changed { cwd; created = _ } -> f cwd
   | Missing _ -> ());
  outcome
;;

(* Where [cwd] on [host] is in git, asked at once ([None]: the host cannot
   tell). *)
let git_location_on t (host : Host.t) ~cwd =
  if String.equal host.id Host.backend_id
  then Option.some_if t.backend_host_enabled (Git_location.find ~cwd)
  else if host.git_location
  then
    Result.ok
      (Git_location.of_result
         (host_exec_on
            t
            host
            ~cancel:Cancellation.never
            ~on_output:ignore
            ~call_id:"git_location"
            ~cwd
            ~name:Git_location.op
            ~arguments:(`Object [])))
  else None
;;

let session_host (host : Host.t) ~pinned =
  { Session.Host.id = host.id; name = host.name; pinned }
;;

(* Where the session is now, before it moves. *)
let current_place t : Worktree_home.Place.t =
  let repo =
    match Session.repo t.session, find_host t t.active_host with
    | Some repo, _ when String.equal (Session.cwd t.session) t.cwd -> Some repo
    | _, Some host -> git_location_on t host ~cwd:t.cwd
    | _, None -> None
  in
  { cwd = t.cwd; host = Option.map (location t) ~f:fst; repo }
;;

let place_on t (host : Host.t) ~pinned ~cwd : Worktree_home.Place.t =
  { cwd
  ; host = Some (session_host host ~pinned)
  ; repo = git_location_on t host ~cwd
  }
;;

(* [move] changes the directory; the home follows ({!Worktree_home}). Where
   the session is in git is only asked when it matters: a worktree switch,
   or a move with a home. *)
let moving_home t ~worktree ~to_ ~move =
  let home = Session.home t.session in
  if (not worktree) && Option.is_none home
  then move ()
  else (
    let from = current_place t in
    move ();
    let to_ : Worktree_home.Place.t = to_ () in
    let change =
      if worktree
      then Worktree_home.switch home ~from ~to_
      else Worktree_home.cd home ~from ~to_
    in
    match change with
    | Keep -> ()
    | Set home ->
      ignore (Session.set_home t.session (Some home) : Session.Entry.t)
    | Clear -> ignore (Session.set_home t.session None : Session.Entry.t))
;;

(* The session cwd is a property of the host, so switching hosts means
   choosing a directory there; without [cwd] the host's own is used. *)
let set_active_host ?(if_missing = Dir_change.If_missing.Fail) t id ~cwd =
  match find_host t id with
  | None when String.equal id Host.backend_id && not t.backend_host_enabled ->
    Or_error.error_string "the backend tool host is disabled"
  | None when String.equal id t.active_host ->
    Or_error.error_string (host_unavailable_message t)
  | None -> Or_error.errorf "unknown tool host %S" id
  | Some host ->
    let outcome =
      match cwd with
      | None ->
        Ok (Dir_change.Outcome.Changed { cwd = host.cwd; created = false })
      | Some path -> change_dir_on t host (Tool.expand_home path) ~if_missing
    in
    Or_error.map
      outcome
      ~f:
        (apply_dir_change ~f:(fun cwd ->
           moving_home
             t
             ~worktree:false
             ~to_:(fun () -> place_on t host ~pinned:true ~cwd)
             ~move:(fun () -> activate_host t host ~cwd ~pinned:true)))
;;

let executor t : Tool.executor =
  fun context tool arguments ->
  let arguments =
    if Seen_files.applies_to (Tool.name tool)
    then Seen_files.with_scope arguments ~scope:(Session.id t.session)
    else arguments
  in
  let bash = String.equal (Tool.name tool) (Tool.name Tool_bash.tool) in
  if bash && Tool_bash.runs_in_backend context arguments
  then Tool.execute tool context arguments
  else (
    let run () =
      if
        (not tool.spec.on_host)
        || (String.equal t.active_host Host.backend_id && t.backend_host_enabled)
      then Tool.execute tool context arguments
      else
        active_host_exec
          t
          ?stdin:context.stdin
          ~cancel:context.cancel
          ~on_output:context.on_output
          ~call_id:context.call_id
          ~cwd:context.cwd
          ~name:(Tool.name tool)
          ~arguments
          ()
    in
    if bash
    then
      Worktree_watch.around context ~dir:t.config.worktrees_dir ~arguments run
    else run ())
;;

(* AGENTS.md/CLAUDE.md come from the active host's filesystem. *)
let instructions t ~cwd =
  Host_ops.instructions_of_result
    (host_exec
       t
       ~cancel:Cancellation.never
       ~on_output:ignore
       ~call_id:"instructions"
       ~cwd
       ~name:Host_ops.instructions_op
       ~arguments:(Host_ops.instructions_args ~home:t.home))
;;

(* Built once per conversation and recorded in the session, so the prompt
   prefix stays cacheable; later cwd/host changes reach the model as notes on
   the next user message instead. *)
let tools t = Tools.for_model t.model (t.tools @ t.mcp_tools)

let build_system_prompt t =
  let { Host_ops.Instructions.files; nix; skills } =
    instructions t ~cwd:t.cwd
  in
  System_prompt.build
    ~instructions:files
    ~nix
    ~skills
    ?auto_background:(Config.auto_background t.config)
    ~cwd:t.cwd
    ~home:t.home
    ~tools:(Tools.specs (tools t))
    ()
;;

let skills t =
  let result =
    host_exec
      t
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"skills"
      ~cwd:t.cwd
      ~name:Host_ops.instructions_op
      ~arguments:(Host_ops.instructions_args ~home:t.home)
  in
  if result.is_error
  then Error (Error.of_string result.text)
  else Ok (Host_ops.instructions_of_result result).skills
;;

(* [/skill:NAME ARGS] becomes the skill's file plus ARGS; other texts are
   sent as they are. *)
let expand_skill t text =
  match Skill.invocation text with
  | None -> Ok None
  | Some (name, args) ->
    Host_ops.skill_of_result
      (host_exec
         t
         ~cancel:Cancellation.never
         ~on_output:ignore
         ~call_id:"skill"
         ~cwd:t.cwd
         ~name:Host_ops.skill_op
         ~arguments:(Host_ops.skill_args ~home:t.home name))
    |> Or_error.map ~f:(fun (skill, body) ->
      Some (Skill.expand skill ~body ~args))
;;

let mcp_exec t ?(cancel = Cancellation.never) ~call_id name arguments =
  Host_ops.listing_of_result
    (host_exec t ~cancel ~on_output:ignore ~call_id ~cwd:t.cwd ~name ~arguments)
;;

let mcp_call t (context : Tool.Context.t) ~source ~server ~tool arguments =
  host_exec
    t
    ~cancel:context.cancel
    ~on_output:context.on_output
    ~call_id:context.call_id
    ~cwd:context.cwd
    ~name:Host_ops.mcp_call_op
    ~arguments:
      (Host_ops.mcp_call_args ~home:t.home ~source ~server ~tool arguments)
;;

let use_listing t (listing : Mcp_tools.Listing.t) =
  t.mcp_tools <- Mcp_tools.tools listing ~call:(mcp_call t);
  List.iter (Mcp_tools.Listing.notices listing) ~f:(fun notice ->
    if not (Set.mem t.mcp_noticed notice)
    then (
      t.mcp_noticed <- Set.add t.mcp_noticed notice;
      broadcast t (Notice notice)))
;;

(* The host's MCP servers as of now. A host that cannot tell (an older
   tool host, none connected) has none. *)
let refresh_mcp t ~cancel =
  if Option.is_some t.mcp
  then (
    match
      mcp_exec
        t
        ~cancel
        ~call_id:"mcp"
        Host_ops.mcp_servers_op
        (Host_ops.mcp_servers_args ~home:t.home ~reconnect:false)
    with
    | Ok listing -> use_listing t listing
    | Error _ -> t.mcp_tools <- [])
;;

let mcp_disabled = "MCP is off in this backend (it was started with -no-tools)"

let mcp_servers ?(reconnect = false) t =
  if Option.is_none t.mcp
  then Or_error.error_string mcp_disabled
  else
    Or_error.map
      (mcp_exec
         t
         ~call_id:"mcp"
         Host_ops.mcp_servers_op
         (Host_ops.mcp_servers_args ~home:t.home ~reconnect))
      ~f:(fun listing ->
        use_listing t listing;
        listing)
;;

let approve_mcp t ~source ~server =
  if Option.is_none t.mcp
  then Or_error.error_string mcp_disabled
  else
    Or_error.map
      (mcp_exec
         t
         ~call_id:"mcp"
         Host_ops.mcp_approve_op
         (Host_ops.mcp_approve_args ~home:t.home ~source ~server))
      ~f:(fun listing ->
        use_listing t listing;
        listing)
;;

let system_prompt t =
  match Session.system_prompt t.session with
  | Some text -> text
  | None ->
    let text = build_system_prompt t in
    ignore (Session.set_system_prompt t.session ~text : Session.Entry.t);
    tell t { (environment t) with plan_mode = t.told.plan_mode };
    text
;;

(* ---- goal ------------------------------------------------------------- *)

let set_goal_state t goal =
  t.goal <- goal;
  ignore (Session.set_goal t.session goal : Session.Entry.t)
;;

(* Goal messages are not user messages, but carry the plan mode note too. *)
let goal_message t ~source text =
  let notes = Option.to_list (plan_mode_note t) in
  Message.user
    ~origin:{ Injected.empty with notes; source = Some source }
    (Injected.prefix_notes notes text)
;;

let reminder_message t (reminder : Reminder.t) ~repeat =
  goal_message
    t
    ~source:
      (Reminder
         { name = Reminder.label reminder
         ; repeat
         ; max =
             (match reminder.trigger with
              | On_stop { max_repeats } -> Some max_repeats
              | Every_turns _ -> None)
         })
    (Reminder.message_text reminder ~repeat)
;;

let due_reminders t =
  List.map
    (Reminder.Tracker.due t.reminder_tracker t.reminders)
    ~f:(reminder_message t ~repeat:None)
;;

let stop_goal t ~reason =
  match t.goal with
  | Some goal when Goal.is_active goal ->
    set_goal_state t (Some { goal with status = Stopped { reason } });
    broadcast
      t
      (Notice (sprintf "goal stopped: %s; /goal resume continues it" reason))
  | Some _ | None -> ()
;;

let complete_goal t ~summary =
  match t.goal with
  | Some goal when Goal.is_active goal ->
    set_goal_state t (Some { goal with status = Done { summary } });
    broadcast t (Notice ("goal complete: " ^ summary));
    state_changed t;
    Ok ()
  | Some _ | None ->
    Or_error.error_string "no goal is active, so there is nothing to complete"
;;

(* ---- review --------------------------------------------------------- *)

let update_review t ~f =
  Or_error.map (f t.review) ~f:(fun (result, changes) ->
    List.iter changes ~f:(fun change ->
      ignore (Session.record_review t.session change : Session.Entry.t);
      t.review <- Review_state.apply t.review change);
    if not (List.is_empty changes) then broadcast t (Review_changed t.review);
    result)
;;

let now t = Eio.Time.now (Eio.Stdenv.clock t.env)

let read_review_file t ~root ~path =
  let result =
    host_exec
      t
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"review"
      ~cwd:root
      ~name:Host_files.read_op
      ~arguments:
        (`Object
            [ "path", `String (Filename.concat root path)
            ; "offset", `Number "0"
            ; "length", `Number (Int.to_string Review_snapshot.max_bytes)
            ])
  in
  if result.is_error
  then
    if String.is_suffix (String.strip result.text) ~suffix:"does not exist"
    then Ok (Review_snapshot.content_of None ~size:0)
    else Or_error.error_string (String.strip result.text)
  else
    let open Or_error.Let_syntax in
    let%bind json = Json.parse result.text in
    let%bind data =
      match Json.member "data" json with
      | Some (`String data) ->
        Result.map_error (Base64.decode data) ~f:(fun (`Msg m) ->
          Error.of_string ("bad data from the host: " ^ m))
      | _ -> Or_error.error_string "bad reply from the host"
    in
    let size =
      match Json.member "size" json with
      | Some (`Number n) ->
        Option.value (Int.of_string_opt n) ~default:(String.length data)
      | _ -> String.length data
    in
    Ok (Review_snapshot.content_of (Some data) ~size)
;;

let update_review_file ?by ?text t ~root ~path ~action ~f =
  Or_error.map (f t.review) ~f:(fun (result, changes) ->
    let after = List.fold changes ~init:t.review ~f:Review_state.apply in
    let current =
      match text with
      | Some text ->
        Ok (Review_snapshot.content_of (Some text) ~size:(String.length text))
      | None -> read_review_file t ~root ~path
    in
    let snapshot =
      match current with
      | Error _ -> []
      | Ok (content, text) ->
        Review_state.snapshot
          after
          { root; path; action; by; at = now t; content }
          ~text
    in
    List.iter (changes @ snapshot) ~f:(fun change ->
      ignore (Session.record_review t.session change : Session.Entry.t);
      t.review <- Review_state.apply t.review change);
    if not (List.is_empty changes && List.is_empty snapshot)
    then broadcast t (Review_changed t.review);
    result)
;;

let reply_tools t =
  if List.is_empty t.review.comments
  then []
  else
    [ Tool_review_reply.create ~reply:(fun ~id ~text ~edits ~resolve ->
        update_review t ~f:(fun review ->
          Review_state.reply
            review
            ~id
            ~author:Agent
            ~text
            ~edits
            ~resolve
            ~at:(now t)
            ()))
    ]
;;

(* Offered only while a goal is active, and never to subagents. *)
let goal_tools t =
  match t.goal with
  | Some goal when Goal.is_active goal ->
    [ Tool_goal.create ~complete:(complete_goal t) ]
  | Some _ | None -> []
;;

let loop_config t =
  { Agent_loop.Config.model = t.model
  ; thinking = t.thinking
  ; system = Some (system_prompt t)
  ; tools = tools t @ goal_tools t @ reply_tools t
  ; max_turns = None
  ; max_tokens = None
  ; retries = Agent_loop.Config.default_retries
  ; cache_key = Some (Session.id t.session)
  }
;;

(* The text with the attachments' [<file>] blocks, what they are, and their
   images. *)
let with_attachments t text attachments =
  match attachments with
  | [] -> text, [], []
  | _ ->
    let block path =
      let result =
        host_exec
          t
          ~cancel:Cancellation.never
          ~on_output:ignore
          ~call_id:"attachment"
          ~cwd:t.cwd
          ~name:Host_ops.read_file_op
          ~arguments:
            (Seen_files.with_scope
               (`Object [ "path", `String path ])
               ~scope:(Session.id t.session))
      in
      match result with
      | { is_error = true; text; images = _ } ->
        sprintf "<file path=%S error=%S/>" path text, []
      | { is_error = false; text = content; images } ->
        let content =
          if String.is_suffix content ~suffix:"\n"
          then content
          else content ^ "\n"
        in
        sprintf "<file path=%S>\n%s</file>" path content, images
    in
    let blocks, images = List.unzip (List.map attachments ~f:block) in
    ( text ^ "\n\n" ^ String.concat blocks ~sep:"\n\n"
    , List.map2_exn attachments blocks ~f:(fun path block ->
        { Injected.Attachment.path; length = String.length block })
    , List.concat images )
;;

(* Without a system prompt yet, it is to be built from the current
   environment: only plan mode, which it does not describe, is told. *)
let take_notes t =
  let now = environment t in
  let told =
    if Option.is_some (Session.system_prompt t.session)
    then t.told
    else { now with plan_mode = t.told.plan_mode }
  in
  let notes = Told_environment.notes ~told now ~host_name:t.active_host_name in
  if not (List.is_empty notes) then tell t now;
  notes
;;

let user_message t (q : Queued.t) =
  Reminder.Tracker.user_spoke t.reminder_tracker;
  let text = Option.value q.expanded ~default:q.text in
  let text, attachments, attached = with_attachments t text q.attachments in
  let notes = take_notes t in
  let origin = { Injected.notes; attachments; source = q.source } in
  Message.user
    ~images:(q.images @ attached)
    ?origin:(Option.some_if (not (Injected.is_empty origin)) origin)
    (Injected.prefix_notes notes text)
;;

(* Spend the session's messages do not record, recorded as [Spend] entries:
   subagents' responses (as they arrive, so a budget sees a running
   subagent's spend) and compactions. *)
let rec account t ?(nested = false) (event : Agent_event.t) =
  let add usage cost_usd =
    ignore (Session.add_spend t.session ~usage ~cost_usd : Session.Entry.t)
  in
  match event with
  | Subagent { event; _ } -> account t ~nested:true event
  | Message_end (Assistant a) when nested ->
    add a.usage (Model.cost_usd (model_of t a) a.usage)
  | Compacted { usage; cost_usd; _ } -> add usage cost_usd
  | _ -> ()
;;

(* A subagent whose top-level ancestor is gone (a background subagent's
   task, or the main run for its synchronous ones) can never end on its
   own: it is ended here, through the normal event path. *)
let sweep_orphans t ~main_running =
  let background root =
    Result.ok (Background_tasks.find t.background ~kind:Subagent root)
  in
  let alive root =
    match background root with
    | Some task -> Background_tasks.Task.running task
    | None -> main_running
  in
  let why root =
    match background root with
    | Some _ -> sprintf "background subagent %s ended" root
    | None -> "the run ended"
  in
  List.iter
    (Subagent_log.orphan_ends t.subagent_log ~alive ~why)
    ~f:(fun event -> broadcast t (Loop event))
;;

(* The log follows the session, saved in its data directory once the
   session file exists. *)
let load_subagent_log t =
  Subagent_log.load t.subagent_log ~dir:(fun () ->
    if Session.persisted t.session
    then Some (Session.data_dir (Session.path t.session))
    else None);
  Background_tasks.avoid_subagent_ids
    t.background
    (List.map (Subagent_log.summaries t.subagent_log) ~f:(fun s -> s.id))
;;

let deliveries t =
  match Background_tasks.take_undelivered t.background with
  | [] -> []
  | tasks -> [ Background_tasks.delivery_message tasks ]
;;

(* The system prompt names the tools of the model it was built for. *)
let set_model t model =
  t.model <- model;
  environment_changed t;
  ignore
    (Session.set_model t.session ~model:(Model.key model) ~thinking:t.thinking
     : Session.Entry.t);
  state_changed t
;;

(* The model to continue with when [model] cannot be used: the next one in
   [fallback_models] after it (or the first, if it is not listed) that this
   hand-over chain has not tried yet. *)
let next_model t ~tried =
  let chain = t.config.fallback_models in
  let after =
    match
      List.findi chain ~f:(fun _ key ->
        match Model_registry.find t.models key with
        | Some m -> String.equal (Model.key m) (Model.key t.model)
        | None -> false)
    with
    | Some (i, _) -> List.drop chain (i + 1)
    | None -> chain
  in
  List.find_map after ~f:(fun key ->
    match Model_registry.find t.models key with
    | Some m when not (Set.mem tried (Model.key m)) -> Some m
    | Some _ | None -> None)
;;

let unavailable_error added =
  List.find_map (List.rev added) ~f:(function
    | Message.Assistant { stop_reason = Error message; _ }
      when Usage_limit.unavailable message -> Some message
    | Message.Assistant _ -> Some ""
    | User _ | Tool_result _ -> None)
  |> Option.filter ~f:(Fn.non String.is_empty)
;;

let handover_message ~from ~(to_ : Model.t) ~error =
  Message.user
    ~origin:
      { Injected.empty with
        source = Some (Handover { from; to_ = Model.key to_; error })
      }
    (sprintf
       "[prigh: %s cannot continue (%s), so %s takes over this conversation \
        from here. Carry on with the task where it left off.]"
       from
       error
       (Model.key to_))
;;

let compacted t (outcome : Compaction.Outcome.t) =
  account
    t
    (Compacted
       { summary = outcome.summary
       ; usage = outcome.usage
       ; cost_usd = Model.cost_usd t.model outcome.usage
       });
  broadcast t (Compacted { summary = outcome.summary })
;;

let last_stop added =
  List.find_map (List.rev added) ~f:(function
    | Message.Assistant a -> Some a.stop_reason
    | User _ | Tool_result _ -> None)
;;

let resume_message =
  Message.user
    ~origin:{ Injected.empty with source = Some Resume }
    "[prigh: the usage limit has reset. Carry on with the interrupted task \
     where it left off.]"
;;

let cancel_resume_timer t =
  Option.iter t.resume ~f:(fun r ->
    t.resume <- None;
    Session.set_resume_at t.session None;
    Cancellation.cancel r.cancel)
;;

let duration_hum seconds =
  let minutes = Float.iround_up_exn (Float.max 0. seconds /. 60.) in
  if minutes < 60
  then sprintf "%dm" minutes
  else sprintf "%dh %02dm" (minutes / 60) (minutes mod 60)
;;

(* When the agent ended its turn with nothing left to wait for. *)
let stop_reminders t =
  let { Reminder.Tracker.Stop.fire; used_up } =
    Reminder.Tracker.stopped t.reminder_tracker t.reminders
  in
  List.iter used_up ~f:(fun r ->
    match r.trigger with
    | On_stop { max_repeats } ->
      broadcast
        t
        (Notice
           (sprintf
              "reminder %s fired %s; it fires again after your next message"
              r.name
              (if max_repeats = 1
               then "once"
               else sprintf "%d times in a row" max_repeats)))
    | Every_turns _ -> ());
  List.map fire ~f:(fun (r, repeat) ->
    reminder_message t r ~repeat:(Some repeat))
;;

(* Calls left without results by a backend that stopped while they ran get
   them now, before anything else is added after them. *)
let close_interrupted t =
  List.iter
    (Tool_pairing.unanswered (Session.messages t.session))
    ~f:(fun call ->
      let m = Message.Tool_result (Tool_pairing.interrupted call) in
      ignore (Session.append_message t.session m : Session.Entry.t);
      broadcast t (Loop (Message_start m));
      broadcast t (Loop (Message_end m)))
;;

(* A bug in a run's bookkeeping fails the run, not the backend (the agent's
   switch is the server's), and does not leave the session running forever. *)
let run_failed t ~resolve exn =
  eprintf
    "prigh: a run of session %s failed: %s\n%!"
    (Session.id t.session)
    (Exn.to_string exn);
  if Promise.try_resolve resolve ()
  then (
    t.run <- None;
    broadcast t (Notice ("run failed: " ^ Exn.to_string exn));
    state_changed t)
;;

(* Finished background tasks not delivered yet go first. [tried] are the
   models a hand-over chain has used so far (empty for a user's prompt). *)
let rec start_run ?(tried = String.Set.empty) ?(then_ = fun () -> []) t prompts =
  refresh_location t;
  cancel_resume_timer t;
  close_interrupted t;
  let cancel = Cancellation.create () in
  let finished, resolve = Promise.create () in
  t.run <- Some { cancel; finished };
  let prompts = deliveries t @ prompts in
  state_changed t;
  Guarded.fork
    ~sw:t.sw
    ~what:"a run"
    ~on_error:(run_failed t ~resolve)
    (fun () ->
       (* [then_] may wait for the host (attachments): the run is already
       marked as started, so no other can start meanwhile. *)
       let prompts = prompts @ then_ () in
       let prompts = prompts @ due_reminders t in
       let session = t.session in
       let finish ~idle =
         t.run <- None;
         Promise.resolve resolve ();
         if idle then state_changed t
       in
       refresh_mcp t ~cancel;
       let halted = ref None in
       let added =
         match
           Agent_loop.run
             ~env:t.env
             ~provider:t.provider
             ~config:(loop_config t)
             ~cwd:t.cwd
             ~cancel
             ~approve:(approve t)
             ~ask_user:(ask_user t ~cancel)
             ~execute:(executor t)
             ~background:t.background
             ~steer:(fun () ->
               let l = Steer_queue.take_steers t.queue in
               if not (List.is_empty l) then queue_update t;
               let queued = List.map l ~f:(user_message t) in
               (* A plan mode switched during the run is told between turns. *)
               let note =
                 if List.is_empty queued
                 then
                   Option.to_list
                     (Option.map (plan_mode_note t) ~f:(fun note ->
                        Message.user
                          ~origin:{ Injected.empty with notes = [ note ] }
                          note.text))
                 else []
               in
               deliveries t @ note @ queued @ due_reminders t)
             ~compact:(fun ~keep_recent_tokens ~cancel _messages ->
               match
                 Compaction.compact
                   ~env:t.env
                   ~provider:t.provider
                   ~model:t.model
                   ~keep_recent_tokens
                   ~cancel
                   session
               with
               | Ok outcome -> Ok (Session.messages session, outcome)
               | Error e ->
                 broadcast
                   t
                   (Notice ("auto-compaction failed: " ^ Error.to_string_hum e));
                 Error e)
             ~halt:(fun () ->
               let reason = budget_exhausted t in
               Option.iter reason ~f:(fun r -> halted := Some r);
               reason)
             ~emit:(fun event ->
               account t event;
               track_plan t event;
               match event with
               | Turn_end _ ->
                 if phys_equal session t.session
                 then Reminder.Tracker.turn_ended t.reminder_tracker;
                 broadcast t (Loop event)
               | Compacted { summary; _ } ->
                 if phys_equal session t.session
                 then (
                   broadcast t (Compacted { summary });
                   state_changed t)
               | Message_end m ->
                 ignore (Session.append_message session m : Session.Entry.t);
                 broadcast t (Loop event)
               | _ -> broadcast t (Loop event))
             ~context:(Session.messages session)
             ~prompts
             ()
         with
         | added -> added
         | exception exn ->
           broadcast t (Notice ("run failed: " ^ Exn.to_string exn));
           []
       in
       Option.iter !halted ~f:(fun reason -> broadcast t (Notice reason));
       sweep_orphans t ~main_running:false;
       let tried = Set.add tried (Model.key t.model) in
       let cancelled = Cancellation.is_cancelled cancel in
       let current = phys_equal session t.session in
       let unavailable =
         if cancelled || not current then None else unavailable_error added
       in
       let handover =
         Option.bind unavailable ~f:(fun error ->
           match next_model t ~tried with
           | None ->
             if not (List.is_empty t.config.fallback_models)
             then
               broadcast
                 t
                 (Notice
                    (sprintf
                       "%s cannot continue and no model in fallback_models is \
                        left to hand over to; /model picks another"
                       (Model.key t.model)));
             None
           | Some next ->
             let from = Model.key t.model in
             broadcast
               t
               (Notice
                  (sprintf
                     "%s: %s; handing over to %s"
                     from
                     error
                     (Model.key next)));
             set_model t next;
             Some [ handover_message ~from ~to_:next ~error ])
       in
       let resuming =
         match unavailable, handover with
         | Some error, None -> schedule_resume t ~error
         | None, _ ->
           t.past_resets <- 0;
           false
         | Some _, Some _ -> false
       in
       (* Steering messages that arrived after the last turn boundary go
       first: they were sent before any follow-up still queued. *)
       if Steer_queue.steers_first t.queue then queue_update t;
       auto_compact t;
       (* The goal carries on after a run that ended normally; anything else
       stops it, except a hand-over or a resume, which carry on the run. *)
       let goal_continues =
         match t.goal with
         | Some goal when current && Goal.is_active goal ->
           let max = t.config.goal_max_continuations in
           if cancelled
           then (
             stop_goal t ~reason:"the run was aborted";
             false)
           else if Option.is_some handover || resuming
           then false
           else if t.plan_mode
           then
             (* The model cannot work towards the goal in plan mode: it waits
             for the user to approve the plan, then carries on. *)
             false
           else (
             match budget_exhausted t, last_stop added with
             | Some _, _ ->
               stop_goal t ~reason:"the session's budget is spent";
               false
             | None, Some (Error e) ->
               stop_goal t ~reason:("the run failed: " ^ e);
               false
             | None, Some Aborted ->
               stop_goal t ~reason:"the run was aborted";
               false
             | None, (Some (End_turn | Tool_use | Length) | None) ->
               if goal.continuations >= max
               then (
                 stop_goal
                   t
                   ~reason:
                     (sprintf "it used all %d automatic continuations" max);
                 false)
               else true)
         | Some _ | None -> false
       in
       (* After an abort, finished tasks wait for the next prompt (or the next
       one to finish) rather than restarting the run at once. *)
       let next =
         match handover with
         | Some prompts -> Some (`Handover prompts)
         | None ->
           (match Steer_queue.take_follow_up t.queue with
            | Some queued ->
              queue_update t;
              Some (`Prompts [ user_message t queued ])
            | None ->
              if
                Background_tasks.has_undelivered t.background
                && not (Cancellation.is_cancelled cancel)
              then Some (`Prompts [])
              else if goal_continues
              then (
                match t.goal with
                | Some goal ->
                  let goal =
                    { goal with continuations = goal.continuations + 1 }
                  in
                  set_goal_state t (Some goal);
                  Some
                    (`Prompts
                        [ goal_message
                            t
                            ~source:
                              (Goal_continuation
                                 { continuation = goal.continuations
                                 ; max = t.config.goal_max_continuations
                                 })
                            (Goal.continuation_message
                               goal
                               ~max_continuations:
                                 t.config.goal_max_continuations)
                        ])
                | None -> None)
              else if
                current
                && (not cancelled)
                && Option.is_none handover
                && (not resuming)
                && (not t.plan_mode)
                && Option.is_none (budget_exhausted t)
                && (not (Background_tasks.has_running t.background))
                &&
                match last_stop added with
                | Some (End_turn | Tool_use | Length) -> true
                | Some (Error _ | Aborted) | None -> false
              then (
                match stop_reminders t with
                | [] -> None
                | prompts -> Some (`Prompts prompts))
              else None)
       in
       (* A hand-over, a queued message, a goal's continuation or a reminder
       carries on: the run's next state says [running] again, with no idle
       moment in between (in which the server would evict a session without
       clients). *)
       finish ~idle:(Option.is_none next);
       Option.iter next ~f:(function
         | `Handover prompts -> start_run ~tried t prompts
         | `Prompts prompts -> start_run t prompts);
       auto_describe t)

(* A usage limit with no fallback left: resume when it resets, if the
   provider said when. *)
and schedule_resume t ~error =
  if
    (not t.config.resume_on_limit_reset)
    || Usage_limit.credentials_missing error
  then false
  else (
    match Usage_limit.resets_at error with
    | None ->
      broadcast
        t
        (Notice
           (sprintf
              "%s: the usage limit is reached and the provider did not say \
               when it resets, so the run will not resume by itself; send a \
               message to try again, or /model picks another model"
              (Model.key t.model)));
      false
    | Some reset ->
      let now = Eio.Time.now (Eio.Stdenv.clock t.env) in
      (* A reset time already past (a stale header, a skewed clock) would
         retry at once, again and again: back off instead, from a minute
         up to an hour. *)
      let at, when_ =
        if Float.( > ) reset now
        then (
          t.past_resets <- 0;
          ( reset
          , sprintf
              "when it resets at %s (in %s)"
              (Usage_limit.utc_string reset)
              (duration_hum (reset -. now)) ))
        else (
          let backoff =
            Float.min 3600. (60. *. (2. **. Float.of_int t.past_resets))
          in
          t.past_resets <- t.past_resets + 1;
          ( now +. backoff
          , sprintf
              "in %s (the provider says it reset at %s, which has passed)"
              (duration_hum backoff)
              (Usage_limit.utc_string reset) ))
      in
      arm_resume t ~at;
      broadcast
        t
        (Notice
           (sprintf
              "%s: the usage limit is reached; the run resumes %s: /resume now \
               retries at once, /resume cancel stays stopped"
              (Model.key t.model)
              when_));
      true)

(* Recorded in the session, so that it is armed again when the session is
   loaded (see [restore_resume]). *)
and arm_resume t ~at =
  cancel_resume_timer t;
  let cancel = Cancellation.create () in
  let resume = { Resume.at; cancel } in
  t.resume <- Some resume;
  Session.set_resume_at t.session (Some at);
  Guarded.fork ~sw:t.sw ~what:"resuming after a usage limit" (fun () ->
    (* A little after the reset, so the provider agrees it has passed. *)
    match
      Cancellation.protect cancel ~f:(fun () -> t.sleep_until (at +. 5.))
    with
    | None -> ()
    | Some () ->
      if Option.exists t.resume ~f:(fun r -> phys_equal r resume)
      then (
        t.resume <- None;
        if Session.gone t.session
        then (
          broadcast
            t
            (Notice
               "the usage limit has reset, but this session was deleted, so \
                its run does not resume");
          state_changed t)
        else (
          Session.set_resume_at t.session None;
          if is_running t
          then state_changed t
          else start_run t [ resume_message ])))

(* Runs after the turn is over so the user is not kept waiting; [wait_idle]
   still covers it. *)
and auto_describe t =
  let session = t.session in
  if
    t.auto_describe
    && Option.is_none t.describing
    && Session_description.wanted session
  then (
    let finished, resolve = Promise.create () in
    t.describing <- Some finished;
    Fiber.fork ~sw:t.sw (fun () ->
      Guarded.run ~what:"describing the session" (fun () ->
        match
          Session_description.describe
            ~provider:t.provider
            ~model:t.model
            session
        with
        | Ok _ -> if phys_equal session t.session then state_changed t
        | Error e ->
          broadcast
            t
            (Notice ("session description failed: " ^ Error.to_string_hum e)));
      t.describing <- None;
      Promise.resolve resolve ()))

and auto_compact t =
  let state = state t in
  if Compaction.should_compact t.model ~input_tokens:state.context_tokens
  then (
    match
      Compaction.compact
        ~env:t.env
        ~provider:t.provider
        ~model:t.model
        t.session
    with
    | Ok outcome -> compacted t outcome
    | Error e ->
      broadcast t (Notice ("auto-compaction failed: " ^ Error.to_string_hum e)))
;;

(* Messages a subagent stopped before taking are not lost silently. *)
let subagent_queue_changed t =
  broadcast t (Subagent_queues (Background_tasks.queues t.background));
  List.iter (Background_tasks.tasks t.background ~kind:Subagent) ~f:(fun task ->
    Option.iter (Background_tasks.Task.inbox task) ~f:(fun inbox ->
      let dropped = Subagent_inbox.take_dropped inbox in
      (* A discarded one belongs to a session no longer shown. *)
      if not (Background_tasks.Task.delivered task)
      then
        List.iter dropped ~f:(fun text ->
          broadcast
            t
            (Notice
               (sprintf
                  "%s stopped before it got your message %S, which was not \
                   delivered: tell the main agent instead"
                  (Background_tasks.Task.id task)
                  (Background_tasks.short_task text))))))
;;

(* A background task finished (or was delivered): an idle agent starts a
   turn to receive it; a running one picks it up at its next turn boundary. *)
let background_changed t =
  sweep_orphans t ~main_running:(is_running t);
  if (not (is_running t)) && Background_tasks.has_undelivered t.background
  then start_run t [];
  state_changed t
;;

(* Seeds subagent ids so they do not repeat ids the model has already seen
   in this conversation. *)
let subagent_calls messages =
  List.sum
    (module Int)
    messages
    ~f:(function
      | Message.Assistant a ->
        List.count (Message.Assistant.tool_calls a) ~f:(fun call ->
          String.equal call.name "subagent")
      | User _ | Tool_result _ -> 0)
;;

(* Jobs appear as [started job j<n>: ...] results, results of calls moved to
   the background and [[job j<n> ...]] report lines. *)
let last_job_number messages =
  let number_after s ~prefix =
    Option.bind (String.chop_prefix s ~prefix) ~f:(fun rest ->
      Int.of_string_opt (String.take_while rest ~f:Char.is_digit))
  in
  List.fold messages ~init:0 ~f:(fun acc message ->
    let numbers =
      match (message : Message.t) with
      | Tool_result r when String.equal r.tool_name "bash" ->
        Option.to_list
          (Option.first_some
             (number_after r.text ~prefix:"started job j")
             (Option.bind (Tool_bash.moved_job_id r.text) ~f:(fun id ->
                number_after id ~prefix:"j")))
      | User { text; _ } ->
        List.filter_map
          (String.split_lines text)
          ~f:(number_after ~prefix:"[job j")
      | Tool_result _ | Assistant _ -> []
    in
    List.fold numbers ~init:acc ~f:Int.max)
;;

(* A resume the session recorded (before a backend restart, or eviction)
   is armed again while its time is still to come. *)
let restore_resume t =
  match Session.resume_at t.session with
  | None -> ()
  | Some at ->
    if Float.( > ) at (Eio.Time.now (Eio.Stdenv.clock t.env))
    then arm_resume t ~at
    else Session.set_resume_at t.session None
;;

let create
      ~env
      ~sw
      ~provider
      ~tools
      ~sessions_dir
      ~home
      ?session
      ?model
      ?thinking
      ?(fallback_model = Model.default)
      ?(models = Model_registry.builtin ())
      ?(auto_describe = false)
      ?(backend_host = true)
      ?mcp
      ?(use_default_cwd = true)
      ?max_budget_usd
      ?sleep_until
      ~cwd
      ()
  =
  let sleep_until =
    match sleep_until with
    | Some f -> f
    | None -> Eio.Time.sleep_until (Eio.Stdenv.clock env)
  in
  let config =
    match Config.load ~home with
    | Ok config -> config
    | Error _ -> Config.default
  in
  let model =
    match model with
    | Some model -> model
    | None ->
      Option.bind (Config.start_model config) ~f:(Model_registry.find models)
      |> Option.value ~default:fallback_model
  in
  let thinking =
    Option.first_some thinking config.default_thinking
    |> Option.value ~default:Thinking.default
  in
  let session =
    match session with
    | Some s -> s
    | None ->
      let cwd =
        match Option.map config.default_cwd ~f:Tool.expand_home with
        | Some dir
          when use_default_cwd
               && ((not backend_host) || Sys_unix.is_directory_exn dir) -> dir
        | Some _ | None -> cwd
      in
      Session.create ~dir:sessions_dir ~cwd ()
  in
  let cwd = Session.cwd session in
  let t =
    { env
    ; sw
    ; provider
    ; tools
    ; sessions_dir
    ; home
    ; models
    ; backend_host_enabled = backend_host
    ; session
    ; cwd
    ; git_branch =
        (if backend_host
         then Option.bind (Git_location.find ~cwd) ~f:(fun l -> l.branch)
         else None)
    ; repo_asked = None
    ; model
    ; thinking
    ; run = None
    ; describing = None
    ; auto_describe
    ; queue = Steer_queue.create ()
    ; subscribers = []
    ; config
    ; pending_confirms = String.Table.create ()
    ; pending_questions = String.Table.create ()
    ; approval_mode = config.approval_mode
    ; plan_mode = false
    ; session_rules = []
    ; plan = None
    ; shell_seq = 0
    ; hosts = []
    ; host_cwds = String.Table.create ()
    ; backend_cwd = cwd
    ; active_host = (if backend_host then Host.backend_id else "")
    ; active_host_name = ""
    ; host_pinned = false
    ; pending_execs = String.Table.create ()
    ; exec_seq = 0
    ; told = { host = ""; cwd; tools = []; plan_mode = false }
    ; background =
        Background_tasks.create
          ~env
          ~sw
          ~first_subagent_id:(subagent_calls (Session.messages session) + 1)
          ~first_job_id:(last_job_number (Session.messages session) + 1)
          ()
    ; subagent_log = Subagent_log.create ()
    ; mcp
    ; mcp_tools = []
    ; mcp_noticed = String.Set.empty
    ; max_budget_usd
    ; goal = None
    ; review = Review_state.empty
    ; reminders = []
    ; reminder_tracker = Reminder.Tracker.create ()
    ; resume = None
    ; past_resets = 0
    ; sleep_until
    }
  in
  restore_settings t;
  restore_host t;
  restore_told t;
  load_subagent_log t;
  restore_resume t;
  Background_tasks.connect
    t.background
    ~auto_background:(fun () -> Config.auto_background t.config)
    ~emit:(fun event ->
      account t event;
      broadcast t (Loop event))
    ~on_change:(fun () -> background_changed t)
    ~on_queue_change:(fun () -> subagent_queue_changed t);
  t
;;

let prompt ?(attachments = []) ?(images = []) t text =
  let running () =
    Or_error.error_string "a run is already in progress; use steer or follow_up"
  in
  if is_running t
  then running ()
  else if Option.is_some (budget_exhausted t)
  then Or_error.error_string (Option.value_exn (budget_exhausted t))
  else
    Or_error.bind (expand_skill t text) ~f:(fun skill ->
      if is_running t
      then running ()
      else
        Ok
          (start_run t [] ~then_:(fun () ->
             [ user_message
                 t
                 { text; expanded = skill; source = None; attachments; images }
             ])))
;;

let enqueue_queued t kind queued =
  if is_running t
  then (
    Steer_queue.push t.queue kind queued;
    queue_update t)
  else start_run t [] ~then_:(fun () -> [ user_message t queued ])
;;

let enqueue t kind ?(attachments = []) ?(images = []) text =
  match budget_exhausted t with
  | Some reason -> Or_error.error_string reason
  | None ->
    Or_error.map (expand_skill t text) ~f:(fun skill ->
      enqueue_queued
        t
        kind
        { Queued.text; expanded = skill; source = None; attachments; images })
;;

let steer ?attachments ?images t text =
  enqueue t Steer ?attachments ?images text
;;

let follow_up ?attachments ?images t text =
  enqueue t Follow_up ?attachments ?images text
;;

(* Queued text is restored verbatim: attachments are only inlined when the
   message is actually sent. *)
let abort t =
  let restored =
    List.map (Steer_queue.take_all t.queue) ~f:(fun (q : Queued.t) -> q.text)
  in
  queue_update t;
  Option.iter t.run ~f:(fun run -> Cancellation.cancel run.cancel);
  if Option.is_some t.resume
  then (
    cancel_resume_timer t;
    broadcast t (Notice "the automatic resume is cancelled");
    state_changed t);
  restored
;;

let rec wait_idle t =
  match t.run, t.describing with
  | Some run, _ ->
    Promise.await run.finished;
    wait_idle t
  | None, Some describing ->
    Promise.await describing;
    wait_idle t
  | None, None ->
    if Background_tasks.has_running t.background
    then (
      Background_tasks.wait_all t.background;
      wait_idle t)
;;

let has_running_subagents t =
  Background_tasks.has_running ~kind:Subagent t.background
;;

let has_running_background t = Background_tasks.has_running t.background
let running_background t = Background_tasks.running_count t.background

let steer_subagent t ~agent_id ~kind text =
  Background_tasks.send t.background ~id:agent_id kind ~from:User text
;;

let dequeue_subagent t ~agent_id =
  Or_error.map
    (Background_tasks.inbox_of t.background agent_id)
    ~f:Subagent_inbox.pop_last
;;

let remove_subagent_queued t ~agent_id ~queue ~index ~text =
  Or_error.bind
    (Background_tasks.inbox_of t.background agent_id)
    ~f:(fun inbox -> Subagent_inbox.remove inbox queue ~index ~text)
;;

let subagent_queues t = Background_tasks.queues t.background

let cancel_subagent t ~agent_id =
  Background_tasks.cancel t.background ~kind:Subagent agent_id
;;

let cancel_background ?discard t =
  Background_tasks.cancel_all ?discard t.background
;;

let jobs t = Background_tasks.tasks t.background ~kind:Job
let kill_job t ~job_id = Background_tasks.cancel t.background ~kind:Job job_id

let job_output t ~job_id ~lines =
  Or_error.map
    (Background_tasks.find t.background ~kind:Job job_id)
    ~f:(fun task ->
      Tool_jobs.output_text
        task
        ~now:(Eio.Time.now (Eio.Stdenv.clock t.env))
        ~lines
        ~offset:0)
;;

let start_job t ~command =
  Tool_bash.start_job
    t.background
    ~command
    ~run:(fun ~id ~cancel ~stdin ~on_output ->
      active_host_exec
        t
        ~stdin
        ~cancel
        ~on_output
        ~call_id:id
        ~cwd:t.cwd
        ~name:"bash"
        ~arguments:
          (Tool_bash.foreground_arguments
             (`Object [ "command", `String command ]))
        ())
;;

let subagents t = Subagent_log.summaries t.subagent_log
let subagent t key = Subagent_log.find t.subagent_log key

let pending_confirms t =
  Hashtbl.data t.pending_confirms
  |> List.map ~f:(fun (w : Waiting_confirm.t) -> w.pending)
  |> List.sort ~compare:(fun (a : Pending_confirm.t) b ->
    String.compare a.call_id b.call_id)
;;

let dequeue t =
  let popped = Steer_queue.pop_last t.queue in
  Option.iter popped ~f:(fun _ -> queue_update t);
  popped
;;

let remove_queued t ~queue ~index ~text =
  Steer_queue.remove t.queue queue ~index ~matches:(fun (item : Queued.t) ->
    Option.for_all text ~f:(String.equal item.text))
  |> Or_error.map ~f:(fun item ->
    queue_update t;
    item)
;;

let shell t ~command ~add_to_context =
  if is_running t
  then
    Or_error.error_string
      "cannot run a shell command while a run is in progress"
  else (
    let call_id = sprintf "shell-%d" t.shell_seq in
    t.shell_seq <- t.shell_seq + 1;
    let call : Content.Tool_call.t =
      { id = call_id
      ; name = "shell"
      ; arguments = Json.to_string (`Object [ "command", `String command ])
      }
    in
    let output = Buffer.create 1024 in
    broadcast t (Loop (Tool_start call));
    let result =
      host_exec
        t
        ~cancel:Cancellation.never
        ~on_output:(fun chunk ->
          Buffer.add_string output chunk;
          broadcast t (Loop (Tool_output { call_id; chunk })))
        ~call_id
        ~cwd:t.cwd
        ~name:"bash"
        ~arguments:
          (`Object [ "command", `String command; "timeout", `Number "120" ])
    in
    broadcast
      t
      (Loop
         (Tool_end
            { call
            ; result =
                { Message.Tool_result.tool_call_id = call_id
                ; tool_name = "shell"
                ; text = result.text
                ; is_error = result.is_error
                ; images = []
                }
            }));
    if add_to_context
    then (
      let truncated = Truncate.head (Buffer.contents output) in
      let output =
        if truncated.truncated
        then
          sprintf
            "[output truncated: showing the first part of %d lines / %d bytes]\n\
             %s"
            truncated.total_lines
            truncated.total_bytes
            truncated.text
        else truncated.text
      in
      let message =
        Message.user
          ~origin:{ Injected.empty with source = Some (Shell { command }) }
          (sprintf "$ %s\n%s" command output)
      in
      broadcast t (Loop (Message_start message));
      ignore (Session.append_message t.session message : Session.Entry.t);
      broadcast t (Loop (Message_end message));
      state_changed t);
    Ok result)
;;

let set_thinking t thinking =
  t.thinking <- thinking;
  ignore
    (Session.set_model t.session ~model:(Model.key t.model) ~thinking
     : Session.Entry.t);
  state_changed t
;;

let compact ?instructions t =
  if is_running t
  then Or_error.error_string "cannot compact while a run is in progress"
  else
    Compaction.compact
      ~env:t.env
      ~provider:t.provider
      ~model:t.model
      ?instructions
      t.session
    |> Or_error.map ~f:(fun (outcome : Compaction.Outcome.t) ->
      compacted t outcome;
      state_changed t;
      outcome.summary)
;;

(* ---- /diff, /review, /init ------------------------------------------ *)

let git_diff ?path ?base ?dir ?rev ?direct ?default_base t =
  host_exec
    t
    ~cancel:Cancellation.never
    ~on_output:ignore
    ~call_id:"git_diff"
    ~cwd:t.cwd
    ~name:Git_diff.op
    ~arguments:(Git_diff.args ?path ?base ?dir ?rev ?direct ?default_base ())
  |> Git_diff.of_tool_result
;;

let git_refs ?dir t =
  host_exec
    t
    ~cancel:Cancellation.never
    ~on_output:ignore
    ~call_id:"git_refs"
    ~cwd:t.cwd
    ~name:Git_diff.refs_op
    ~arguments:(Git_diff.refs_args ?dir ())
  |> Git_diff.refs_of_tool_result
;;

let git_show ?dir t ~rev ~path =
  host_exec
    t
    ~cancel:Cancellation.never
    ~on_output:ignore
    ~call_id:"git_show"
    ~cwd:t.cwd
    ~name:Git_diff.show_op
    ~arguments:(Git_diff.show_args ?dir ~rev ~path ())
  |> Git_diff.shown_of_tool_result
;;

let resolve_path t path =
  let path = Tool.expand_home path in
  if Filename.is_absolute path then path else Filename.concat t.cwd path
;;

(* ---- worktrees ------------------------------------------------------ *)

let worktree_call ?cwd t ~name ~arguments =
  host_exec
    t
    ~cancel:Cancellation.never
    ~on_output:ignore
    ~call_id:"worktree"
    ~cwd:(Option.value_map cwd ~default:t.cwd ~f:(resolve_path t))
    ~name
    ~arguments
;;

let worktrees ?cwd t =
  worktree_call
    ?cwd
    t
    ~name:Worktree.list_op
    ~arguments:(Worktree.list_args ~dir:t.config.worktrees_dir)
  |> Worktree.listing_of_result
;;

let create_worktree ?cwd t ~branch ~base =
  worktree_call
    ?cwd
    t
    ~name:Worktree.create_op
    ~arguments:
      (Worktree.create_args ~dir:t.config.worktrees_dir ~branch ~base ())
  |> Worktree.created_of_result
;;

let remove_worktree ?cwd t ~path ~force =
  worktree_call
    ?cwd
    t
    ~name:Worktree.remove_op
    ~arguments:
      (Worktree.remove_args ~dir:t.config.worktrees_dir ~path ~force ())
  |> Worktree.removed_of_result
;;

(* A command's prompt: sent now when idle, otherwise after the current run
   like a follow-up. *)
let command_prompt t ~text ~expanded ~source =
  match budget_exhausted t with
  | Some reason -> Or_error.error_string reason
  | None ->
    enqueue_queued
      t
      Follow_up
      { Queued.text
      ; expanded = Some expanded
      ; source = Some source
      ; attachments = []
      ; images = []
      };
    Ok ()
;;

let review t ~args =
  let open Or_error.Let_syntax in
  let first, rest = Review.split_args args in
  let%bind target, diff, instructions =
    let uncommitted ~instructions =
      let%map diff = git_diff t in
      Review.Target.Uncommitted, diff, instructions
    in
    match first with
    | None -> uncommitted ~instructions:None
    | Some base ->
      (match git_diff t ~base with
       | Ok diff ->
         Ok
           ( Review.Target.Base { base; merge_base = diff.against }
           , diff
           , Option.some_if (not (String.is_empty rest)) rest )
       | Error e when Git_diff.is_unknown_revision (Error.to_string_hum e) ->
         uncommitted ~instructions:(Some (String.strip args))
       | Error _ as e -> e)
  in
  if String.is_empty (String.strip diff.diff)
  then Or_error.error_string (Review.nothing_to_review target ~root:diff.root)
  else
    command_prompt
      t
      ~text:(String.strip ("/review " ^ String.strip args))
      ~expanded:(Review.prompt ~target ~instructions diff)
      ~source:(Review { target = Review.Target.describe target })
;;

let init t =
  command_prompt
    t
    ~text:"/init"
    ~expanded:(Init_prompt.prompt ~cwd:t.cwd)
    ~source:Init
;;

let review_state t = t.review

let submit_review ?root t =
  match budget_exhausted t with
  | Some reason -> Or_error.error_string reason
  | None ->
    Or_error.map
      (update_review t ~f:(fun review ->
         Or_error.map
           (Review_state.submit ?root review)
           ~f:(fun (text, n, changes) -> (text, n), changes)))
      ~f:(fun (text, count) ->
        enqueue_queued
          t
          Steer
          { Queued.text =
              (if count = 1
               then "Review comment"
               else sprintf "%d review comments" count)
          ; expanded = Some text
          ; source = Some (Review_comments { count })
          ; attachments = []
          ; images = []
          };
        count)
;;

let decide_proposal ?by t ~id ~post ~accept =
  let open Or_error.Let_syntax in
  let%bind comment, edits = Review_state.proposal t.review ~id ~post in
  let state : Review_comment.Proposal.State.t =
    if not accept
    then Rejected
    else (
      let result =
        host_exec
          t
          ~cancel:Cancellation.never
          ~on_output:ignore
          ~call_id:"review"
          ~cwd:comment.root
          ~name:"edit"
          ~arguments:
            (`Object
                [ "path", `String (Review_comment.file comment)
                ; ( "edits"
                  , `Array
                      (List.map edits ~f:(fun (e : Review_comment.Edit.t) ->
                         `Object
                           [ "old_text", `String e.old_text
                           ; "new_text", `String e.new_text
                           ])) )
                ])
      in
      if result.is_error then Failed (String.strip result.text) else Accepted)
  in
  update_review_file
    ?by
    t
    ~root:comment.root
    ~path:comment.path
    ~action:(if accept then Accepted else Rejected)
    ~f:(fun review -> Review_state.decide review ~id ~post state ~at:(now t))
;;

let review_since t ~root ?path ?index () =
  let open Or_error.Let_syntax in
  let paths =
    match path with
    | Some path -> [ path ]
    | None ->
      Map.keys t.review.snapshots
      |> List.filter_map ~f:(fun (k : Review_state.File_key.t) ->
        Option.some_if (Option.exists k.root ~f:(String.equal root)) k.path)
  in
  List.map paths ~f:(fun path ->
    let%bind current = read_review_file t ~root ~path in
    Review_state.since t.review ~root ~path ?index ~current ())
  |> Or_error.all
;;

(* ---- budget, goal and resume ----------------------------------------- *)

let set_budget t usd =
  match usd with
  | Some usd when Float.( <= ) usd 0. || Float.is_nan usd ->
    Or_error.errorf
      "a budget must be a positive number of dollars, e.g. /budget 5 (or \
       /budget off)"
  | _ ->
    ignore (Session.set_budget t.session ~usd : Session.Entry.t);
    state_changed t;
    Ok ()
;;

let goal_prompt t goal =
  if is_running t
  then ()
  else
    start_run
      t
      [ goal_message
          t
          ~source:(Goal_start { goal = goal.Goal.text })
          (Goal.start_message goal)
      ]
;;

let set_goal t text =
  let text = String.strip text in
  if String.is_empty text
  then
    Or_error.error_string
      "say what done means, e.g. /goal all tests pass (/goal off clears it)"
  else (
    match budget_exhausted t with
    | Some reason -> Or_error.error_string reason
    | None ->
      let goal = Goal.create text in
      set_goal_state t (Some goal);
      state_changed t;
      goal_prompt t goal;
      Ok ())
;;

let clear_goal t =
  match t.goal with
  | None -> Or_error.error_string "there is no goal; /goal TEXT sets one"
  | Some _ ->
    set_goal_state t None;
    state_changed t;
    Ok ()
;;

let resume_goal t =
  match t.goal with
  | None -> Or_error.error_string "there is no goal; /goal TEXT sets one"
  | Some goal when Goal.is_active goal ->
    Or_error.error_string "the goal is already active"
  | Some goal ->
    (match budget_exhausted t with
     | Some reason -> Or_error.error_string reason
     | None ->
       let goal = { goal with status = Active; continuations = 0 } in
       set_goal_state t (Some goal);
       state_changed t;
       goal_prompt t goal;
       Ok ())
;;

(* ---- reminders --------------------------------------------------------- *)

let set_reminders t reminders =
  t.reminders <- reminders;
  ignore (Session.set_reminders t.session reminders : Session.Entry.t);
  state_changed t
;;

let reminders t = t.reminders

let add_reminder t ~text trigger =
  Or_error.map
    (Reminder.create ~taken:(Session.reminder_names t.session) ~text trigger)
    ~f:(fun reminder ->
      set_reminders t (t.reminders @ [ reminder ]);
      reminder)
;;

let no_reminders = "there are no reminders; /remind stop TEXT adds one"

let remove_reminder t name =
  let name = String.strip name in
  match
    List.partition_tf t.reminders ~f:(fun (r : Reminder.t) ->
      String.equal r.name name)
  with
  | _ :: _, rest ->
    set_reminders t rest;
    Ok ()
  | [], [] -> Or_error.errorf "no reminder %S: %s" name no_reminders
  | [], rest ->
    Or_error.errorf
      "no reminder %S; the session's are %s (/remind lists them)"
      name
      (String.concat
         ~sep:", "
         (List.map rest ~f:(fun (r : Reminder.t) -> r.name)))
;;

let clear_reminders t =
  match t.reminders with
  | [] -> Or_error.error_string no_reminders
  | _ :: _ ->
    set_reminders t [];
    Ok ()
;;

(* Leaving plan mode while idle carries on an active goal, which waited for
   the plan's approval. *)
let set_mode ?approval ?plan t =
  let was_planning = t.plan_mode in
  switch_mode ?approval ?plan t;
  match t.goal with
  | Some goal
    when was_planning
         && (not t.plan_mode)
         && Goal.is_active goal
         && (not (is_running t))
         && Option.is_none (budget_exhausted t)
         && goal.continuations < t.config.goal_max_continuations ->
    let goal = { goal with continuations = goal.continuations + 1 } in
    set_goal_state t (Some goal);
    start_run
      t
      [ goal_message
          t
          ~source:
            (Goal_continuation
               { continuation = goal.continuations
               ; max = t.config.goal_max_continuations
               })
          (Goal.continuation_message
             goal
             ~max_continuations:t.config.goal_max_continuations)
      ]
  | Some _ | None -> ()
;;

let no_resume =
  "no run is waiting for a usage limit to reset (one is scheduled when a run \
   stops at a limit whose reset time the provider gives)"
;;

let cancel_resume t =
  match t.resume with
  | None -> Or_error.error_string no_resume
  | Some _ ->
    cancel_resume_timer t;
    state_changed t;
    Ok ()
;;

let resume_pending t = Option.is_some t.resume

let resume_now t =
  match t.resume with
  | None -> Or_error.error_string no_resume
  | Some _ ->
    cancel_resume_timer t;
    if is_running t then state_changed t else start_run t [ resume_message ];
    Ok ()
;;

let btw t ~question ~cancel ~on_delta =
  let session = t.session in
  let model = t.model in
  let system =
    match Session.system_prompt session with
    | Some text -> text
    | None -> build_system_prompt t
  in
  let request =
    Btw.request
      ~cache_key:(Session.id session)
      ~model
      ~system
      ~messages:(Session.messages session)
      ~question
      ()
  in
  let reply =
    t.provider.stream request ~cancel ~on_event:(function
      | Text_delta text -> on_delta text
      | Thinking_delta _
      | Thinking_signature _
      | Tool_call_start _
      | Tool_call_delta _
      | Server_tool_use _
      | Server_tool_result _ -> ())
  in
  let cost_usd = Model.cost_usd model reply.usage in
  ignore
    (Session.add_spend session ~usage:reply.usage ~cost_usd : Session.Entry.t);
  if phys_equal session t.session then state_changed t;
  match reply.stop_reason with
  | Error e -> Or_error.error_string e
  | Aborted -> Or_error.error_string "cancelled"
  | End_turn | Tool_use | Length -> Ok (reply, cost_usd)
;;

(* With [keep_host] (a new session or a fork) the session carries on where
   this one is; otherwise it goes where it last ran. *)
let replace_session t session ~keep_host =
  ignore (abort t);
  cancel_resume_timer t;
  Background_tasks.cancel_all ~discard:true t.background;
  wait_idle t;
  Background_tasks.forget_finished t.background;
  t.session <- session;
  Reminder.Tracker.reset t.reminder_tracker;
  load_subagent_log t;
  if keep_host then record_location t else t.cwd <- Session.cwd session;
  restore_host t;
  t.repo_asked <- None;
  refresh_location t;
  restore_settings t;
  restore_told t;
  (match List.hd (hosts t) with
   | Some host when String.is_empty t.active_host ->
     activate_host t host ~cwd:host.cwd ~pinned:false
   | Some _ | None -> ());
  restore_resume t;
  state_changed t
;;

let new_session t =
  replace_session
    t
    (Session.create ~dir:t.sessions_dir ~cwd:t.cwd ())
    ~keep_host:true
;;

let switch_session t ~path =
  Or_error.map (Session.load path) ~f:(fun session ->
    replace_session t session ~keep_host:false)
;;

let fork t ?at () =
  Or_error.map
    (Session.fork ?at t.session ~dir:t.sessions_dir)
    ~f:(fun session -> replace_session t session ~keep_host:true)
;;

let rewind t ~to_ =
  if is_running t
  then Or_error.error_string "cannot rewind while a run is in progress"
  else
    Or_error.map (Session.rewind t.session ~to_) ~f:(fun () ->
      t.plan <- Plan.latest (path_messages t.session);
      restore_told t;
      state_changed t)
;;

let set_session_name t name =
  ignore (Session.set_name t.session ~name : Session.Entry.t);
  state_changed t
;;

let set_cwd
      ?(if_missing = Dir_change.If_missing.Fail)
      ?(worktree = false)
      t
      ~path
  =
  if is_running t
  then
    Or_error.error_string "cannot change directory while a run is in progress"
  else (
    match find_host t t.active_host with
    | None -> Or_error.error_string (host_unavailable_message t)
    | Some host ->
      Or_error.map
        (change_dir_on t host (resolve_path t path) ~if_missing)
        ~f:
          (apply_dir_change ~f:(fun cwd ->
             moving_home
               t
               ~worktree
               ~to_:(fun () -> place_on t host ~pinned:t.host_pinned ~cwd)
               ~move:(fun () -> set_host_cwd t host ~cwd);
             state_changed t)))
;;

let no_home_message =
  "this session hasn't switched into a worktree; /worktree open TARGET switches"
;;

let switch_back ?(main = false) t =
  match Session.home t.session with
  | None -> Or_error.error_string no_home_message
  | Some _ when is_running t ->
    Or_error.error_string "cannot change directory while a run is in progress"
  | Some (home : Session.Home.t) ->
    let host_id =
      Option.value_map home.host ~default:t.active_host ~f:(fun h -> h.id)
    in
    let host_name =
      Option.value_map home.host ~default:t.active_host_name ~f:(fun h ->
        h.name)
    in
    let root = Option.map home.repo ~f:(fun r -> r.root) in
    (match find_host t host_id, main, root with
     | None, _, _ ->
       Or_error.errorf
         "the tool host %s that %s is on is not connected: reconnect it (or \
          /host picks another, and /cd a directory there)"
         (if String.is_empty host_name then "(none)" else host_name)
         home.cwd
     | Some _, true, None ->
       Or_error.errorf
         "%s is not in a repository that prigh knows: /cd elsewhere"
         home.cwd
     | Some host, _, _ ->
       let target = if main then Option.value_exn root else home.cwd in
       (match resolve_dir_on t host target with
        | Error _ when not main ->
          Or_error.errorf
            "%s is gone%s"
            home.cwd
            (match root with
             | Some root ->
               sprintf
                 "; /worktree back main goes to the repository's main checkout \
                  (%s) instead"
                 root
             | None -> "; /cd elsewhere (that forgets it)")
        | Error _ as error -> error
        | Ok cwd ->
          let pinned =
            Option.value_map home.host ~default:t.host_pinned ~f:(fun h ->
              h.pinned)
          in
          if String.equal host.id t.active_host
          then set_host_cwd t host ~cwd
          else activate_host t host ~cwd ~pinned;
          ignore (Session.set_home t.session None : Session.Entry.t);
          state_changed t;
          Ok (Dir_change.Outcome.Changed { cwd; created = false })))
;;

let host_call
      t
      ?stdin
      ?(cancel = Cancellation.never)
      ?(on_output = ignore)
      ~name
      arguments
  =
  active_host_exec
    t
    ?stdin
    ~cancel
    ~on_output
    ~call_id:"files"
    ~cwd:t.cwd
    ~name
    ~arguments
    ()
;;

let list_paths t ~prefix =
  match
    host_exec
      t
      ~cancel:Cancellation.never
      ~on_output:ignore
      ~call_id:"list_paths"
      ~cwd:t.cwd
      ~name:Host_ops.list_paths_op
      ~arguments:(`Object [ "prefix", `String prefix ])
  with
  | { is_error = true; text; _ } -> Or_error.error_string text
  | { is_error = false; text; _ } -> Json.parse text
;;

let list_dirs ?host t ~prefix =
  let host =
    match host with
    | None -> Ok (find_host t t.active_host)
    | Some id ->
      (match find_host t id with
       | Some host -> Ok (Some host)
       | None -> Or_error.errorf "unknown tool host %S" id)
  in
  Or_error.bind host ~f:(function
    | None -> Or_error.error_string (host_unavailable_message t)
    | Some host ->
      let cwd =
        if String.equal host.id t.active_host then t.cwd else host.cwd
      in
      (match
         host_exec_on
           t
           host
           ~cancel:Cancellation.never
           ~on_output:ignore
           ~call_id:"list_dirs"
           ~cwd
           ~name:Host_ops.list_dirs_op
           ~arguments:(`Object [ "prefix", `String prefix ])
       with
       | { is_error = true; text; _ } -> Or_error.error_string text
       | { is_error = false; text; _ } -> Json.parse text))
;;

let delete_session t ~path =
  if String.equal path (Session.path t.session)
  then Or_error.error_string "cannot delete the active session"
  else Or_error.try_with (fun () -> Session.delete_files path)
;;

let export t ~format ?path () =
  let name =
    let base = Filename.basename (Session.path t.session) in
    let stamp = List.hd_exn (String.split base ~on:'_') in
    sprintf
      "%s_%s.%s"
      stamp
      (Session.id t.session)
      (Session.Export_format.extension format)
  in
  let data () =
    match format with
    | Session.Export_format.Markdown -> Session.to_markdown t.session
    | Jsonl -> In_channel.read_all (Session.path t.session)
  in
  if t.backend_host_enabled
  then (
    let path =
      match path with
      | Some p -> resolve_path t p
      | None -> Filename.concat (Filename.concat t.sessions_dir "exports") name
    in
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p (Filename.dirname path);
      Out_channel.write_all path ~data:(data ());
      path))
  else (
    (* The backend's files are not the user's: write on the tool host. *)
    let path = Option.value path ~default:name in
    match
      host_exec
        t
        ~cancel:Cancellation.never
        ~on_output:ignore
        ~call_id:"export"
        ~cwd:t.cwd
        ~name:"write"
        ~arguments:
          (`Object [ "path", `String path; "content", `String (data ()) ])
    with
    | { is_error = true; text; _ } -> Or_error.error_string text
    | { is_error = false; _ } ->
      Ok
        (if Filename.is_absolute path then path else Filename.concat t.cwd path))
;;

let import_session t ~path =
  Or_error.map (Session.import ~dir:t.sessions_dir path) ~f:(fun session ->
    replace_session t session ~keep_host:false;
    Session.path session)
;;

let session_stats t =
  let path = Session.active_path t.session in
  let messages = Session.messages t.session in
  let assistants =
    List.filter_map messages ~f:(function
      | Message.Assistant a -> Some a
      | User _ | Tool_result _ -> None)
  in
  let tool_calls = String.Table.create () in
  List.iter assistants ~f:(fun a ->
    List.iter (Message.Assistant.tool_calls a) ~f:(fun call ->
      Hashtbl.update tool_calls call.name ~f:(function
        | None -> 1
        | Some n -> n + 1)));
  let count is_model =
    List.count path ~f:(fun (e : Session.Entry.t) ->
      match is_model, e.payload with
      | true, Model _ -> true
      | false, Compaction _ -> true
      | _ -> false)
  in
  let state = state t in
  let context_percent =
    match state.context_tokens with
    | 0 -> 0.
    | tokens ->
      Float.of_int tokens /. Float.of_int t.model.context_window *. 100.
  in
  { Session_stats.message_count = List.length messages
  ; turns = List.length assistants
  ; tool_calls =
      Hashtbl.to_alist tool_calls
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  ; usage = state.usage
  ; cost_usd = state.cost_usd
  ; context_percent
  ; model_changes = count true
  ; compactions = count false
  ; duration_seconds = Session.duration_seconds t.session
  }
;;
