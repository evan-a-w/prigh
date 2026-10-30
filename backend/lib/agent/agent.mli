open! Core
open! Import

(** Orchestrates one interactive conversation: owns the session, model
    settings, the running loop (at most one at a time), the steer/follow-up
    queues, and broadcasts events to subscribers. *)

module Host : sig
  (** Where a session's [on_host] tools run: the backend itself (id
      [backend_id]) or a connected client that advertised tool support. *)
  type t =
    { id : string
      (** a client's host id, the same across its reconnects (see
          [Rpc_server]) *)
    ; name : string
    ; cwd : string
    ; session_id : string option (** the session the client is attached to *)
    ; session_name : string option
    ; home : string option [@sexp.option]
      (** its home directory, for [~] in paths meant for it ([hello]'s
          [home]; unknown for a host predating it) *)
    ; exec_input : bool [@sexp.bool]
      (** it takes [Tool_exec_input] (its [hello] listed the [exec_input]
          feature) *)
    ; git_location : bool [@sexp.bool]
      (** it runs [$git_location] (the [git_location] feature): sessions
          record where their cwd there is in git *)
    }
  [@@deriving sexp_of, equal]

  val backend_id : string
end

module State : sig
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
      (** every assistant message in the session file (all branches, also
          those compacted away), plus subagents, compactions and [btw] calls
          since the agent loaded the session *)
    ; cost_usd : float (** of [usage], each response at its model's price *)
    ; context_tokens : int (** input tokens of the last request, if any *)
    ; active_host : string
      (** [Host.id]; absent from [hosts] while that host is disconnected, or
              [""] when the backend host is disabled and no client host was
              ever adopted *)
    ; hosts : Host.t list
      (** the backend first (unless disabled), then connected clients *)
    ; subagents : Background_tasks.Summary.t list
      (** background subagents still running or not yet delivered *)
    ; jobs : Background_tasks.Summary.t list
      (** background shell jobs still running or not yet delivered *)
    ; budget_usd : float option
      (** no turn starts once [cost_usd] reaches it, see {!set_budget} *)
    ; goal : Goal.t option
    ; goal_max_continuations : int
    ; reminders : Reminder.t list (** see {!add_reminder} *)
    ; reminder_stops : int
      (** the times on-stop reminders fired in a row since the user spoke *)
    ; resume_at : float option
      (** when a run stopped by a usage limit resumes (seconds since the
          epoch) *)
    ; approval_mode : Approval_mode.t
    ; plan_mode : bool
    ; plan : Plan.t option (** the model's [update_plan] *)
    ; session_rules : string list
      (** rules the user allowed for this session ("don't ask again") *)
    ; home : Session.Home.t option
      (** where a worktree switch took it from ({!Session.home}): {!switch_back} goes there *)
    ; trim : Session.Trim.t option
      (** what is trimmed from the model's context ({!Session.trim}) *)
    ; hidden : Session.Hidden.t list
      (** what trims and deletions keep out of it ({!Session.hidden}) *)
    ; transcript_filter : string option
      (** what prigh-web shows of the transcript ({!set_transcript_filter}) *)
    }
  [@@deriving sexp_of]
end

module Session_stats : sig
  type t =
    { message_count : int
    ; turns : int (** assistant messages *)
    ; tool_calls : (string * int) list (** by tool name *)
    ; usage : Usage.t
    ; cost_usd : float
    ; context_percent : float
    ; model_changes : int
    ; compactions : int
    ; masks : int (** observation maskings ({!Observation_mask}) *)
    ; duration_seconds : float
    }
  [@@deriving sexp_of]
end

module Event : sig
  type t =
    | Loop of Agent_event.t
    | State_changed of State.t
    | Compacted of { summary : string }
    | Notice of string
    | Config_changed of Config.t
    | Review_changed of Review_state.t (** after every change *)
    | Queue_update of
        { steer : string list (** the queued texts, in order *)
        ; follow_up : string list
        }
    | Subagent_queues of Subagent_inbox.Listing.t list
    (** the background subagents' queues, those with messages; after every
        change of one *)
    | Tool_exec of
        { host : string (** only delivered to this host *)
        ; session_id : string (** the session whose tool call it is *)
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

module Queued : sig
  type t =
    { text : string (** as typed, e.g. [/skill:NAME ARGS] *)
    ; expanded : string option
      (** what the model gets instead: the expanded skill invocation, or a
        command's prompt *)
    ; source : Injected.Source.t option
    ; attachments : string list
    ; images : Image.t list
    }
  [@@deriving sexp_of]
end

(** A tool call waiting for [respond_confirm]. *)
module Pending_confirm : sig
  type t =
    { call_id : string
    ; agent_id : string option (** a subagent's call *)
    ; name : string
    ; summary : string (** what the call does; [exit_plan_mode]'s is the plan *)
    ; rules : string list
      (** what [Always] adds ({!Approval_rule.suggest}); empty when it is
          not offered *)
    }
  [@@deriving sexp_of]
end

(** How the user answers a confirmation: run it once, run it and add the
    confirmation's rules to the session (so calls like it run without
    asking), or refuse it, optionally saying why (the model sees it). *)
module Confirm_answer : sig
  type t =
    | Allow
    | Always
    | Deny of string option
  [@@deriving sexp_of]
end

(** An [ask_user] call waiting for [answer_question]. *)
module Pending_question : sig
  type t =
    { call_id : string
    ; questions : User_question.t list
    ; deadline : float option
      (** when it times out (seconds since the epoch): the config's
          [ask_user_timeout_seconds] or the call's [timeout_seconds] after it
          was asked, see {!User_question.timeout} *)
    }
  [@@deriving sexp_of]
end

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> provider:Provider.t
  -> tools:Tool.t list
  -> sessions_dir:string
  -> home:string
  -> ?session:Session.t
  -> ?model:Model.t
       (** default: the config's [default_model], else [fallback_model] *)
  -> ?thinking:Thinking.t
       (** default: the config's [default_thinking], else [Off] *)
  -> ?fallback_model:Model.t (** default: [Model.default] *)
  -> ?models:Model_registry.t
       (** resolves the session's and the config's model keys *)
  -> ?auto_describe:bool
       (** write a [Session_description] after a turn once the conversation
           is long enough (default: false) *)
  -> ?backend_host:bool
       (** whether the backend itself is a tool host (default: true). Without
           it, [on_host] tools, instructions, path listings and the git branch
           never touch the backend's filesystem, and a session that never
           had a host adopts the first connected client host. *)
  -> ?mcp:Mcp_hub.t
       (** the backend's MCP servers, when it is the tool host; without it
           sessions use no MCP servers on any host *)
  -> ?use_default_cwd:bool
       (** start a new session in the config's [default_cwd] rather than
           [cwd] (default: true; false when [cwd] was given explicitly) *)
  -> ?max_budget_usd:float
       (** the budget of sessions that set none (the CLI's
           [-max-budget-usd]), instead of the config's [max_budget_usd] *)
  -> ?sleep_until:(float -> unit)
       (** waits until a time (seconds since the epoch): for {!resume_now}'s
           automatic counterpart and for an [ask_user] question's timeout;
           default: the clock *)
  -> ?trace_clock:Trace.Clock.t
       (** times the session's trace ({!Trace_file}); default: the clocks *)
  -> cwd:string
  -> unit
  -> t

val subscribe : t -> f:(Event.t -> unit) -> unit
val state : t -> State.t
val session : t -> Session.t
val env : t -> Env.t
val messages : t -> Message.t list

(** Attachments are paths (relative to the agent cwd or absolute) whose
    contents are appended to the user message as [<file>] blocks, read with
    [Tool_read] limits (image files are attached as images, like [images]).

    A text [/skill:NAME ARGS] invokes the skill [NAME] from the tool host
    ({!Skill.expand}); an unknown name is an error. *)

(** Starts a run. Fails if one is already running. *)
val prompt
  :  ?attachments:string list
  -> ?images:Image.t list
  -> t
  -> string
  -> unit Or_error.t

(** Queued and injected after the current turn's tool results, or starts a
    run when idle. *)
val steer
  :  ?attachments:string list
  -> ?images:Image.t list
  -> t
  -> string
  -> unit Or_error.t

(** Queued to run after the current run finishes, or starts a run when idle. *)
val follow_up
  :  ?attachments:string list
  -> ?images:Image.t list
  -> t
  -> string
  -> unit Or_error.t

(** The skills on the active tool host. *)
val skills : t -> Skill.t list Or_error.t

(** The MCP servers for the session on the active tool host, starting those
    not running yet; [reconnect] restarts failed ones. Their tools are
    offered to the model from the next run on (each run starts by asking the
    host again), and problems are reported once each as [Notice]s. *)
val mcp_servers : ?reconnect:bool -> t -> Mcp_tools.Listing.t Or_error.t

(** Approves a project's MCP server on the host and starts it. *)
val approve_mcp
  :  t
  -> source:string
  -> server:string
  -> Mcp_tools.Listing.t Or_error.t

(** Cancels the active run (if any) and clears the queues. Returns the texts
    that were queued and are therefore restored to the caller: steer messages
    in order, then follow-ups. Emits [Queue_update]. *)
val abort : t -> string list

(** Pops the most recently queued message: follow-ups take priority over steer
    messages. Emits [Queue_update] when a message was removed. *)
val dequeue : t -> Queued.t option

(** Removes the [index]th (from 0) message of a queue, if it is still there
    (and its text is [text], when given: the frontend's view may be stale).
    Emits [Queue_update]. *)
val remove_queued
  :  t
  -> queue:Steer_queue.Kind.t
  -> index:int
  -> text:string option
  -> Queued.t Or_error.t

(** The texts of the queued steer and follow-up messages, in order (what the
    last [Queue_update] said). *)
val queued_texts : t -> string list * string list

(** Confirmations still unanswered, by call id. *)
val pending_confirms : t -> Pending_confirm.t list

(** Runs [command] through the bash machinery, emitting [Tool_start],
    [Tool_output] and [Tool_end] events under a synthetic [shell-<n>] call id.
    When [add_to_context] is set, appends [\$ <command>\n<output>] to the
    session as a user message. Fails while a run is in progress. *)
val shell
  :  t
  -> command:string
  -> add_to_context:bool
  -> Tool_result.t Or_error.t

(** Whether the main loop is running (background subagents do not count). *)
val is_running : t -> bool

(** Blocks until no run is active, the queues are empty and no background
    subagent or job is running (their deliveries included). *)
val wait_idle : t -> unit

(** {2 Background subagents and jobs}

    The [subagent] tool and [bash] with [background] start them and return at
    once. A finished one's report becomes a user message (several finishing
    together share one): an idle agent starts a turn for it, a running one
    injects it at the next turn boundary (with steering), unless
    [subagent_cancel]/[job_kill] already returned it. [abort] leaves them
    running; after an abort, reports that are ready wait for the next run. *)

val has_running_subagents : t -> bool

(** Subagents or jobs. *)
val has_running_background : t -> bool

(** How many background subagents and jobs are running. *)
val running_background : t -> int

(** Every subagent run since the agent was created (or its session was
    replaced), with status and activity, oldest first. *)
val subagents : t -> Subagent_log.Summary.t list

(** One of [subagents] by agent id or starting tool call id, with its
    transcript. *)
val subagent
  :  t
  -> string
  -> (Subagent_log.Summary.t * Timed_message.t list) option

(** Cancels one; its partial report is still delivered. *)
val cancel_subagent : t -> agent_id:string -> unit Or_error.t

(** Sends the user's message to a running background subagent: a steer
    reaches it at its next step boundary, a follow-up when it ends its turn
    (it carries on with it rather than report). A subagent that finished, or
    a subagent's own, refuses it, saying what to do instead; one that stops
    before taking a message drops it with a [Notice]. Emits [Queue_update]
    whenever a subagent's queue changes. *)
val steer_subagent
  :  t
  -> agent_id:string
  -> kind:Steer_queue.Kind.t
  -> string
  -> unit Or_error.t

(** Takes back the subagent's most recently queued message (follow-ups
    first), if any. *)
val dequeue_subagent : t -> agent_id:string -> string option Or_error.t

(** Like {!remove_queued}, for a subagent's queue. *)
val remove_subagent_queued
  :  t
  -> agent_id:string
  -> queue:Steer_queue.Kind.t
  -> index:int
  -> text:string option
  -> string Or_error.t

(** What [Queue_update]'s [agents] says. *)
val subagent_queues : t -> Subagent_inbox.Listing.t list

(** Cancels every running subagent and job; with [discard] their reports are
    dropped. *)
val cancel_background : ?discard:bool -> t -> unit

(** Background subagents still running when the backend stopped are
    reported to the agent when its session is loaded again, like finished
    ones but [[subagent <id> interrupted]]: this starts the run that
    reports them, if there are any and the agent is idle with no automatic
    resume pending (otherwise the next run does). Switching to a session
    does it too. *)
val report_interrupted_subagents : t -> unit

(** For a backend that is stopping: cancels the run and the background work
    without reporting it, leaving the session's background subagents to be
    reported as interrupted when it is loaded again. *)
val shutdown : t -> unit

(** Every job of this agent, oldest first. *)
val jobs : t -> Background_tasks.Task.t list

(** Kills a running job (on its host); its report is still delivered. *)
val kill_job : t -> job_id:string -> unit Or_error.t

(** The last [lines] lines of a job's output, under a header line. *)
val job_output : t -> job_id:string -> lines:int -> string Or_error.t

(** Starts [command] as a background job on the active host (for [!&cmd]);
    returns its id. *)
val start_job : t -> command:string -> string

val set_model : t -> Model.t -> unit
val set_thinking : t -> Thinking.t -> unit

(** The agent's persistent configuration (loaded from [~/.prigh/config.json] at
    creation). *)
val config : t -> Config.t

(** Writes the configuration and emits [Event.Config_changed]. A changed
    [approval_mode] also switches this session's mode. *)
val set_config : t -> Config.t -> unit Or_error.t

(** When conversations (the session's and its subagents') are masked and
    compacted and who summarises them: the config's [mask_observations],
    [compact_at_tokens] and [compaction_model]. *)
val compaction_settings : t -> Compaction_settings.t

(** Saves the current model and thinking level as the defaults for new
    sessions. Rereads the config file first, so settings changed by other
    sessions are kept. *)
val save_as_default : t -> unit Or_error.t

(** {2 Approvals, plan mode and questions}

    Every tool call of the agent and of its subagents is checked by
    {!Approval.decide} before it runs: with the session's approval mode, plan
    mode, the config's [permissions] and the session's rules. One that needs
    the user's approval emits [Tool_confirm] and waits for
    {!respond_confirm} (or the run's cancellation). The mode and the
    session's rules are recorded in the session ([approval] and
    [allow_rule] entries), so a reloaded session keeps them; a new session
    starts in the config's [approval_mode].

    In plan mode file edits are refused, and every other destructive tool
    asks (unless a rule allows it). The model is told when plan mode changes
    ({!System_prompt.plan_mode_note}: with the next user message, or between
    turns of a running loop), and asks to leave it with [exit_plan_mode],
    whose confirmation carries the plan: approving it leaves plan mode
    (keeping the approval mode). *)

(** Answers a pending [Tool_confirm] request. Fails if no confirmation is
    outstanding for [call_id] (of the subagent [agent_id]): it was answered
    already or withdrawn. [Always] adds the confirmation's rules to the
    session; any other pending confirmation they cover is allowed too. *)
val respond_confirm
  :  ?agent_id:string
  -> t
  -> call_id:string
  -> Confirm_answer.t
  -> unit Or_error.t

(** Switches the approval mode and/or plan mode, recording it in the
    session. Pending confirmations the new mode lets through are allowed;
    leaving plan mode by hand approves a pending [exit_plan_mode], and, when
    idle, carries on an active goal (see {!set_goal}). *)
val set_mode : ?approval:Approval_mode.t -> ?plan:bool -> t -> unit

(** Answers a pending [ask_user] call ([Answered], one list per question, or
    [Declined]). Unanswered, it times out at its deadline: the model gets
    [Timed_out] and [Ask_user_done] says [timed_out]. Fails when there is
    none for [call_id] (another frontend answered first, the run was aborted
    or it timed out) or the answers do not fit the questions. *)
val answer_question
  :  t
  -> call_id:string
  -> User_question.Reply.t
  -> unit Or_error.t

(** Questions still unanswered. *)
val pending_questions : t -> Pending_question.t list

(** [instructions]: what the summary should focus on (the user's). Runs
    also compact by themselves: after a run whose last request crossed
    {!compaction_settings}' threshold, between the turns of a run
    (see {!Agent_loop.run}), and when a request overflows the context
    window. Each emits [Compacted], costed at the prices of the model that
    summarised ({!Compaction_settings.summary_model}). *)
val compact : ?instructions:string -> t -> string Or_error.t

(** {2 Changes: /diff, /review, /init} *)

(** A worktree's changes on the active tool host ({!Git_diff}): the
    session directory's, or [dir]'s; or a branch's ([rev]), not checked
    out. *)
val git_diff
  :  ?path:string
  -> ?base:string
  -> ?dir:string
  -> ?rev:string
  -> ?direct:bool
  -> ?default_base:bool
  -> t
  -> Git_diff.Result.t Or_error.t

(** The branches and tags of the repository of [dir] (default: the
    session's), for completion. *)
val git_refs : ?dir:string -> t -> Git_diff.Refs.t Or_error.t

(** A file ([path], relative to the repository's top) as it is in [rev],
    for the review of a branch that is not checked out. *)
val git_show
  :  ?dir:string
  -> t
  -> rev:string
  -> path:string
  -> Git_diff.Shown.t Or_error.t

(** {2 Worktrees}

    The repository's git worktrees, run on the active tool host in the
    repository of [cwd] (default: the session's; relative to it); new ones
    go under the config's [worktrees_dir] ({!Worktree}). *)

val worktrees : ?cwd:string -> t -> Worktree.Listing.t Or_error.t

val create_worktree
  :  ?cwd:string
  -> t
  -> branch:string
  -> base:string option
  -> Worktree.Created.t Or_error.t

(** Removes a worktree of the session cwd's repository ({!Worktree.remove}). *)
val remove_worktree
  :  ?cwd:string
  -> t
  -> path:string
  -> force:bool
  -> Worktree.t Or_error.t

(** [/review ARGS]: a run reviewing the uncommitted changes, or the changes
    against a base when the first word of [ARGS] names a revision; the rest
    (all of [ARGS] otherwise) is extra instructions. The message is
    [Review.prompt] with the [Review] source; while a run is in progress it
    waits like a follow-up. Refused when there is nothing to review. *)
val review : t -> args:string -> unit Or_error.t

(** [/init]: a run asking the model to write or improve [AGENTS.md]
    ({!Init_prompt}), queued like [review]. *)
val init : t -> unit Or_error.t

(** {2 Review: comments and approvals}

    The session's {!Review_state}, recorded in it. While it has comments the
    main agent has [reply_to_comment] ({!Tool_review_reply}). Each change
    emits [Review_changed]. *)

val review_state : t -> Review_state.t

(** Applies and records the changes [f] returns, giving back its result. *)
val update_review
  :  t
  -> f:(Review_state.t -> ('a * Review_state.Change.t list) Or_error.t)
  -> 'a Or_error.t

(** Sends the posts of the user's the agent has not seen
    ({!Review_state.submit}) as a message with the [Review_comments] source,
    steering a running agent or starting a run; the comments it covers
    (with [root], those on that worktree). *)
val submit_review : ?root:string -> t -> int Or_error.t

(** Like [update_review], then records a {!Review_snapshot} of the file
    [path] of the worktree [root] for [action] (by the user [by]): [text],
    or the file as it is now on the active tool host. A file that cannot be
    read gets no snapshot, nor does one of a branch that is not checked out
    ([branch:NAME]). *)
val update_review_file
  :  ?by:string
  -> ?text:string
  -> t
  -> root:string
  -> path:string
  -> action:Review_snapshot.Action.t
  -> f:(Review_state.t -> ('a * Review_state.Change.t list) Or_error.t)
  -> 'a Or_error.t

(** A file of the worktree [root] as it is now on the active tool host, as
    {!Review_snapshot.content_of} makes it (at most
    {!Review_snapshot.max_bytes} read). *)
val read_review_file
  :  t
  -> root:string
  -> path:string
  -> (Review_snapshot.Content.t * string option) Or_error.t

(** What changed since the last action ({!Review_state.since}; with [path]
    and [index], since that one) in [path], or in each file of the worktree
    [root] with snapshots. *)
val review_since
  :  t
  -> root:string
  -> ?path:string
  -> ?index:int
  -> unit
  -> Review_state.Since.t list Or_error.t

(** Accepts (applies the edits to the comment's file on the active tool host,
    with [edit], in the comment's worktree) or rejects a pending proposal; one that no longer applies is
    [Failed]. Records the file's snapshot, by [by]. *)
val decide_proposal
  :  ?by:string
  -> t
  -> id:string
  -> post:int
  -> accept:bool
  -> Review_comment.t Or_error.t

(** {2 Budget, goal and resume} *)

(** Sets the session's cost budget ([None]: none, also overriding the
    default), recorded in the session. Once [State.cost_usd] reaches it no
    turn starts (subagents' included): the run stops with a [Notice] saying
    how to raise it, and prompts are refused. *)
val set_budget : t -> float option -> unit Or_error.t

(** Sets the goal (recorded in the session) and, when idle, starts working
    on it. While it is active the model has [goal_complete], and each run
    that ends normally is followed by a continuation restating it, up to the
    config's [goal_max_continuations]; an abort, a failed run, the budget or
    the cap stop it (with a [Notice]). In plan mode no continuation follows
    (the model waits for its plan's approval); leaving plan mode resumes
    them. *)
val set_goal : t -> string -> unit Or_error.t

val clear_goal : t -> unit Or_error.t

(** Makes a stopped or completed goal active again, with its continuations
    reset, and starts working on it when idle. *)
val resume_goal : t -> unit Or_error.t

(** {2 Reminders} *)

(** The session's reminders (recorded in it), sent to the main agent, never
    to a subagent: an on-stop one when the agent ends its turn with nothing
    to wait for (after a goal's continuation, which takes precedence), up to
    its [max_repeats] in a row until the user speaks; a periodic one at the
    start of a run or between steps once its turns have passed. *)
val reminders : t -> Reminder.t list

(** Adds a reminder named [r1], [r2], ... (never reused in the session). *)
val add_reminder
  :  t
  -> text:string
  -> Reminder.Trigger.t
  -> Reminder.t Or_error.t

(** Fails with the session's reminder names when there is none by [name]. *)
val remove_reminder : t -> string -> unit Or_error.t

val clear_reminders : t -> unit Or_error.t

(** When a run stops at a usage limit with no fallback model left, and the
    config's [resume_on_limit_reset] is on, it resumes by itself once the
    limit resets, if the provider said when ([State.resume_at]); an abort or
    a new run cancels that. These fail when nothing is scheduled. *)
val cancel_resume : t -> unit Or_error.t

(** Whether a resume is scheduled (the agent must stay loaded for it). *)
val resume_pending : t -> bool

val resume_now : t -> unit Or_error.t

(** Answers a side question with one tool-less model call over a snapshot of
    the conversation (see {!Btw}), concurrently with any run and without
    touching the session; [on_delta] receives the streamed text. The usage
    counts towards [State.usage]/[cost_usd]. Returns the reply and its cost. *)
val btw
  :  t
  -> question:string
  -> cancel:Cancellation.t
  -> on_delta:(string -> unit)
  -> (Message.Assistant.t * float) Or_error.t

(** The model [generate_text] drafts with: the config's [generate_model],
    else the session's. *)
val generate_model : t -> Model.t Or_error.t

(** The model that names the session and its subagents: the config's
    [naming_model], else {!generate_model}. *)
val naming_model : t -> Model.t Or_error.t

(** A name for a subagent the model gave none ({!Subagent_name.ask}), by
    {!naming_model} (the session's model when it is unknown or fails, with a
    notice saying so); [None] when no model can name it. *)
val name_subagent
  :  t
  -> task:string
  -> siblings:string list
  -> cancel:Cancellation.t
  -> string option

(** One tool-less request without thinking, outside the conversation:
    [system] and the one user message [user]; [on_delta] receives the
    streamed text. The cost counts towards the session's. Returns the
    reply's text and its cost. *)
val complete
  :  t
  -> model:Model.t
  -> system:string
  -> user:string
  -> max_tokens:int
  -> cancel:Cancellation.t
  -> on_delta:(string -> unit)
  -> (string * float) Or_error.t

(** In-place session replacement for a single-agent embedding (the CLI and
    tests); background subagents are cancelled and their reports dropped.
    [Rpc_server] instead keeps one agent per session and moves clients
    between them, so they keep running and deliver to their own session.
    A new session or a fork carries on on the active host, in the same cwd;
    a session switched to (or imported) goes back to the host it last ran
    on, or where a new agent would start when it recorded none. *)
val new_session : t -> unit

val switch_session : t -> path:string -> unit Or_error.t
val fork : t -> ?at:string -> unit -> unit Or_error.t

(** {!Session.append_trim}, refused while a run is in progress. *)
val trim : t -> before:string -> Session.Trim.t Or_error.t

(** {!Session.append_trim_after}, refused while a run is in progress. *)
val trim_after : t -> after:string -> Session.Hidden.t Or_error.t

(** {!Session.append_untrim}, refused while a run is in progress. *)
val untrim : ?entry:string -> t -> Session.Hidden.t Or_error.t

(** {!Session.append_delete}, refused while a run is in progress. *)
val delete_message : t -> message:string -> Session.Hidden.t Or_error.t

(** {!Session.append_undelete}, refused while a run is in progress. *)
val undelete : ?entry:string -> t -> Session.Hidden.t Or_error.t

val rewind : t -> to_:string -> unit Or_error.t
val set_session_name : t -> string -> unit

(** Records which kinds of the transcript a frontend shows for the session
    ({!Session.transcript_filter}; [None]: everything); the text is the
    frontend's. Nothing is recorded when it is unchanged. *)
val set_transcript_filter : t -> string option -> unit

(** The active tool host's home directory, for [~] in paths meant for it
    (asked for once when its [hello] did not say); [None] while it is
    unavailable. *)
val active_host_home : t -> string option

(** Changes the working directory used by tools and the system prompt, and
    records it in the session; [~] is the active tool host's home, a
    relative [path] is under the session cwd. Fails while a run is in
    progress. A missing
    directory fails, is created on the active tool host, or (when it could
    be) is answered [Missing] without changing anything, as [if_missing]
    (default [Fail]) says. With [worktree] it is a worktree switch, which
    records the session's home unless it has one; any change of directory
    may forget it ({!Worktree_home}). *)
val set_cwd
  :  ?if_missing:Dir_change.If_missing.t
  -> ?worktree:bool
  -> t
  -> path:string
  -> Dir_change.Outcome.t Or_error.t

(** Moves the session back to its home ({!Session.home}) on the home's
    host, or with [main] to the main checkout of the home's repository
    (when the home's directory is gone), and forgets the home. Fails
    without a home, while that host is disconnected and when the directory
    is gone (saying what to do). *)
val switch_back : ?main:bool -> t -> Dir_change.Outcome.t Or_error.t

(** Paths under the session cwd matching [prefix], listed on the active tool
    host (for [@] completion). *)
val list_paths : t -> prefix:string -> Json.t Or_error.t

(** Runs the host operation [name] (see {!Host_files}) on the active tool
    host, in the session cwd; [stdin] and [on_output] stream to and from
    it. *)
val host_call
  :  t
  -> ?stdin:Stdin_feed.t
  -> ?cancel:Cancellation.t
  -> ?on_output:(string -> unit)
  -> name:string
  -> Json.t
  -> Tool.Result.t

(** Directory completions for [prefix] (absolute, [~/] or relative to the
    session cwd on that host), listed on [host] (default: the active one),
    in the notation the prefix used. *)
val list_dirs : ?host:string -> t -> prefix:string -> Json.t Or_error.t

(** Refuses to delete the active session. *)
val delete_session : t -> path:string -> unit Or_error.t

(** Writes the session to [path] (default: [<sessions_dir>/exports/...]) and
    returns the path written. Without the backend host it is written on the
    active tool host instead (default: a file named like the session in the
    cwd). *)
val export
  :  t
  -> format:Session.Export_format.t
  -> ?path:string
  -> unit
  -> string Or_error.t

(** Copies a session file into the sessions directory and switches to it;
    returns the new path. *)
val import_session : t -> path:string -> string Or_error.t

val session_stats : t -> Session_stats.t

(** This session's trace ({!Trace_file}) and its summary, for [/trace]. *)
val trace_report : t -> Trace_report.Report.t

(** {2 Tool hosts}

    The active host is recorded in the session with its cwd (a [cwd] entry,
    see [Session.Host]), so a session loaded again ({!create} with
    [session], {!switch_session}) is on the host it last ran on, connected
    or not, in the cwd it had there. *)

(** Replaces the client hosts (every connected client able to run tools,
    whichever session it is attached to). In-flight executions on hosts that
    are gone keep waiting: the host may come back with them still running
    ({!Rpc_server} fails them when it does not). Hosts keep the cwd this session last used on them, also
    while disconnected. The active host stays active when it goes away; when
    it is listed again the session resumes on it in the same cwd. Meanwhile
    the model's tool calls (and jobs) wait for it until its time in
    [returning] (hosts that went away and may come back, by id; default:
    none), the session moves to another host or the call is cancelled, then
    fail with {!host_unavailable_message}; other calls fail at once. Without
    the backend host, a session that never had a host adopts the first
    one. *)
val set_hosts : ?returning:float String.Map.t -> t -> Host.t list -> unit

(** Makes [id] the active host (with its own cwd) when the session runs on
    the backend by default (not pinned with {!set_active_host}) or has no
    host; called when a client attaches. *)
val prefer_host : t -> string -> unit

(** Why tools cannot run while the active host is not connected: no host
    yet, or waiting for it to reconnect. *)
val host_unavailable_message : t -> string

(** The backend first (unless disabled), then the client hosts. *)
val hosts : t -> Host.t list

val active_host : t -> string

(** Where the session runs, as it records it ({!Session.Host}): the active
    host, connected or not, and the cwd there; [None] while it has none. *)
val location : t -> (Session.Host.t * string) option

(** Switches where tools run from the next call on. The session cwd becomes
    [cwd], which must be a directory on that host (resolved there; [~/] is
    expanded), or the host's own cwd when omitted. Fails for an unknown host
    without switching; a missing directory is handled as by {!set_cwd}. *)
val set_active_host
  :  ?if_missing:Dir_change.If_missing.t
  -> t
  -> string
  -> cwd:string option
  -> Dir_change.Outcome.t Or_error.t

(** Streamed output of a remote execution, relayed by the host. *)
val tool_exec_output : t -> exec_id:string -> chunk:string -> unit Or_error.t

(** Completes a remote execution. *)
val tool_exec_result : t -> exec_id:string -> Tool_result.t -> unit Or_error.t
