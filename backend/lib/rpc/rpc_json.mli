open! Core
open! Import

(** Wire encoding for the frontend: plain tagged JSON objects rather than the
    derived [["Ctor", ...]] form. *)

val message : Message.t -> Json.t

(** [message] with ["at"], milliseconds since the epoch, when known. *)
val timed_message : Timed_message.t -> Json.t

val delta : Assistant_event.t -> Json.t
val state : Agent.State.t -> Json.t

(** What [trim] or [untrim] took out or brought back: [{messages, tokens}]. *)
val trim_result : Session.Trim.t -> Json.t

(** [{"name", "text", "on": "stop", "max_repeats", "repeats"}] or [{"name",
    "text", "every_turns"}]; [repeats]: the times it fired in a row, from
    [stops] (see [Agent.State.reminder_stops]). *)
val reminder : stops:int -> Reminder.t -> Json.t

val config : Config.t -> Json.t

(** [{id, root, path, first, last, anchor, status: open|resolved, thread:
    [{author: user|agent, text, at_ms, delivered, proposal?: {edits:
    [{old_text, new_text}], state: pending|accepted|rejected|failed,
    error?}}]}]. *)
val review_comment : Review_comment.t -> Json.t

(** [{comments, approvals: [{root?, path, hash}], snapshots: [{root, path,
    action, by?, at_ms, content: {kind: text|absent|binary|too_large, md5?,
    bytes?}}]}] ([root] missing: approved before worktrees were told apart,
    in the session's repository; [snapshots] by file, each file's oldest
    first, without their texts). *)
val review : Review_state.t -> Json.t

(** [{path, index, snapshot, current, changed, added, removed, diff, note?}]:
    a snapshot and contents as in [review]'s [snapshots]. *)
val review_since : Review_state.Since.t -> Json.t

val model : Model.t -> Json.t

(** A job for [list_jobs]: id, command, running, exit, delivered, elapsed
    seconds, bytes and last output line. *)
val job : now:float -> Background_tasks.Task.t -> Json.t

(** With [repo] when it is known ([null]: not in a repository) and the
    worktrees made for its subagents. *)
val session_summary : Session.Summary.t -> Json.t

val session_stats : Agent.Session_stats.t -> Json.t
val entry : Session.Entry.t -> Json.t
val thinking : Thinking.t -> Json.t
val thinking_of_string : string -> Thinking.t Or_error.t
val auth_status : Provider_auth.Status.t -> Json.t

(** [("type", "event"); ("event", name); ...fields]. With [now] (seconds),
    the messages of [message_start] and [message_end] events, also nested in
    [subagent] ones, carry it as their ["at"]: a [message_end] is sent as its
    message is appended to the session, so it agrees with [get_messages]
    (and [get_subagent]) to within milliseconds. *)
val event : ?now:float -> Agent.Event.t -> Json.t

(** [("type", "event"); ("event", "auth"); ("kind", ...); ...fields]. *)
val login_event : Login_manager.Event.t -> Json.t

(** Times are milliseconds since the epoch ([*_ms]); [stale]: finished
    before the session's latest run started. *)
val subagent_summary : Subagent_log.Summary.t -> Json.t

(** [{"subagent": summary, "messages": transcript}]. *)
val subagent : Subagent_log.Summary.t * Timed_message.t list -> Json.t

(** [{"steer_texts", "follow_up_texts", "agents": [{agent_id, steer_texts,
    follow_up_texts}], "confirms": [{call_id, agent_id, name, summary,
    rules}], "questions": [{call_id, questions}]}]. *)
val pending
  :  steer:string list
  -> follow_up:string list
  -> agents:Subagent_inbox.Listing.t list
  -> confirms:Agent.Pending_confirm.t list
  -> questions:Agent.Pending_question.t list
  -> Json.t
