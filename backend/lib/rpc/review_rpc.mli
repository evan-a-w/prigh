open! Core
open! Import

(** The review's RPC methods ({!Review_state}), on the calling client's
    session. Changes are answered with the comment changed and broadcast to
    the session's clients as a [review] event with the whole review.

    - [get_review {}]: {!Rpc_json.review}.
    - [review_comment {root, path, first, last, anchor, text}]: a new
      comment on lines [first]-[last] of [path] (relative to [root], the
      repository's top directory, as [git_diff] says).
    - [review_reply {id, text}], [review_set_status {id, status:
      open|resolved}]: the comment.
    - [review_delete {id}]: [{}].
    - [review_submit {root?}]: [{count}], the comments sent to the agent
      (with [root]: those on that worktree only).
    - [review_decide {id, post, accept}]: accepts (applies) or rejects the
      proposal of the [post]th post; the comment.
    - [review_approve {root?, path, hash}], [review_unapprove {root?,
      path}]: the review. [root] is the worktree's top directory, as
      [git_diff] says; without it, the approval is of the session's
      repository (as before worktrees were told apart).
    - [review_since {root, path?, index?}]: [{files}], what changed since
      the last action on each file of the worktree [root] that has one (or
      on [path]; with [index], since that action of its history, oldest
      first), as {!Rpc_json.review_since}.

    Approving, unapproving, commenting, replying and deciding a proposal
    (also [fs_write] with [review_root]) record the file's
    {!Review_snapshot}, by the user [by]. *)

val methods : string list

(** [None] for other methods. *)
val dispatch
  :  ?by:string
  -> Agent.t
  -> meth:string
  -> params:Json.t
  -> Json.t Or_error.t option
