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
    - [review_submit {}]: [{count}], the comments sent to the agent.
    - [review_decide {id, post, accept}]: accepts (applies) or rejects the
      proposal of the [post]th post; the comment.
    - [review_approve {path, hash}], [review_unapprove {path}]: the review. *)

val methods : string list

(** [None] for other methods. *)
val dispatch
  :  Agent.t
  -> meth:string
  -> params:Json.t
  -> Json.t Or_error.t option
