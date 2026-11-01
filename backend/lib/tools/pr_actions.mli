open! Core
open! Import

(** The [pull_request] tool's actions beyond reading and opening pull
    requests: comments, comment, edit, merge, close, reopen, ready, draft,
    review, reply, request_reviewers. [None] for any other action. *)
val run
  :  forges:Forges.t
  -> git:Forges.git
  -> args:Json.t
  -> string
  -> string Or_error.t option
