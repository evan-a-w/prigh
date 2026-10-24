open! Core

(** [/review]: a code review of the working tree's changes, as a run of the
    session's model with a review prompt (after Codex's review rubric) asking
    for prioritised findings with file and line. *)

module Target : sig
  type t =
    | Uncommitted
    | Base of
        { base : string
        ; merge_base : string
        }
  [@@deriving sexp_of, equal]

  (** [uncommitted changes], [changes against main]. *)
  val describe : t -> string
end

(** [/review ARGS]: the first word, which may name a base, and the rest. *)
val split_args : string -> string option * string

(** Diffs up to this many bytes are put in the prompt; larger ones are left
    for the model to read. *)
val inline_limit : int

val prompt
  :  target:Target.t
  -> instructions:string option
  -> Git_diff.Result.t
  -> string

(** The error when there is nothing to review. *)
val nothing_to_review : Target.t -> root:string -> string
