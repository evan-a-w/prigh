open! Core
open! Import

(** [/pr new]'s dialog: a pull request's title, body and base, whether it
    is a draft and whether to push the branch first, prefilled from
    [pr_defaults]. Typing goes to the focused field (at its end); Space on
    a checkbox toggles it. *)

module Field : sig
  type t =
    | Title
    | Body
    | Base
    | Draft
    | Push
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of, equal]

(** [title] (typed after [/pr new]) replaces the default one; push first is
    on when the branch is not on the remote with all its commits. *)
val create : ?title:string -> P.Pr_defaults.t -> t

val defaults : t -> P.Pr_defaults.t
val focus : t -> Field.t
val title : t -> string
val body : t -> string
val base : t -> string
val draft : t -> bool
val push : t -> bool
val error : t -> string option

(** Whether it was sent and the dialog waits for the answer. *)
val submitting : t -> bool

val insert : t -> string -> t

(** Ctrl+J: a new line in the body. *)
val newline : t -> t

val backspace : t -> t

(** Empties the focused field. *)
val clear : t -> t

(** Deletes the focused field's last word. *)
val kill_word : t -> t

(** Tab and Down: the next field; Shift+Tab and Up: the previous one. *)
val next : t -> t

val prev : t -> t

(** Enter: the [pr_create] request (the form now [submitting]), unless the
    title is empty. *)
val submit : t -> [ `Wait of t | `Create of t * P.Request.Method.t ]

(** The backend refused: shown, the form editable again. *)
val failed : t -> error:string -> t
