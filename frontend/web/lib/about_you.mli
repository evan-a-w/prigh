open! Core
open! Import

(** The settings page's About you section: what the user wrote about
    themselves for the model ({!User_info}), as [get_user_info] and
    [user_info_changed] said it, and the text being edited. It belongs to the
    user (switching starts over). *)

module Status : sig
  type t =
    | Unknown (** not asked *)
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; info : User_info.t (** as the backend has it *)
  ; draft : string option (** typed and not saved; [None]: as saved *)
  ; saving : bool (** sent, no reply yet *)
  ; saved : bool (** just saved: said until the next edit *)
  ; elsewhere : bool
    (** it changed on the backend (another browser, the file) under the draft *)
  ; error : string option (** why it was not saved, and what to do *)
  }
[@@deriving sexp_of, equal]

val empty : t
val loading : t -> t
val loaded : t -> (User_info.t, string) Result.t -> t

(** [user_info_changed]: shown at once unless a draft differs from it. *)
val changed : t -> User_info.t -> t

(** The text in the box. *)
val text : t -> string

val dirty : t -> bool
val set_text : t -> string -> t

(** Drops the draft for what the backend has. *)
val revert : t -> t

(** [Set_user_info] of the draft, or why not (said under the box). *)
val save : t -> t * Request.Method.t option

val was_saved : t -> (User_info.t, string) Result.t -> t
