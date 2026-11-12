open! Core
open! Import

(** The settings page's Custom instructions section: the user's
    ({!Custom_instructions}) as [get_custom_instructions] and
    [custom_instructions_changed] said them, the one being written or
    edited, and the one whose removal waits for a yes. They belong to the
    user (switching starts over). *)

module Status = Memory_notes.Status
module Draft = Memory_notes.Draft

type t =
  { status : Status.t
  ; instructions : Custom_instructions.t
  ; draft : Draft.t option
  ; removing : string option (** the one asked about *)
  ; error : string option (** the last change that failed *)
  }
[@@deriving sexp_of, equal]

val empty : t
val loading : t -> t
val loaded : t -> (Custom_instructions.t, string) Result.t -> t

(** [custom_instructions_changed]: the draft of one that is gone is
    dropped. *)
val changed : t -> Custom_instructions.t -> t

(** About how many tokens they add to every session's context. *)
val tokens : t -> int

(** Past this many tokens the section warns that they cost every session
    context. *)
val warn_tokens : int

(** Edits [id] (or writes a new one, [None]). *)
val edit : t -> string option -> t

val set_title : t -> string -> t
val set_text : t -> string -> t
val cancel : t -> t

(** The draft to send ([Save_custom_instruction]), or why not (said under
    it). *)
val save : t -> t * Request.Method.t option

val saved : t -> (Custom_instructions.Note.t, string) Result.t -> t
val ask_remove : t -> string -> t
val cancel_remove : t -> t

(** [Remove_custom_instruction] of the one asked about. *)
val remove : t -> t * Request.Method.t option

val removed : t -> (unit, string) Result.t -> t
val failed : t -> string -> t
