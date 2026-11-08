open! Core
open! Import

(** The settings page's Memory section: the user's notes ({!Memory}) as
    [get_memory] and [memory_changed] said them, the filter over them, the
    note being written or edited, and the one whose deletion waits for a
    yes. The notes belong to the user (switching starts over). *)

module Status : sig
  type t =
    | Unknown (** not asked *)
    | Loading
    | Loaded
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Draft : sig
  type t =
    { id : string option (** the note edited; [None]: a new one *)
    ; title : string
    ; text : string
    ; saving : bool (** sent, no reply yet *)
    ; error : string option (** why the backend refused it *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; memory : Memory.t
  ; filter : string
  ; draft : Draft.t option
  ; forgetting : string option (** the note asked about *)
  ; error : string option (** the last change that failed *)
  }
[@@deriving sexp_of, equal]

val empty : t
val loading : t -> t
val loaded : t -> (Memory.t, string) Result.t -> t

(** [memory_changed]: the draft of a note that is gone is dropped. *)
val changed : t -> Memory.t -> t

(** The notes shown: newest first, or the filter's best first. *)
val visible : t -> Memory.Note.t list

val set_filter : t -> string -> t

(** Edits note [id] (or writes a new note, [None]). *)
val edit : t -> string option -> t

val set_title : t -> string -> t
val set_text : t -> string -> t
val cancel : t -> t

(** The draft to send: [Save_memory]'s fields, or why not (said under it). *)
val save : t -> t * Request.Method.t option

val saved : t -> (Memory.Note.t, string) Result.t -> t
val ask_forget : t -> string -> t
val cancel_forget : t -> t

(** [Forget_memory] of the note asked about. *)
val forget : t -> t * Request.Method.t option

val forgotten : t -> (unit, string) Result.t -> t
val failed : t -> string -> t
