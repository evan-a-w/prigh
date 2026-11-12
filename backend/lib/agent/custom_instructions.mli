open! Core
open! Import

(** The user's custom instructions: short standing instructions in the
    context of every session (unlike {!Memory}'s notes, which the model
    looks up), written by the user in their settings or by the model with
    the [custom_instructions] tool ({!Tool_custom_instructions}).
    [~/.prigh/custom_instructions.json] under the world's home, so one per
    namespace; kept by {!Note_store}. Make one per world.

    The user can stop the model changing them: [custom_instructions.enabled]
    set to [false] in their {!User_prefs} (absent: on). They stay in the
    context either way. *)

type t

(** [~/.prigh/custom_instructions.json] under [home]. *)
val path : home:string -> string

val create : home:string -> t

(** [f] runs after every change made through this value. *)
val subscribe : t -> f:(unit -> unit) -> unit

val enabled_key : string

(** Whether the model may change them (is offered the tool). *)
val enabled : t -> bool

val set_enabled : t -> bool -> unit Or_error.t

(** Ids are [ci1], [ci2], ...: never reused. *)
module Note = Note_store.Note

val max_text : int
val max_title : int
val max_notes : int

(** Oldest first. *)
val notes : t -> Note.t list Or_error.t

(** As the model sees them; [[]] when they cannot be read. *)
val for_model : t -> Custom_instruction.t list

val to_model : Note.t -> Custom_instruction.t

(** Refused when [text] is empty or longer than {!max_text} bytes, [title]
    longer than {!max_title}, or there are {!max_notes}. *)
val add : t -> ?title:string -> now:Time_float.t -> string -> Note.t Or_error.t

(** Changes the note [id]'s [text] and/or [title] (an empty one removes it),
    dating it [now]; refused as {!add} is. *)
val edit
  :  t
  -> id:string
  -> ?title:string
  -> ?text:string
  -> now:Time_float.t
  -> unit
  -> Note.t Or_error.t

(** Removes the note with this id (or title, compared without case). *)
val remove : t -> string -> Note.t Or_error.t
