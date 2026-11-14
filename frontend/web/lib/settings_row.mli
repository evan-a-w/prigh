open! Core
open! Import

(** The settings' building blocks, so that every section reads alike: a row
    per setting (its name, a line on what it does, its control at the end),
    rows in groups (a titled card), and the few controls they use. *)

(** Where a setting is kept, said beside its name when it is not where its
    section's others are. *)
module Scope : sig
  type t =
    | Browser (** this browser only *)
    | Account (** follows the user to every browser *)
    | Backend (** the backend's configuration *)
end

(** A setting: [name], [about] it in a line, a [note] under that ([level]
    ["warm"] or ["muted"]) and its [controls] at the end. [wide] puts the
    controls under the text, the row's full width (the theme's samples). *)
val row
  :  ?cls:string
  -> ?note:string * string
  -> ?scope:Scope.t
  -> ?wide:bool
  -> ?keys:string
       (** A shortcut after [about], hidden on phones and touch screens. *)
  -> name:string
  -> about:string
  -> Node.t list
  -> Node.t

(** Rows in a card, under [title] with [note] across from it. *)
val group : ?title:string -> ?note:string -> Node.t list -> Node.t

val switch
  :  ?disabled:bool
  -> label:string
  -> title:string
  -> on:bool
  -> on_click:unit Effect.t
  -> unit
  -> Node.t

(** A number with its [unit] after it (a field to type it in, applied on
    Enter or leaving it) and an On/Off switch; off hides the field.
    [invalid] marks the field (the row's note says why). *)
val number_switch
  :  ?disabled:bool
  -> id:string
  -> label:string
  -> unit:string
  -> value:string
  -> placeholder:string
  -> title:string
  -> switch_title:string
  -> invalid:bool
  -> on:bool
  -> on_input:(string -> unit Effect.t)
  -> on_commit:unit Effect.t
  -> on_switch:(bool -> unit Effect.t)
  -> unit
  -> Node.t

(** A choice made in a picker: the current [value] with a chevron, as a
    select box reads. *)
val select
  :  ?disabled:bool
  -> cls:string
  -> title:string
  -> label:string
  -> on_click:unit Effect.t
  -> string
  -> Node.t

(** A choice of a few, side by side: [(label, title, on, on_click)]. *)
val segmented
  :  ?disabled:bool
  -> label:string
  -> (string * string * bool * unit Effect.t) list
  -> Node.t

(** A segmented control whose choices are (name, a line about it, tooltip,
    current, on click): on a phone a list of rows, the line under each name;
    elsewhere the line is hidden. *)
val choice_list
  :  ?disabled:bool
  -> label:string
  -> (string * string * string * bool * unit Effect.t) list
  -> Node.t
