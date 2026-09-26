open! Core
open! Import

module Source : sig
  type t =
    | Command
    | Argument of Commands.Spec.t
    | Path
    | Directory of { host : string option }
    (** [/cd] and the [/host] directory prompt: listed by the backend on [host]
        (the active one when [None]) *)
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val source : t -> Source.t
val prefix : t -> string
val items : t -> Picker.Item.t list
val selected : t -> int

(** Whether Up/Down have been used; see [accepts_on_enter]. *)
val navigated : t -> bool

(** Enter accepts the highlighted item for commands, and for arguments and paths
    once the user has typed a filter or moved the highlight; before that Enter
    runs the command as typed (so [/model] Enter Enter opens the picker rather
    than silently picking the first model). *)
val accepts_on_enter : t -> bool

val set_items : t -> Picker.Item.t list -> t
val up : t -> t
val down : t -> t
val selected_item : t -> Picker.Item.t option

(** [compute ~line ~col ...] where [line] is the editor line under the cursor,
    [col] is the cursor's byte offset within it and [line_index] its index. *)
val compute
  :  line:string
  -> col:int
  -> line_index:int
  -> models:P.Model.t list
  -> auth:P.Auth_status.t list
  -> sessions:P.Session_summary.t list option
  -> logged_in:(string -> bool)
  -> t option

(** Directory completion for a whole prompt line (the [/host] directory
    question): the prefix is the entire text. *)
val directory : host:string -> text:string -> t

(** Replaces [prefix] (at its original offset in [editor_text]) with the
    selected item, or leaves the text unchanged when nothing is selected. *)
val accept : t -> editor_text:string -> string
