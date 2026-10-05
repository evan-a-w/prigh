open! Core

(** What a key does. The page listens on the whole document and asks [handle]:
    dialogs own the keyboard, then the completion popup, then the editor; a few
    shortcuts work everywhere. *)

module Target : sig
  type t =
    | Editor of { cursor : int } (** the prompt editor, with its caret *)
    | Field (** another text field *)
    | Control (** a button, link or fold: Enter and Space are its own *)
    | Page
  [@@deriving sexp_of]
end

type t =
  { key : string (** [KeyboardEvent.key] *)
  ; code : string (** [KeyboardEvent.code], e.g. [KeyP] *)
  ; shift : bool
  ; alt : bool
  ; ctrl : bool
  ; meta : bool
  ; selection : bool (** text is selected (so Ctrl+X cuts it) *)
  ; target : Target.t
  }
[@@deriving sexp_of]

(** [None]: the browser's default (typing, moving the caret). *)
val handle : App.Model.t -> t -> App.Action.t option

(** The bindings, for [/help]. *)
val help : (string * string) list

(** The TUI's keys that the browser keeps, and what to use instead. *)
val browser : (string * string) list
