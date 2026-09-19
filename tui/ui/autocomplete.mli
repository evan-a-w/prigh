open! Core
module P = Prigh_protocol

module Source : sig
  type t =
    | Command
    | Argument of Commands.Spec.t
    | Path
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val source : t -> Source.t
val prefix : t -> string
val items : t -> Picker.Item.t list
val selected : t -> int
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

(** Replaces [prefix] (at its original offset in [editor_text]) with the
    selected item, or leaves the text unchanged when nothing is selected. *)
val accept : t -> editor_text:string -> string
