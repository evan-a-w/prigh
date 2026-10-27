open! Core
open! Import

(** What a trim after a message keeps out of the context, as a divider in
    the transcript. *)
module Divider : sig
  type t =
    { after : int (** the entry it follows ({!Message_index.after}) *)
    ; hidden : Hidden.t
    ; shown : Trimmed.Shown.t (** the messages below it, when shown *)
    ; restoring : bool (** its Undo is on its way *)
    }
end

(** A transcript as DOM: user messages, deliveries of background work,
    assistant text (markdown), thinking, tool cards (with subagents' nested
    transcripts), compactions, notices and how a reply stopped. Messages show
    when they were sent (replies: when they ended) and a separator marks
    where the day changes. Tool outputs fold as [fold] (default
    [Fold.default]) says. With [trim_after], the user's messages numbered
    after it have "Trim everything before this" ([trim_attribute]). With
    [index], the messages of the context have "Delete"
    ([delete_attribute]) and, but the newest, "Trim everything after this"
    ([trim_after_attribute]). The entries in [deleting] are left out;
    [dividers] go after their entries. *)
val view
  :  ?fold:Fold.t
  -> ?trim_after:int
  -> ?index:Message_index.t
  -> ?deleting:Int.Set.t
  -> ?dividers:Divider.t list
  -> Message_time.t
  -> Chat.t
  -> Node.t

(** The attributes of a message's "Trim everything before this", "Delete"
    and "Trim everything after this": the entry's number. *)
val trim_attribute : string

val delete_attribute : string
val trim_after_attribute : string

(** On every entry that is a message of the context: its number. *)
val message_attribute : string

(** On a divider ({!Divider}) and its buttons: the range's {!Hidden.entry};
    the buttons also have [hidden_action_attribute]: [show], [hide] or
    [undo]. *)
val hidden_attribute : string

val hidden_action_attribute : string

(** A [data:] URL. *)
val image_src : Image.t -> string
