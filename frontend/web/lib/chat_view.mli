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
    [Fold.default]) says. With [index], the messages of the context have a
    row of actions: "Trim everything before this" ([trim_attribute]; on the
    user's messages and the replies numbered after [first], the entry the
    context starts with), "Trim everything after this"
    ([trim_after_attribute]; but on the newest) and "Delete"
    ([delete_attribute]). The entries in [deleting] are left out; [dividers]
    go after their entries, whatever the filter. What [Fold.filter] hides is
    left out, with its actions, in subagents' transcripts too; a landmark of
    [Jump] or a message of the context left out keeps an empty [.filtered]
    with its attributes in its place. *)
val view
  :  ?fold:Fold.t
  -> ?first:int
  -> ?index:Message_index.t
  -> ?deleting:Int.Set.t
  -> ?dividers:Divider.t list
  -> Message_time.t
  -> Chat.t
  -> Node.t

(** How many parts of the loaded entries [Fold.filter] hides: messages,
    blocks of replies, how replies stopped. The entries in [deleting] do not
    count. *)
val hidden : ?deleting:Int.Set.t -> fold:Fold.t -> Chat.t -> int

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
