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

(** A message's open context menu ({!Context_menu.Target.Message}), and
    its highlighted item if it can be chosen: a trim's highlight draws where
    it would cut, a delete's fades the message. *)
module Menu : sig
  type t =
    { entry : int
    ; highlighted : Context_menu.Item.t option
    }
end

(** A transcript as DOM: user messages, deliveries of background work,
    assistant text (markdown), thinking, tool cards (with subagents' nested
    transcripts), compactions, notices and how a reply stopped. Messages show
    when they were sent (replies: when they ended) and a separator marks
    where the day changes. Tool outputs fold as [fold] (default
    [Fold.default]) says. With [index], the user's messages and the replies
    of the context have a context menu: they are focusable and have a ⋯
    button ([more_id], with [menu_attribute]: the entry's number); [menu] is
    the open one. The entries in [deleting] are left out; [dividers] go
    after their entries, whatever the filter. What [Fold.filter] hides is
    left out, in subagents' transcripts too; a landmark of [Jump] or a
    message of the context left out keeps an empty [.filtered] with its
    attributes in its place (no menu). *)
val view
  :  ?fold:Fold.t
  -> ?menu:Menu.t
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

(** On every entry that is a message of the context: its number. *)
val message_attribute : string

(** On a message's ⋯ button: its entry's number. *)
val menu_attribute : string

(** The DOM id of entry [n]'s ⋯ button, which its menu opens under from the
    keyboard. *)
val more_id : int -> string

(** On a divider ({!Divider}) and its buttons: the range's {!Hidden.entry};
    the buttons also have [hidden_action_attribute]: [show], [hide] or
    [undo]. *)
val hidden_attribute : string

val hidden_action_attribute : string

(** A [data:] URL. *)
val image_src : Image.t -> string
