open! Core
open! Import

(** The quiet transcript's grouping: every run of consecutive steps that are
    not messages (thinking, tool calls, the provider's own tools, reports of
    background work and reminders) is one box, which messages (the user's,
    the model's text, a [render_ui] view, how a reply stopped) break. A run
    may span entries: its box shows where its first step is.

    Computed once per entry: a transcript's grouping is kept by the list cell
    of its newest entry ({!Chat.rev_entries}), so that a streamed chunk only
    regroups the newest entry. *)

module Step : sig
  type t =
    | Thinking of
        { n : int (** the entry *)
        ; index : int (** the block in its message *)
        ; text : string
        ; live : bool (** still streaming *)
        }
    | Call of
        { n : int
        ; call : Tool_call.t
        ; streaming : bool (** its arguments are still arriving *)
        ; at : Time_ns.t option (** when its message ended *)
        }
    | Server of
        { n : int
        ; index : int
        ; tool : Content.Server_tool.t
        ; streaming : bool
        }
    | Report of
        { n : int
        ; user : Message.User.t
        }

  (** What toggles its row ({!Fold}): a call's {!Tool_view.open_key}. *)
  val key : t -> string

  val phrase : t -> Prigh_ui.Tool_phrase.Step.t
  val kind : t -> Transcript_filter.Kind.t
end

module Box : sig
  type t

  (** What toggles it ({!Fold}): ["run:"] and its first step's key, so that
      it stays open as steps are added. *)
  val key : t -> string

  (** Oldest first. *)
  val steps : t -> Step.t list

  (** When the message before it was sent (or ended). *)
  val since : t -> Time_ns.t option
end

module Segment : sig
  type t =
    | Whole (** the entry shows as it is *)
    | Blocks of int list (** these blocks of its message, in order *)
    | Box of Box.t (** a run starts here *)
end

type t

(** The grouping of [chat]'s loaded entries. *)
val of_chat : Chat.t -> t

(** What the entry [n] shows, in order: nothing when all of it went into a
    box started before it. *)
val segments : t -> int -> Segment.t list

(** The box still taking steps: the newest thing in the transcript is a
    step of it. *)
val tail : t -> Box.t option

(** Whether the newest box started before entry [n] and is still taking
    steps after it. *)
val continues : t -> int -> bool

val equal_segments : Segment.t list -> Segment.t list -> bool
