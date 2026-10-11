open! Core
open! Import

(** What both frontends show for the harness's additions to user messages
    ([P.Origin]): environment notes, attached files and the messages it
    writes in the user's place (reports, hand-overs, goals, [/review], ...)
    are quiet notes with a glyph, never the user's own words. *)

(** One character standing for the kind of note: [⇄] tools moved, [⌂] the
    directory, [✎] the editor, [↩] reports, [↪] a hand-over, [◎] a goal,
    [◷] a goal's continuation, [↻] a resume, [$] shell output, [≡] the
    summary, [⚑] a review, [▤] [/init]. *)
val glyph : P.Origin.Kind.t -> string

(** What the user typed: [P.Message.User.body], a skill's expansion as
    [/skill:NAME ARGS]; empty for a message of the harness's. *)
val typed : P.Message.User.t -> string

(** A line for lists ([/fork], [/tree]): the glyph and title of a message of
    the harness's, else the first line of what the user typed (or its first
    image). *)
val label : P.Message.User.t -> string
