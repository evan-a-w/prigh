open! Core

(** A transcript line with its left gutter. The transcript is a work log: the
    user's turns carry a bar, the agent's steps hang off a two-column gutter
    that holds their marks (a tool's outcome), and their details are indented
    under them. Wrapping keeps the gutter: bars repeat, marks do not. *)

type t

val gutter_width : int

(** At the left edge, no gutter (the blank line before a turn). *)
val flush : Content.Line.t -> t

(** [mark] (one column) in the gutter of the first wrapped line. *)
val mark : Content.Span.t -> Content.Line.t -> t

(** [bar] (one column) in the gutter of every wrapped line. *)
val bar : Content.Span.t -> Content.Line.t -> t

(** Indented [depth] gutters: 1 aligns with a step's text, 2 is a step's
    details (tool output). *)
val indent : ?depth:int -> Content.Line.t -> t

val wrap : t list -> width:int -> Content.t
