open! Core
open! Import

(** Session links in what the page draws: a link to [?session=ID]
    ({!Prigh_ui.Session_link}) is an [a.session-link] with [id_attribute],
    which the page's listener follows in place (a modifier or middle click:
    in a new window). Ids of the user's sessions in text and tool output
    become such links too.

    Which sessions those are is the page's session list, set by the view
    before it draws ({!set}); what is drawn with it (cached parts, entries)
    must be drawn again when its {!Prigh_ui.Session_link.Known.stamp}
    changes. *)

val set : Session_summary.t list -> unit
val known : unit -> Prigh_ui.Session_link.Known.t

(** [Known.stamp (known ())] *)
val stamp : unit -> int

(** On a session link: the id (or prefix) it goes to. *)
val id_attribute : string

(** A link to session [id] around [children]; its tooltip is the session's
    title when it is known. *)
val anchor : string -> Node.t list -> Node.t

(** [text] with the ids of known sessions made links ({!Prigh_ui.Session_link.ids}). *)
val linkify : string -> Node.t list

(** [text] as a link when it is all a known session's id or prefix (inline
    code that names one). *)
val whole : string -> Node.t option

(** [f ()] with [known] as the sessions: drawing later (a lazy node) what
    was keyed by its stamp. *)
val within : Prigh_ui.Session_link.Known.t -> (unit -> 'a) -> 'a
