open! Core
open! Import

(** The user's answer through an interactive [render_ui] component (a form,
    choices, a checklist with [submit]): the [ui_action] param of the
    [prompt] and [follow_up] RPCs. It becomes a user message of the note
    {!note} alone, whose text the model gets is exactly

    {v
The user responded through "<title>" (render_ui call <call_id>, component <component>):
<values as compact JSON>
    v} *)

type t =
  { call_id : string
  ; component : string
  ; title : string
  ; values : Json.t
  }
[@@deriving sexp_of]

(** [{call_id, component, title, values}]; [title] may be missing (then the
    component's id). Ids with control characters are refused; the title's
    runs of whitespace and control characters become one space. *)
val of_json : Json.t -> t Or_error.t

(** The message's text, as above. *)
val text : t -> string

(** One line for the user, e.g. [Sent "Deploy options": env = prod,
    replicas = 3]; at most 160 bytes. *)
val summary : t -> string

val note : t -> Injected.Note.t

(** Fails unless [call_id] is a [render_ui] call in [messages] whose result
    is not an error and that has an interactive component (a form, choices,
    a checklist with [submit]) with the id [component], saying which there
    are. *)
val check : t -> Message.t list -> unit Or_error.t
