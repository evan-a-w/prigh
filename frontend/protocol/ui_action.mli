open! Core

(** The user's answer through a [render_ui] component (a form, choices, a
    checklist with a submit button): the [ui_action] parameter of [prompt]
    and [follow_up]. The backend turns it into a user message whose text
    is {!message_text}, with the origin kind [Ui_action]. *)

type t =
  { call_id : string (** the [render_ui] call *)
  ; component : string (** the component's id *)
  ; title : string (** its title, prompt or id *)
  ; values : Json.t
  }
[@@deriving sexp_of, equal]

val to_json : t -> Json.t

(** [render_ui call CALL, component COMPONENT]: how the message names the
    component it answers. *)
val reference : call_id:string -> component:string -> string

(** The text the model sees, as the backend writes it:
    {v The user responded through "<title>" (render_ui call <call_id>, component <component>):
<values as compact JSON> v} *)
val message_text : t -> string

module Sent : sig
  type t =
    { call_id : string
    ; component : string
    ; values : Json.t option (** [None] when they do not parse *)
    }
  [@@deriving sexp_of]

  (** Reads a user message's text written by {!message_text}. *)
  val of_text : string -> t option
end
