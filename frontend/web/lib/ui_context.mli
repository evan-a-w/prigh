open! Core
open! Import

(** What a [render_ui] call's components render with: its id, the page's
    {!Ui_state}, the replies already in the transcript and the way to tell
    the page what the user did. *)

module Page : sig
  type t =
    { state : Ui_state.t
    ; inject : Ui_event.t -> unit Effect.t
    ; sent : Ui_action.Sent.t list (** the transcript's replies *)
    }

  (** No state, and nothing to tell (subagents' transcripts, tests). *)
  val none : t
end

type t =
  { call_id : string
  ; state : Ui_state.t
  ; sent : Ui_action.Sent.t list (** this call's replies in the transcript *)
  ; inject : Ui_event.t -> unit Effect.t
  ; fold : Fold.t
  }

val create : Page.t -> call_id:string -> fold:Fold.t -> t

(** [path]'s piece of state [name]. *)
val get : t -> path:string -> string -> string option

val set : t -> path:string -> string -> string -> unit Effect.t

(** The replies sent through [render_ui] components: in user messages the
    backend made of a [ui_action] (origin kind [Ui_action]). *)
val sent_of_messages : Message.User.t list -> Ui_action.Sent.t list

(** The values sent through [component], from the transcript or on their way;
    [None] while it has not been sent. *)
val sent_values : t -> component:string -> Json.t option

(** [text] (inline markdown) as DOM, without the paragraph's margins. *)
val md : ?cls:string -> string -> Node.t
