open! Core
open! Import

(** The session's status wherever the user's {!Status_style} puts it, from
    the same pieces ({!Status_view}) and with the same details card:

    - {b island}: a translucent pill at the top of the chat ({!island});
    - {b ring}: a ring round the send button ({!around_send}), the thinking
      level and the mode beside the host ({!row_chips});
    - {b whisper}: one quiet line under the composer ({!under_composer});
    - {b border}: the composer's bottom edge as the meter, a tag at its end
      ({!in_composer}), the thinking level and the mode beside the host;
    - {b hub}: the model chip's figures ({!model_badge}) and menu;
    - {b sidebar}: a card at the foot of the docked sidebar
      ({!sidebar_card}), else the whisper's line. *)

(** Classes on the composer for the style: [status-ring], [status-border]...
    and [busy] while the agent works. *)
val composer_classes : App.Model.t -> (string * bool) list

(** E: the pill over the top of the chat (activity, context, cost, thinking,
    mode), glowing while the agent works; the details below it. *)
val island : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** A: [send] inside the context's ring, which turns while the agent works;
    the details above it. Other styles: [send] as it is. *)
val around_send
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
  -> Node.t

(** A and C: the thinking level and the mode after the host chip (A leaves
    out the mode while it is [ask]). *)
val row_chips : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** C: the meter along the composer's bottom edge and its tag. Also the
    activity, for screen readers, where the style shows none (the hub). *)
val in_composer
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** B, and F without the docked sidebar: the line under the composer. *)
val under_composer
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** D: what the model chip shows after the model's name: the context's
    little ring and percent, and the mode's shield unless it is [ask]. *)
val model_badge : App.Model.t -> Node.t

(** F: the card at the foot of the docked sidebar. *)
val sidebar_card
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** Where the thinking level's and the mode's chips are live under their
    menu's backdrop: the sidebar's card (F, docked). *)
val chips_in_sidebar : App.Model.t -> bool
