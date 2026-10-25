open! Core
open! Import

(** The theme and accent drawn small: in the [/theme] and [/accent] pickers
    and the settings' appearance section. *)

(** A dot of the accent's colour (on the theme on show). *)
val swatch : Theme.Accent.t -> Node.t

(** A miniature of the page in [name]'s colours, with the accent on show. *)
val sample : Theme.Name.t -> Node.t

(** The settings' rows: the themes as a choice of three samples, the accent
    as swatches and a colour input for any other. *)
val rows : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t list
