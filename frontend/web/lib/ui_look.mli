(** The classes of render_ui's controls: loon's (shadcn's) components, with
    the hook classes the tests and the stylesheet select on. *)

(** A button: [primary] (the accent), [secondary] or [ghost]; [small]. *)
val button : ?small:bool -> [ `Primary | `Secondary | `Ghost ] -> string

val input : string
val textarea : string
val select : string

(** The calculator's number fields. *)
val calc_input : string
