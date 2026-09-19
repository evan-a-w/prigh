open! Core

(** A small VT100/xterm emulator over the byte stream a terminal would receive.
    It is deliberately permissive: unknown escape sequences are skipped, and
    [feed] never raises. Tests feed it Notty's output and compare the resulting
    grid with the pure renderer. *)

type t

val create : width:int -> height:int -> t

(** Crops or pads the grid, as a terminal does for an alternate-screen app. *)
val resize : t -> width:int -> height:int -> unit

(** Consume a chunk of terminal output. Styling, cursor moves, erases, OSC 8
    hyperlinks and the alternate screen are tracked. *)
val feed : t -> string -> unit

(** [height] rows, right-trimmed, with the cursor drawn as [▏] at its column
    when visible; matches [Screen.to_plain ~show_cursor:true]. *)
val to_plain : t -> string

(** Like [Content.to_styled]: style runs become [[red]…[/]], [[bold]],
    [[link=url]], …. *)
val to_styled : t -> string
