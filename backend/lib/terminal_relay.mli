open! Core
open! Import

(** Browser terminals whose shell runs on a tool-host client. The browser's
    frames go to the host as events:
    - [{"type":"event","event":"terminal_open","host","term_id","key","cwd",
      "cols","rows"}]
    - [{"type":"event","event":"terminal_frame","host","term_id","kind",
      "data"}]
    - [{"type":"event","event":"terminal_close","host","term_id"}] when the
      browser went away;

    and the host answers with [terminal_frame] ([term_id], [kind], [data])
    and [terminal_closed] ([term_id]) requests. Frames carry [kind] ["binary"]
    (base64 [data]) or ["text"]. *)

type t

val create : unit -> t

(** Relays [channel] to the host until either side closes. [send_event]
    reaches the host client while it is connected. *)
val serve
  :  t
  -> host:string
  -> send_event:(Json.t -> unit)
  -> key:string
  -> cwd:string
  -> cols:int
  -> rows:int
  -> Terminal_channel.t
  -> unit

(** A [terminal_frame] request from [client], which must own the terminal. *)
val frame : t -> client:string -> Json.t -> unit Or_error.t

(** A [terminal_closed] request from [client]. *)
val closed : t -> client:string -> Json.t -> unit Or_error.t

(** Closes the browser side of every terminal relayed to [host]. *)
val host_gone : t -> host:string -> unit
