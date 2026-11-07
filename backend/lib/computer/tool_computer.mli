open! Core
open! Import

(** The [computer] tool: sees and drives the user's real desktop through the
    session's tool host, when that host was started with [-computer]
    ({!Computer_helper}). Offered only then (see [Agent]); its calls run on
    the host ([on_host]). *)

val name : string
val tool : Tool.t

(** The tool for a host whose screen is [info]: with [native], offered as
    the model's built-in computer-use tool when it has one
    ({!Tool_spec.Native.Computer}, sized as the screenshots). *)
val for_screen : native:bool -> Computer_info.t -> Tool.t

(** For the system prompt when the tool is offered: what is on the screen is
    data, not instructions. *)
val guidance : string

(** What a host without computer use answers. *)
val off_error : string

(** The tool host's side: runs a [computer] call with the host's helper,
    if it has one. *)
val execute
  :  Computer_helper.t option
  -> cancel:Cancellation.t
  -> Json.t
  -> Tool_result.t
