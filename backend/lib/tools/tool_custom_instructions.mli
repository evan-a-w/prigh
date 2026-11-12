open! Core
open! Import

(** [custom_instructions]: the user's standing instructions, in the context
    of every session ({!Custom_instructions}): [add] (text, optional title),
    [edit] (id, text and/or title), [remove] (id), [list]. Runs in the
    backend on the world's; only the main agent has it, and only while the
    user lets the model change them ({!Custom_instructions.enabled}). Every
    change but [list] asks in ask mode, as [memory]'s do. A change is
    emitted as [Custom_instruction_set], so that the session's model is not
    told back what it did. Its description tells the model to keep them for
    lasting preferences and standing instructions. *)

val name : string
val spec : Tool_spec.t

(** Whether [action] changes them (approvals). *)
val changes : string -> bool

(** [now] (default: the clock) dates the notes. While the user does not let
    the model change them every call is refused. *)
val create
  :  ?now:(unit -> Time_float.t)
  -> custom_instructions:Custom_instructions.t
  -> unit
  -> Tool.t
