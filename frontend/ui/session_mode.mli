open! Core
open! Import

(** What the status lines call the mode: the session's approval mode, or
    [plan] while in plan mode (which keeps the approval mode to return to),
    or [chat] while in chat mode (a chat without the tool host, which keeps
    the mode before it to return to). *)

type t =
  { approval : P.Approval_mode.t
  ; plan : bool
  ; chat : bool
  }
[@@deriving sexp_of, equal]

val of_state : P.State.t -> t

(** Alt+M: ask → auto-edit → smart → auto → plan → chat → ask. Entering plan
    or chat mode keeps the approval mode; leaving chat by cycling goes to
    [ask]. *)
val cycle : t -> t

(** ["ask"], ["auto-edit"], ["smart"], ["auto"], ["plan"], ["chat"]. *)
val label : t -> string

(** The modes [/mode] offers, in cycle order, as from [current]: the
    approval modes leave plan and chat mode, [plan] keeps the approval mode
    and leaves chat, [chat] keeps the mode to return to. *)
val choices : current:t -> t list

(** What [/mode NAME] means, as from [current]; [None] for an unknown name. *)
val of_name : current:t -> string -> t option

(** [t] out of chat mode: back to the mode before it ([/chat off]). *)
val leaving_chat : t -> t

(** What asks first, in a few words for a control: ["Asks: edits &
    commands"], ["Asks: commands"], ["Smart: a model decides"],
    ["Auto: never asks"], ["Plan mode"], ["Chat: no tool host"]. *)
val title : t -> string

(** How the user leaves plan mode (the model cannot): ["/plan off or
    Alt+M"]. *)
val leave : string

(** How the user leaves chat mode: ["/chat off or Alt+M"]. *)
val leave_chat : string

(** One line on what the mode does, for notices and pickers; plan and chat
    mode's say how to leave them ({!leave}, {!leave_chat}). *)
val describe : t -> string

(** [set_mode]'s parameters. *)
val params : t -> (string * P.Json.t) list

val names : string list
