open! Core

(** What the terminal is sent for a notification ([/notify]): the bell, or a
    desktop notification as OSC 777 for terminals that only know that one
    (foot, urxvt) and OSC 9 for the rest (iTerm2, WezTerm, Ghostty, kitty,
    Windows Terminal, ...). Inside tmux a desktop notification is wrapped
    for passthrough (it needs [set -g allow-passthrough on]); the bell is
    tmux's own. *)
val sequence
  :  Prigh_ui.Notify.Mode.t
  -> term:string option (** [$TERM] *)
  -> tmux:bool (** [$TMUX] is set *)
  -> Prigh_ui.Notify.t
  -> string
