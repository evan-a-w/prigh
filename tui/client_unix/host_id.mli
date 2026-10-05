open! Core

(** Our tool host's id ([hello]'s [host_id]): one per machine and user, kept
    in [~/.prigh/host-id], so that the backend knows this machine again when
    the TUI reconnects or is restarted, and its sessions follow it back. The
    file is in the home of the TUI, which is also the home of the
    [prigh tool-host] worker it spawns (the home the tools see); it is the
    same file [prigh tool-host -connect] uses, so a TUI and a standalone
    tool host of one user on one machine are one host (as are two TUIs: the
    newest connection holds the id). The file may be edited to choose an id. *)

(** [<home>/.prigh/host-id] *)
val file : home:string -> string

(** A fresh random id, [host-] and 16 hex digits. *)
val generate : unit -> string

(** The id in {!file}, created with a {!generate}d one if missing or empty.
    Processes starting at once agree on one id. *)
val load_or_create : home:string -> string Or_error.t

(** {!load_or_create}'s id; when the file cannot be kept, a {!generate}d one,
    after telling [warn] why. *)
val choose : home:string -> warn:(string -> unit) -> string
