open! Core
open! Import

(** What a tool host is known by ([hello]'s [host_id]): one per machine and
    user, kept in [~/.prigh/host-id] of the home its tools see, so sessions
    follow a host across reconnects and restarts. The TUI keeps the same
    file ([tui/client_unix/host_id.ml]), so a TUI and [prigh tool-host
    -connect] on one machine are the same host. The file may be edited to
    choose an id. *)

(** [<home>/.prigh/host-id] *)
val file : home:string -> string

(** A fresh random id, [host-] and 16 hex digits. *)
val generate : unit -> string

(** The id in {!file}, created with a {!generate}d one if missing or empty.
    Processes starting at once agree on one id. *)
val load_or_create : home:string -> string Or_error.t

(** [given] (an explicit [-host-id]) if any, else {!load_or_create}'s; when
    the file cannot be kept, a {!generate}d one, after telling [warn] why. *)
val choose
  :  ?given:string
  -> home:string
  -> warn:(string -> unit)
  -> unit
  -> string
