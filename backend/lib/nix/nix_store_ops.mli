open! Core
open! Import

(** How the backend reaches the Nix store, faked in tests.

    [local] runs nix as the backend's own user: [nix store gc] deletes what
    no GC root keeps (through the daemon in a multi-user install), and
    deleting old generations ([nix-collect-garbage]) reaches only that
    user's profiles. [helper] asks the container's root helper (see
    DOCKER.md, "Garbage collection") through the FIFO [DIR/requests], which
    only the backend's user can write and which accepts only
    [gc ID GENERATIONS BY]; the helper deletes every user's old generations
    as that user, then collects as root, and writes its progress to
    [DIR/ID.log] and its outcome to [DIR/ID.result]. The helper also keeps
    [DIR/last-gc], [DIR/running] and [DIR/schedule]. *)
type t =
  { check : unit -> unit Or_error.t
    (** whether there is a store to manage, else what to do *)
  ; store : unit -> Nix_store_status.Store.t Or_error.t
  ; last_gc : unit -> Nix_gc_report.t option (** recorded by the helper *)
  ; running : unit -> Nix_store_status.Running.t option
    (** a collection the backend did not start: the helper's schedule *)
  ; schedule : unit -> Nix_store_status.Schedule.t option
  ; collect :
      by:string
      -> Nix_generations.t option
      -> on_line:(string -> unit)
      -> Nix_gc_report.t Or_error.t
    (** passes on Nix's output lines as they come *)
  }

val local : env:Env.t -> t
val helper : env:Env.t -> dir:string -> t

(** [nix path-info --all -s]'s output: the number of paths and their total
    size. *)
val parse_path_info : string -> (int * int) Or_error.t

(** [df -Pk]'s output: total and available bytes. *)
val parse_df : string -> (int * int) Or_error.t
