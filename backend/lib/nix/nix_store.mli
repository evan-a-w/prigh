open! Core
open! Import

(** The backend's view of the Nix store, for superusers ([nix_store_status],
    [nix_store_gc]): one garbage collection at a time, whoever asks. *)
type t

val create
  :  sw:Switch.t
  -> now:(unit -> float)
  -> ?progress_interval:float
       (** seconds between progress reports while only paths are being
           deleted (default 0.25) *)
  -> Nix_store_ops.t
  -> t

val status : t -> Nix_store_status.t Or_error.t

(** Deletes [generations] of every profile it may, then collects garbage,
    reporting progress to [on_progress] until it is done. Refused while
    another collection runs. The collection goes on if the caller is
    cancelled (e.g. its connection drops). *)
val gc
  :  t
  -> by:string
  -> Nix_generations.t option
  -> on_progress:(Nix_gc_progress.t -> unit)
  -> Nix_gc_report.t Or_error.t
