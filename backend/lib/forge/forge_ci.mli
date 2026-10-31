open! Core
open! Import

(** The repository itself, and its CI. GitHub has Actions' REST API (runs,
    their jobs and steps, logs, re-runs). Forgejo's CI is mostly commit
    statuses (see {!Pr_checks}), often from an outside CI whose page they
    link to; its Actions API lists runs ([/actions/tasks], newer versions)
    but has no jobs, logs or re-runs, so those say where to look instead. *)

module Info : sig
  type t =
    { full_name : string
    ; description : string
    ; default_branch : string
    ; private_ : bool
    ; fork : bool
    ; archived : bool
    ; stars : int
    ; forks : int
    ; open_issues : int
    ; url : string
    ; homepage : string
    ; readme : string option (** cut to a few KB *)
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

val view : Forge_api.t -> Forge_repo.t -> Info.t Or_error.t

module Run : sig
  type t =
    { id : int
    ; name : string
    ; title : string
    ; status : string
      (** GitHub's [queued], [in_progress], [completed]; Forgejo's state *)
    ; conclusion : string option (** [success], [failure], ... when completed *)
    ; branch : string
    ; sha : string
    ; event : string
    ; url : string
    ; created_at : string
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

module Job : sig
  type t =
    { id : int
    ; name : string
    ; status : string
    ; conclusion : string option
    ; url : string
    ; steps : (string * string) list (** name, conclusion or status *)
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

(** Newest first. *)
val runs
  :  Forge_api.t
  -> Forge_repo.t
  -> branch:string option
  -> limit:int
  -> Run.t list Or_error.t

(** A run and its jobs (GitHub). *)
val run : Forge_api.t -> Forge_repo.t -> int -> (Run.t * Job.t list) Or_error.t

(** The end of a job's log, at most [max_bytes] (GitHub); [true] when cut. *)
val job_log
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> max_bytes:int
  -> (string * bool) Or_error.t

(** Re-runs a run, or only its failed jobs (GitHub). *)
val rerun
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> failed_only:bool
  -> unit Or_error.t
