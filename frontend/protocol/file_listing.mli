open! Core

(** [fs_list]'s reply: a directory on the session's active tool host (the
    file browser). *)

module Kind : sig
  type t =
    | File
    | Dir
    | Link of [ `File | `Dir | `Missing ] (** a symlink, by what it points to *)
    | Other (** devices, sockets, pipes *)
  [@@deriving sexp_of, equal]

  (** Directories and links to them: opening one lists it. *)
  val is_dir : t -> bool
end

module Entry : sig
  type t =
    { name : string
    ; kind : Kind.t
    ; size : int option (** files (and links to files) only *)
    ; mtime : Time_ns.t option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { path : string (** absolute, symlinks resolved *)
  ; parent : string option (** [None] at [/] *)
  ; home : string (** the host's [~], for showing paths *)
  ; entries : Entry.t list (** at most 5000, by name *)
  ; total : int (** all the entries shown, also those beyond the 5000 *)
  ; hidden : int (** dot files left out *)
  ; select : string option (** listed for this file in it *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
