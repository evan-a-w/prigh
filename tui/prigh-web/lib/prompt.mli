open! Core
open! Import

(** A dialog asking for a path ([/cd], [/host]'s directory, [/export],
    [/import]): the backend lists completions as you type, Tab takes the
    highlighted one, and a failure (e.g. no such directory) is shown in the
    dialog, which stays open so that the answer can be fixed. *)

module Action : sig
  type t =
    | Cd
    | Host_cwd of
        { host : string (** its id *)
        ; name : string
        }
    | Export
    | Import
  [@@deriving sexp_of, equal]
end

type t =
  { action : Action.t
  ; input : string
  ; suggestions : string list
  ; selected : int option (** the highlighted suggestion *)
  ; error : string option
  ; busy : bool (** submitted: waiting for the backend *)
  }
[@@deriving sexp_of, equal]

val create : ?input:string -> Action.t -> t
val set_input : t -> string -> t

(** The RPC that lists completions for the input: [list_dirs] on the host the
    directory is for, or [list_paths]. *)
val listing : t -> active_host:string -> string * (string * Json.t) list

(** The listing for [prefix]; stale ones are ignored. *)
val set_suggestions : t -> prefix:string -> string list -> t

val move : t -> int -> t

(** The highlighted suggestion (else the first) in place of the input. *)
val complete : t -> t
