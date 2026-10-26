open! Core
open! Import

(** A file of a reviewed worktree as it was when the user acted on it in the
    review (approved it, commented, accepted a proposal, saved it in the
    review's editor...), so the review can show what changed in it since
    ({!Review_state.since}). The text itself is kept once per distinct
    content in the review's blobs, by its MD5. *)

module Action : sig
  type t =
    | Approved
    | Unapproved
    | Commented
    | Replied
    | Accepted (** a proposal of the agent's: after it was applied *)
    | Rejected
    | Edited (** saved in the review's editor *)
  [@@deriving sexp, jsonaf, equal, enumerate]

  (** [approved], [commented], ... *)
  val to_string : t -> string
end

module Content : sig
  (** What the file held. *)
  type t =
    | Text of { md5 : string } (** its bytes' digest (hex), the blob's key *)
    | Absent (** no such file *)
    | Binary of { md5 : string } (** not kept *)
    | Too_large of { bytes : int } (** more than {!max_bytes}: not kept *)
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { root : string (** the worktree's top directory *)
  ; path : string (** relative to [root] *)
  ; action : Action.t
  ; by : string option (** the user who acted, when signed in *)
  ; at : float
  ; content : Content.t
  }
[@@deriving sexp, jsonaf, equal]

(** Files larger than this (1 MiB) are not kept. *)
val max_bytes : int

(** A file's bytes as read: [Content.t] and the text to keep, if any. *)
val content_of : string option -> size:int -> Content.t * string option

val md5 : string -> string
