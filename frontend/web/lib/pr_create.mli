open! Core
open! Import

(** The create-pull-request dialog ([/pr new [title]]): prefilled from
    [pr_defaults], then [pr_create]. A refusal stays in it with what was
    typed. *)

module Action : sig
  type t =
    | Title of string
    | Body of string
    | Base of string
    | Toggle_draft
    | Toggle_push
  [@@deriving sexp_of, equal]
end

type t =
  { defaults : Pr_defaults.t option (** [None] while it loads *)
  ; failed : string option (** [pr_defaults] could not say: nothing to do *)
  ; title : string
  ; body : string
  ; base : string
  ; draft : bool
  ; push : bool (** push the branch first *)
  ; busy : bool (** creating *)
  ; error : string option (** why it was refused, or what is missing *)
  ; back : Pull_requests.t option (** the list it was opened from *)
  }
[@@deriving sexp_of, equal]

(** Loading the defaults; [title], if not empty, is kept over theirs. *)
val create : ?back:Pull_requests.t -> title:string -> unit -> t

(** Fills what was not typed already; [push] is on when the branch is not
    all on the remote. *)
val set_defaults : t -> Pr_defaults.t -> t

val update : t -> Action.t -> t

(** The [pr_create] request, or why it can't be sent yet. *)
val request : t -> (Request.Method.t, string) Result.t
