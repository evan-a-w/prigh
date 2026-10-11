open! Core

(** The model's task plan ([update_plan]), as the state carries it. *)

module Status : sig
  type t =
    | Pending
    | In_progress
    | Completed
  [@@deriving sexp_of, equal]
end

module Step : sig
  type t =
    { step : string
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

type t =
  { explanation : string option
  ; steps : Step.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** [update_plan]'s arguments, as a tool call carries them; [None] for an
    empty plan (it clears it). *)
val of_arguments : Json.t -> t option Or_error.t

val completed : t -> int

(** The step in progress, else the first pending one. *)
val current : t -> Step.t option
