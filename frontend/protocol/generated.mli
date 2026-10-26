open! Core

(** [generate_text]: a field's text drafted by the model, streamed as
    [generate_delta] events and answered with the whole. *)

module Kind : sig
  type t =
    | Commit_message
    | Pr (** title on the first line, then the body *)
    | Branch_name
    | Session_title
  [@@deriving sexp_of, equal, enumerate]

  (** As the backend names it: [commit_message], [pr], ... *)
  val to_string : t -> string
end

type t =
  { gen_id : string
  ; text : string
  ; title : string option (** [Pr] *)
  ; body : string option (** [Pr] *)
  ; model : string
  ; cost_usd : float
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** A pull request's text so far (streamed or whole): its first line, and
    the rest. *)
val split_pr : string -> string * string
