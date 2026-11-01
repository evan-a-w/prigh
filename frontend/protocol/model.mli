open! Core

module Cost : sig
  type t =
    { input : float
    ; output : float
    ; cache_read : float
    }
  [@@deriving sexp_of, equal]
end

(** The largest image the model accepts, as the backend knows it. *)
module Image_limit : sig
  type t =
    { max_bytes : int (** decoded *)
    ; max_dimension : int (** pixels, either way *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; provider : string
  ; key : string
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  ; image_limit : Image_limit.t option
    (** [None] from a backend from before it sent one *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
