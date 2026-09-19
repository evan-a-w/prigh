open! Core
open! Import

module Context : sig
  type t =
    { env : Env.t
    ; cwd : string
    ; cancel : Cancellation.t
    ; on_output : string -> unit (** streamed partial output, e.g. from bash *)
    }

  val create
    :  ?cancel:Cancellation.t
    -> ?on_output:(string -> unit)
    -> env:Env.t
    -> cwd:string
    -> unit
    -> t
end

module Result : sig
  type t =
    { text : string
    ; is_error : bool
    }
  [@@deriving sexp_of]

  val ok : string -> t
  val error : string -> t
end

type t =
  { spec : Tool_spec.t
  ; run : Context.t -> Json.t -> Result.t
  }

val name : t -> string

(** Runs the tool, turning [Tool_args.Invalid] and other exceptions into error
    results. *)
val execute : t -> Context.t -> Json.t -> Result.t

(** Resolves a user-supplied path against the context cwd, expanding [~]. *)
val resolve_path : Context.t -> string -> string

val expand_home : string -> string
