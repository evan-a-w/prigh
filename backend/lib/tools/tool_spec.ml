open! Core
open! Import

module Destructive = struct
  type t =
    | Always
    | Never
    | Depends of (Json.t -> bool)
    (** whether a call with these arguments changes anything; a [list]
        action of a tool that also creates and removes does not *)

  let sexp_of_t = function
    | Always -> Sexp.Atom "true"
    | Never -> Sexp.Atom "false"
    | Depends _ -> Sexp.Atom "depends_on_arguments"
  ;;

  let of_bool b = if b then Always else Never

  let call t args =
    match t with
    | Always -> true
    | Never -> false
    | Depends f -> f args
  ;;
end

module Native = struct
  type t =
    | Computer of
        { width : int
        ; height : int
        }
    (** the [computer] tool, whose screenshots are [width] x [height]:
        providers whose model has a built-in computer-use tool
        ([Model.native_computer]) offer that instead of the schema *)
  [@@deriving sexp_of, equal]
end

type t =
  { name : string
  ; description : string
  ; parameters : Json.t
  ; parallel_safe : bool
  ; destructive : Destructive.t
    (** whether a call needs the user's approval in [Ask] mode *)
  ; on_host : bool
    (** Runs on the session's active tool host (filesystem and process
          tools) rather than always in the backend (e.g. subagent). *)
  ; freeform : string option [@sexp.option]
    (** A Lark grammar: providers with custom tools (OpenAI Responses) let
        the model write the input as free text matching it rather than JSON.
        Such tools take one string parameter, [input], which is how the
        text reaches [run] and how other providers send it. *)
  ; native : Native.t option [@sexp.option]
    (** a provider-defined form the tool may be offered in instead *)
  }
[@@deriving sexp_of]
