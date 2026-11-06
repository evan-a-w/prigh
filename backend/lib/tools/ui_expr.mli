open! Core
open! Import

(** The expressions of [render_ui]'s [calculator] outputs, which the
    frontends evaluate as the inputs change; the backend only checks them.

    Grammar, loosest first: [c ? a : b] (right associative); comparisons
    [< <= > >= == !=] (1 or 0); [+ -]; [* / %]; unary [-]; [^] (right
    associative, binding tighter than unary minus: [-2^2] is [-4]);
    numbers ([12], [0.5], [.5], [1e6]), input names (identifiers),
    parentheses and function calls. *)

type t =
  | Number of float
  | Name of string
  | Neg of t
  | Binary of string * t * t (** [+ - * / % ^ < <= > >= == !=] *)
  | Cond of t * t * t
  | Call of string * t list
[@@deriving sexp_of, equal]

(** The functions with how many arguments each takes: [min] and [max] one or
    more; [pow] two; [abs round floor ceil sqrt log] (base 10) [ln exp]
    one. *)
val functions : (string * [ `Exactly of int | `At_least of int ]) list

(** Fails with what is wrong and where (1-based column), e.g. [expected ")"
    at column 9]. *)
val parse : string -> (t, string) Result.t

(** What is wrong with [t]'s names given the [inputs] there are: unknown
    names, unknown functions, wrong argument counts, each saying what
    exists. *)
val check : t -> inputs:string list -> string list
