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

(** The value of [t] given the inputs' values: the semantics the frontends
    mirror (the backend itself only checks expressions). IEEE doubles
    throughout: [/] by 0 is infinite or nan; [a % b] is the remainder with
    [a]'s sign (JavaScript's [%], C's [fmod]); [^] and [pow] are powers;
    comparisons are 1 or 0 (any comparison with nan is 0, except [!=]);
    [c ? a : b] is [b] when [c] is 0 or nan, else [a]; [min] and [max] are
    nan if any argument is; [round] rounds halves away from 0; [log] is
    base 10, [ln] base e. Unknown names and functions are nan. *)
val eval : t -> inputs:(string -> float) -> float
