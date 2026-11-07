open! Core

(** A calculator's output expression: numbers, input names, [+ - * / ^ %],
    unary [-], parentheses, comparisons [< <= > >= == !=] (1 or 0),
    [cond ? a : b] and the functions [min max abs round floor ceil sqrt pow
    log ln exp] ([log] is base 10; [round x] or [round x digits]). [^] binds
    tighter than unary minus and to the right: [-2^2] is [-4]. *)

type t [@@deriving sexp_of]

(** An error says where and what to write instead. *)
val parse : string -> t Or_error.t

(** The input names it reads. *)
val names : t -> string list

(** A name [env] does not know is an error; so are dividing by zero and a
    result that is not a finite number. *)
val eval : t -> env:(string -> float option) -> float Or_error.t
