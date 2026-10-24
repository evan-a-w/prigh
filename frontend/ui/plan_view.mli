open! Core
open! Import

(** How both frontends show the model's plan ([update_plan]). *)

(** For the status line: [plan 2/5: current step], [None] without a plan or
    once every step is completed. *)
val status : P.Plan.t -> string option

(** [✓], [▸] or [○]. *)
val mark : P.Plan.Status.t -> string

(** One line per step, marked, and the explanation first when there is one. *)
val lines : P.Plan.t -> string list
