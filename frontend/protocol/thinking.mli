open! Core

(** Thinking settings as the backend names them: ["off"], ["default"] (the
    provider's default: thinking on, no effort sent) and the effort levels. *)

(** Every setting, lowest first: for when no model is known (new sessions'
    default). *)
val all : string list

(** ["on"], the name of ["default"] before levels were per model, as
    ["default"]; anything else as it is. *)
val normalize : string -> string

(** What a person reads: ["provider's default"] for ["default"]. *)
val label : string -> string

(** A short phrase for pickers. *)
val description : string -> string

(** The model's settings, in cycling order: what the backend lists, or (from
    an older backend) {!all} for a model that thinks and just ["off"]
    otherwise. *)
val options : Model.t -> string list

(** The setting after [current] in the model's {!options}, wrapping. *)
val next : Model.t -> current:string -> string

(** The error for a setting the model does not take: its options. *)
val unsupported : Model.t -> string -> string
