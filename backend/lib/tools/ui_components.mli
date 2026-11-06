open! Core
open! Import

(** The arguments of [render_ui]: [{"components": [Component, ...]}], the
    components frontends render inline (prigh-web) or as one-line
    placeholders (prigh-tui). Frontends parse them tolerantly; here they are
    checked strictly, so that the model learns of every mistake with its
    JSON path and a fix rather than the user seeing a broken rendering. *)

(** The component types, in the order the tool's description gives them. *)
val types : string list

module Checked : sig
  type t =
    { summaries : string list
      (** one per top-level component, e.g. [chart "Build times"], [table
          "Slowest tests" (12 rows)] *)
    ; images : (string * string) list
      (** the JSON path and the absolute path of every [image] and
          [gallery] image, to check on the tool host *)
    ; interactive : string list
      (** the components that reply to the model ([form], [choices], a
          [checklist] with [submit]), e.g. [form "Deploy options"] *)
    }
  [@@deriving sexp_of]
end

(** Every problem with the arguments, each as [PATH: problem; fix], e.g.
    [components[1].series[0].values: 11 values for 12 labels; give one per
    label]. [resolve] makes an image's relative path absolute, for the error
    that says which absolute path to use. *)
val check
  :  Json.t
  -> resolve:(string -> string)
  -> (Checked.t, string list) Result.t

(** The [id]s of the components in [render_ui]'s arguments, nested ones
    too. *)
val ids : Json.t -> string list
