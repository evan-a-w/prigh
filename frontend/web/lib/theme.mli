open! Core
open! Import

(** How prigh-web looks: a theme and an accent, the colour of whatever wants
    the user's attention (their turns, the selection, focus, work in
    progress). The page sets them on the document ([data-theme],
    [data-accent], and for a custom accent its colour as [--sodium]); the
    stylesheets derive everything else from them. *)

module Name : sig
  type t =
    | Original (** graphite with a warm accent, the look prigh-web had first *)
    | Dark (** neutral greys: the accent is the only colour but outcomes' *)
    | Light
  [@@deriving sexp_of, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t option
  val describe : t -> string
end

module Accent : sig
  module Preset : sig
    type t =
      | Orange
      | Blue
      | Green
      | Purple
      | Pink
      | Teal
      | Yellow
    [@@deriving sexp_of, equal, enumerate]

    val to_string : t -> string

    (** Its colour on the dark themes (the light one darkens it). *)
    val hex : t -> string
  end

  type t =
    | Preset of Preset.t
    | Custom of string (** [#rrggbb], lower case *)
  [@@deriving sexp_of, equal]

  (** A preset's name (any case), or [#rgb] / [#rrggbb]. *)
  val of_string : string -> t option

  val to_string : t -> string
  val hex : t -> string
  val all_presets : t list
end

type t =
  { name : Name.t
  ; accent : Accent.t
  }
[@@deriving sexp_of, equal]

val default : t

(** What the page puts on the document element: its attributes, and the
    CSS variables set on it (a custom accent's colour and the ink that reads
    on it). *)
val attributes : t -> (string * string) list

val vars : t -> (string * string) list
