open! Core

(** Messages' times as the reader sees them: in the browser's time zone
    (its UTC offset, which the page passes in, so this stays pure) and
    relative to today. *)

type t [@@deriving sexp_of, equal]

(** [utc_offset]: local time minus UTC, e.g. +2h in Paris in summer. *)
val create : now:Time_ns.t -> utc_offset:Time_ns.Span.t -> t

(** ["14:32"] today, ["Yesterday 14:32"], ["3 Oct 14:32"] this year,
    ["3 Oct 2025 14:32"] before. *)
val short : t -> Time_ns.t -> string

(** For a tooltip: ["Monday 5 October 2026, 14:32:10"]. *)
val full : t -> Time_ns.t -> string

(** The local date. *)
val date : t -> Time_ns.t -> Date.t

(** A day separator: ["Today"], ["Yesterday"], ["Mon 3 Oct"] this year,
    ["Mon 3 Oct 2025"] before. *)
val day_label : t -> Date.t -> string
