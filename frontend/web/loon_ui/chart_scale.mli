(** Scales and ticks for {!Chart}, as Recharts computes them (d3-scale's
    linear, band and point scales; Recharts' "nice" tick values). *)

open! Import

(** [num x] is [x] as JavaScript prints it ([String(x)]): what Recharts and
    d3 write into SVG attributes. *)
val num : float -> string

(** Recharts' [round]: to 4 decimal places, with [-0] as [0]. *)
val round4 : float -> float

module Linear : sig
  type t =
    { domain : float * float
    ; range : float * float
    }

  (** d3's [scaleLinear().domain(domain).range(range)]. *)
  val apply : t -> float -> float

  (** The inverse of [apply]. *)
  val invert : t -> float -> float
end

module Band : sig
  type t

  (** d3's [scaleBand()] (or, with [~point:true], [scalePoint()]) over [n]
      categories. *)
  val create
    :  ?padding_inner:float
    -> ?padding_outer:float
    -> ?point:bool
    -> n:int
    -> range:float * float
    -> unit
    -> t

  (** The start of category [i]'s band. *)
  val apply : t -> int -> float

  val bandwidth : t -> float
  val step : t -> float
  val count : t -> int

  (** The category under coordinate [x] (Recharts' tooltip lookup: the
      nearest band). *)
  val index_at : t -> float -> int option
end

(** Recharts' [getNiceTickValues]: [count] round tick values covering
    [(min, max)], from which a number axis takes its domain. *)
val nice_ticks : ?allow_decimals:bool -> ?count:int -> float * float -> float list

(** Recharts' [getTickValuesFixedDomain]: ticks within a given domain. *)
val fixed_domain_ticks
  :  ?allow_decimals:bool
  -> ?count:int
  -> float * float
  -> float list
