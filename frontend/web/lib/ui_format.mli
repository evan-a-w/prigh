open! Core

(** Numbers as [render_ui] components show them. *)

(** [1,234.5]: thousands separated; [decimals] fixed, else as many as the
    value needs, up to 2 (up to 4 below 1). *)
val number : ?decimals:int -> float -> string

(** A fraction as a percentage: [0.125] is [12.5%]. *)
val percent : ?decimals:int -> float -> string

(** [$1,234.50], [€12.00], [¥1,200] (JPY and KRW have no decimals), else
    [CHF 12.00]. *)
val currency : ?decimals:int -> code:string -> float -> string

(** Short, for axes: [950], [1.2k], [3.4M], [0.25]. *)
val compact : float -> string

(** Ticks for an axis covering [lo, hi]: about [count], at 1, 2 or 5 times a
    power of ten, the first at or below [lo] and the last at or above [hi].
    Zero is included when the range is within one tick of it. *)
val ticks : ?count:int -> float -> float -> float list
