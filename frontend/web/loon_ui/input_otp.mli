(** Accessible one-time password component with copy paste functionality.
    Port of shadcn/ui's [InputOTP] (the input-otp library's behaviour: one
    transparent [<input>] over the slots takes the typing, pasting and
    autofill; the slot where the next character goes shows a caret).

    {[
      Input_otp.component ~max_length:6
        (return (fun otp ->
          [ Input_otp.group
              [ Input_otp.slot otp ~index:0
              ; Input_otp.slot otp ~index:1
              ; Input_otp.slot otp ~index:2
              ]
          ; Input_otp.separator ()
          ; Input_otp.group [ ... ]
          ]))
        graph
    ]} *)

open! Import

(** The characters accepted (input-otp's [REGEXP_ONLY_*]). *)
module Pattern : sig
  type t =
    | Digits
    | Chars
    | Digits_and_chars
  [@@deriving enumerate, sexp_of, compare, equal]

  (** The [pattern] attribute's regular expression. *)
  val regexp : t -> string

  val matches : t -> string -> bool
end

(** The value and the input's selection while focused, for the slots. *)
type t

(** input-otp's handling of the caret: a collapsed caret at [s] inside the
    code selects one character, so that typing replaces it and arrows move
    by a character. [adjust ~value ~max_length ~prev (s, e)] is the
    selection to make instead of [(s, e)] (and its direction), given the
    previous one; [None] keeps it. *)
module Selection : sig
  type direction =
    | Forward
    | Backward

  val adjust
    :  value:string
    -> max_length:int
    -> prev:(int * int) option
    -> int * int
    -> (int * int * direction) option
end

(** Keeps the code, [default_value] (or empty) at first, and the input's
    selection. [class_] goes on the [<input>], [container_class] on the
    container; input that does not match [pattern] is refused. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?container_class:string
  -> ?id:string
  -> ?disabled:bool
  -> ?pattern:Pattern.t
  -> ?default_value:string
  -> max_length:int
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> string Control.t Bonsai.t

(** A row of slots, joined. *)
val group : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** The box showing character [index]; [data-active] when the focused
    input's selection covers it (or its caret is there). *)
val slot : ?attrs:Attr.t list -> ?class_:string -> t -> index:int -> Node.t

(** A dash between groups ([role="separator"]). *)
val separator : ?attrs:Attr.t list -> unit -> Node.t
