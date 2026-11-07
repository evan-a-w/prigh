open! Core
open! Import

(** Context hygiene for computer use: a screenshot is worth little once
    newer ones show the screen, and each costs a thousand tokens or more.
    Requests carry only the latest few as images; the session keeps them
    all.

    Omitting one changes the request from that message on, which costs the
    provider's prompt cache everything after it. So screenshots are pruned
    in batches: they pile up to [max], and the request after that omits all
    but the latest [min] at once; the requests in between only append. *)

(** The placeholder text an omitted screenshot leaves: ["[screenshot
    omitted]"]. *)
val placeholder : string

(** How many of [n] screenshots (oldest first) are omitted: none up to
    [max], then [n - min] rounded down to a multiple of [max - min + 1].
    That keeps between [min] and [max], changes only when [n] goes over
    [max] (and every [max - min + 1] more after that), and then keeps
    exactly [min]. It depends on [n] alone, so every request built from the
    same messages omits the same ones. With [max = min], every new
    screenshot drops the oldest. *)
val omitted : min:int -> max:int -> int -> int

(** [messages] with the oldest {!omitted} images of [computer] results
    replaced by {!placeholder} (a line after the result's text). With
    [drop_stale_thinking] (a model that checks what came before each thinking
    block, {!Model.binds_thinking_to_prefix}), also without the thinking of
    the replies written while an image now omitted was still sent, which the
    provider would refuse. *)
val prune
  :  drop_stale_thinking:bool
  -> min:int
  -> max:int
  -> Message.t list
  -> Message.t list
