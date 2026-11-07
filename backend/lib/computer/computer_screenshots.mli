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
    replaced by {!placeholder} (a line after the result's text), and the
    thinking left out of the replies written while an image now omitted was
    still sent: a provider may check that what came before a thinking block
    is what it was (Anthropic's preserved thinking), and refuse the request
    otherwise. Leaving those out keeps the thinking sent an unbroken run of
    what the model wrote against the request as it is now. *)
val prune : min:int -> max:int -> Message.t list -> Message.t list
