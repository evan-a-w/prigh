open! Core
open! Import

(** How large an image each model accepts, and what to do when a provider
    refuses one as too large.

    Known limits come from the provider's API ({!known}); a refusal teaches a
    stricter one for that model ({!learn}, kept until the backend restarts),
    and [/downsize-images] tightens it by hand ({!tighten}). Requests send
    every image within {!for_request}, downscaled when it is over it (see
    {!Image_fit}): switching to a stricter model never bricks a session. *)

(** Per image, before anything is learned: Anthropic 5 MB of base64 and
    8000 px (with a margin); OpenAI 20 MB; others Anthropic's, the strictest
    common one. *)
val known : Model.t -> Image.Limit.t

(** {!known}, or what was learned or tightened for the model since. *)
val for_model : Model.t -> Image.Limit.t

(** For a request with [images] images: Anthropic allows only 2000 px each
    in a request of more than 20. *)
val for_request : Model.t -> images:int -> Image.Limit.t

(** Whether a provider error says an image was too large ("image exceeds 5
    MB maximum", "image dimensions exceed max allowed size: 8000 pixels",
    "image too large"), as opposed to another failure. *)
val matches : string -> bool

(** The limit [error] states, within [current]: its MB or bytes (with a 10%
    margin) and pixels; {!Image.Limit.tighten} of [current] when it states
    neither or nothing stricter. [None] when [error] is not about an image's
    size. *)
val of_error : string -> current:Image.Limit.t -> Image.Limit.t option

(** Remembers [limit] for the model (never loosening it). *)
val learn : Model.t -> Image.Limit.t -> unit

(** Makes the model's limit stricter by hand ([/downsize-images]), returning
    it. *)
val tighten : Model.t -> Image.Limit.t

(** Forgets what was learned, for tests. *)
val reset : unit -> unit
