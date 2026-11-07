open! Core
open! Import

(** [render_ui]: the model shows components ({!Ui_components}) inline in the
    chat. Frontends render them from the call's arguments (like
    [update_plan]'s); the model gets a short acknowledgement, e.g. [Shown:
    chart "Build times", table "Slowest tests" (12 rows).], or an error
    listing every problem by JSON path, and then nothing is shown. Images
    are checked on the session's tool host (the context's [ui]): they must
    exist and be png, jpeg, gif, webp or svg of at most 20 MB.

    It is always offered (so the prompt prefix stays the same), but refuses
    to run unless the user's latest prompt came from prigh-web. Only the
    main agent has it ({!Tools.for_context}). *)

val tool : Tool.t
val name : string
