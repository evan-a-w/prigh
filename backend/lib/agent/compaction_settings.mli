open! Core
open! Import

(** How a conversation's context is kept in its window: when old tool
    outputs are masked ({!Observation_mask}), when the conversation is
    compacted ({!Compaction}) and which model writes the summary. The
    config's [mask_observations], [compact_at_tokens] and
    [compaction_model], resolved. *)

type t =
  { max_tokens : int option
    (** compact once the context exceeds this, even when it is under 80% of
        the window ({!threshold}); [None]: 80% only *)
  ; model : Model.t option (** the summariser; [None]: the conversation's *)
  ; mask_observations : bool
  }
[@@deriving sexp_of]

(** 80% of the window, summarised by the conversation's model; masking on. *)
val window_only : t

(** The input tokens above which a conversation held by [model] is
    compacted: 80% of its window, or [t.max_tokens] when that is less. *)
val threshold : t -> Model.t -> int

(** True when [input_tokens] exceed {!threshold}. *)
val should_compact : t -> Model.t -> input_tokens:int -> bool

(** Whether to carry out an observation-masking [plan] on a context of
    [input_tokens]: when the request [overflowed] the window (anything that
    helps), or when the context is over 5/8 of {!threshold} and masking
    frees at least 1/8 of it (half and a tenth of the window for a model
    compacted at 80% of it). The floor holds over the threshold too, and so
    keeps masking from re-masking (and so invalidating the provider's cache)
    every turn: with less to free, the conversation is summarised instead,
    and when it is not (nothing to summarise, or the summary failed),
    masking waits until the batch is large. A smaller floor would not do:
    one turn's output alone can exceed it. [t.mask_observations] is the
    caller's to check. *)
val worth_masking
  :  t
  -> Model.t
  -> input_tokens:int
  -> overflowed:bool
  -> Observation_mask.Plan.t
  -> bool

(** The model that summarises a conversation held by [model]: [t.model] when
    its window holds what is summarised (up to [model]'s threshold), else
    [model]. *)
val summary_model : t -> Model.t -> Model.t
