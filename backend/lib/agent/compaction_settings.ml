open! Core
open! Import

type t =
  { max_tokens : int option
  ; model : Model.t option
  ; mask_observations : bool
  }
[@@deriving sexp_of]

let window_only = { max_tokens = None; model = None; mask_observations = true }

let threshold t (model : Model.t) =
  let window = model.context_window * 4 / 5 in
  Option.value_map t.max_tokens ~default:window ~f:(Int.min window)
;;

let should_compact t model ~input_tokens = input_tokens > threshold t model

let worth_masking
      t
      model
      ~input_tokens
      ~overflowed
      (plan : Observation_mask.Plan.t)
  =
  let threshold = threshold t model in
  overflowed
  || (input_tokens * 8 > threshold * 5 && plan.tokens_saved * 8 >= threshold)
;;

let summary_model t (model : Model.t) =
  match t.model with
  | Some summariser when summariser.context_window >= threshold t model ->
    summariser
  | Some _ | None -> model
;;
