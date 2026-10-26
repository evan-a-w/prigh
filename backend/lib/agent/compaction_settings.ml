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

let worth_masking_at
      ?(at = 0.625)
      ?(min_saving = 0.125)
      ~threshold
      ~input_tokens
      ~overflowed
      (plan : Observation_mask.Plan.t)
  =
  let threshold = Float.of_int threshold in
  overflowed
  || (Float.(of_int input_tokens > threshold * at)
      && Float.(of_int plan.tokens_saved >= threshold * min_saving))
;;

let worth_masking t model =
  worth_masking_at ?at:None ?min_saving:None ~threshold:(threshold t model)
;;

let summary_model t (model : Model.t) =
  match t.model with
  | Some summariser when summariser.context_window >= threshold t model ->
    summariser
  | Some _ | None -> model
;;
