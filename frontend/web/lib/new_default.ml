open! Core
open! Import

type t =
  | Model of string option
  | Thinking of string option
  | Mode of Approval_mode.t
  | Host of string option
  | Dir of string option
  | Fallback of string list
  | Budget of float option
[@@deriving sexp_of, equal]

let builtin_mode = Config.default.approval_mode
let builtin_thinking = "on"
let budget_usd = 5.

let apply t (config : Config.t) =
  match t with
  | Model default_model -> { config with default_model }, "default_model"
  | Thinking default_thinking ->
    { config with default_thinking }, "default_thinking"
  | Mode approval_mode -> { config with approval_mode }, "approval_mode"
  | Host default_host -> { config with default_host }, "default_host"
  | Dir default_cwd -> { config with default_cwd }, "default_cwd"
  | Fallback fallback_models ->
    { config with fallback_models }, "fallback_models"
  | Budget max_budget_usd -> { config with max_budget_usd }, "max_budget_usd"
;;

module Picker = struct
  type t =
    | Model
    | Fallback
    | Host
  [@@deriving sexp_of, equal]
end
