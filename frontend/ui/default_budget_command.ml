open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set of float option
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /default-budget or /default-budget DOLLARS|off"

let parse rest =
  let word = String.strip rest in
  match String.lowercase word with
  | "" -> Action.Show
  | "off" | "none" -> Set None
  | _ ->
    (match
       Float.of_string_opt (String.chop_prefix_if_exists word ~prefix:"$")
     with
     | Some usd when Float.is_finite usd && Float.(usd > 0.) -> Set (Some usd)
     | Some _ | None ->
       Error
         (sprintf
            "%S is not a positive number of dollars (off: no budget); %s"
            word
            usage))
;;

let fields : Action.t -> _ = function
  | Set usd ->
    [ ( "max_budget_usd"
      , Option.value_map usd ~default:`Null ~f:(fun usd ->
          `Number (sprintf "%g" usd)) )
    ]
  | Show | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set max_budget_usd -> { config with max_budget_usd }
  | Show | Error _ -> config
;;

let dollars usd = sprintf "$%.2f" usd

let summary (config : P.Config.t) =
  match config.max_budget_usd with
  | Some usd ->
    sprintf
      "sessions without their own /budget stop once they cost %s \
       (/default-budget off removes it)"
      (dollars usd)
  | None ->
    "no default budget: /default-budget DOLLARS sets one for sessions without \
     their own /budget"
;;
