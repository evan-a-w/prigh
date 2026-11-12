open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set_model of string option
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /generate-model or /generate-model MODEL|off"

let parse rest =
  match
    String.split (String.strip rest) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [] -> Action.Show
  | [ "off" ] -> Set_model None
  | [ name ] -> Set_model (Some name)
  | _ -> Error usage
;;

let fields : Action.t -> _ = function
  | Set_model model ->
    [ ( "generate_model"
      , Option.value_map model ~default:`Null ~f:(fun s -> `String s) )
    ]
  | Show | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set_model generate_model -> { config with generate_model }
  | Show | Error _ -> config
;;

let summary (config : P.Config.t) =
  sprintf
    "commit messages, pull requests and branch names are drafted by %s \
     (/generate-model MODEL|off)"
    (Option.value config.generate_model ~default:"the session's model")
;;

let complete ~models rest =
  if String.exists rest ~f:Char.is_whitespace
  then None
  else
    Some
      ( 0
      , ("off", "the session's model drafts them")
        :: List.map models ~f:(fun (m : P.Model.t) -> m.key, m.name) )
;;
