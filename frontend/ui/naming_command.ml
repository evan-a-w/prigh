open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set_model of string option
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /naming or /naming model MODEL|off"

let parse rest =
  match
    String.split (String.strip rest) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [] -> Action.Show
  | [ "model"; "off" ] -> Set_model None
  | [ "model"; name ] -> Set_model (Some name)
  | [ "model" ] -> Error ("which model? " ^ usage)
  | _ -> Error usage
;;

let fields : Action.t -> _ = function
  | Set_model model ->
    [ ( "naming_model"
      , Option.value_map model ~default:`Null ~f:(fun s -> `String s) )
    ]
  | Show | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set_model naming_model -> { config with naming_model }
  | Show | Error _ -> config
;;

let summary (config : P.Config.t) =
  sprintf
    "sessions and subagents are named by %s (/naming model MODEL|off)"
    (match config.naming_model with
     | Some model -> model
     | None -> "the generate model, else the session's model")
;;

let complete ~models rest =
  match String.lfindi rest ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> Some (0, [ "model", "the model that names sessions and subagents" ])
  | Some i ->
    let word = String.prefix rest i in
    let arg = String.lstrip (String.drop_prefix rest i) in
    let offset = String.length rest - String.length arg in
    if String.exists arg ~f:Char.is_whitespace
    then None
    else (
      match word with
      | "model" ->
        Some
          ( offset
          , ("off", "the generate model, else the session's, names them")
            :: List.map models ~f:(fun (m : P.Model.t) -> m.key, m.name) )
      | _ -> None)
;;
