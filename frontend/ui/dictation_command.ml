open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set_model of string option
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /dictation or /dictation model MODEL|off"

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

let suggestions =
  [ "openai/gpt-4o-mini-transcribe", "OpenAI, fast and cheap (the default)"
  ; "openai/gpt-4o-transcribe", "OpenAI, most accurate"
  ; "openai/whisper-1", "OpenAI Whisper"
  ]
;;

let fields : Action.t -> _ = function
  | Set_model model ->
    [ ( "dictation_model"
      , Option.value_map model ~default:`Null ~f:(fun s -> `String s) )
    ]
  | Show | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set_model dictation_model -> { config with dictation_model }
  | Show | Error _ -> config
;;

let summary (config : P.Config.t) =
  sprintf
    "dictation is transcribed by %s (/dictation model MODEL|off)"
    (match config.dictation_model with
     | Some model -> model
     | None -> P.Config.default_dictation_model ^ ", the default")
;;

let complete rest =
  match String.lfindi rest ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> Some (0, [ "model", "the model that transcribes dictation" ])
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
          , ("off", sprintf "the default, %s" P.Config.default_dictation_model)
            :: suggestions )
      | _ -> None)
;;
