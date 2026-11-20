open! Core
open! Import

module Row = struct
  type t =
    { id : string
    ; label : string
    ; default : bool
    ; current : bool
    }
  [@@deriving sexp_of, equal]
end

let level_label level =
  match Thinking.normalize level with
  | "off" -> "Off"
  | "default" -> "Default"
  | "minimal" -> "Minimal"
  | "low" -> "Low"
  | "medium" -> "Medium"
  | "high" -> "High"
  | "xhigh" -> "Extra High"
  | "max" -> "Max"
  | other -> String.capitalize other
;;

(* The provider's own level, when the model names one it takes. *)
let known_default (model : Llm.t) =
  match Option.map model.thinking_default ~f:Thinking.normalize with
  | Some level
    when (not (List.mem [ "default"; "off" ] level ~equal:String.equal))
         && List.mem (Thinking.options model) level ~equal:String.equal ->
    Some level
  | _ -> None
;;

let rows (model : Llm.t) ~current =
  let current = Thinking.normalize current in
  let options = Thinking.options model in
  match known_default model with
  | Some known ->
    List.filter_map options ~f:(fun level ->
      if String.equal level "default"
      then None
      else if String.equal level known
      then
        Some
          { Row.id = "default"
          ; label = level_label level
          ; default = true
          ; current =
              String.equal current "default" || String.equal current level
          }
      else
        Some
          { id = level
          ; label = level_label level
          ; default = false
          ; current = String.equal current level
          })
  | None ->
    List.map options ~f:(fun level ->
      { Row.id = level
      ; label = level_label level
      ; default = String.equal level "default"
      ; current = String.equal current level
      })
;;

let chip_label (model : Llm.t) ~current =
  if not model.supports_thinking
  then None
  else (
    match Thinking.normalize current, known_default model with
    | "default", Some known -> Some (level_label known)
    | level, _ -> Some (level_label level))
;;
