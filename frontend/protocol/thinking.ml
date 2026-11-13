open! Core

let all =
  [ "off"; "default"; "minimal"; "low"; "medium"; "high"; "xhigh"; "max" ]
;;

let normalize s =
  match String.lowercase (String.strip s) with
  | "on" -> "default"
  | s -> s
;;

let label s =
  match normalize s with
  | "default" -> "provider's default"
  | s -> s
;;

let description s =
  match normalize s with
  | "off" -> "answer straight away"
  | "default" -> "the provider picks the effort"
  | "minimal" -> "barely"
  | "low" -> "a little"
  | "medium" -> "some"
  | "high" -> "more"
  | "xhigh" -> "a lot, for long agentic work"
  | "max" -> "as much as the model allows"
  | _ -> ""
;;

let options (model : Model.t) =
  match model.thinking_options with
  | _ :: _ as options -> options
  | [] -> if model.supports_thinking then all else [ "off" ]
;;

let next model ~current =
  let options = options model in
  let current = normalize current in
  match List.findi options ~f:(fun _ o -> String.equal o current) with
  | Some (i, _) -> List.nth_exn options ((i + 1) % List.length options)
  | None -> List.hd_exn options
;;

let unsupported (model : Model.t) setting =
  if model.supports_thinking
  then
    sprintf
      "%s has no thinking “%s”: use %s."
      model.name
      setting
      (String.concat ~sep:", " (options model))
  else sprintf "%s does not think: /model switches to one that does." model.name
;;
