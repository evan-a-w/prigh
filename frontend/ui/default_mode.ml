open! Core
open! Import

type t =
  | Approval of P.Approval_mode.t
  | Chat
[@@deriving sexp_of, equal]

let all = List.map P.Approval_mode.all ~f:(fun m -> Approval m) @ [ Chat ]

let of_config (c : P.Config.t) =
  if c.default_chat_mode then Chat else Approval c.approval_mode
;;

let apply t (c : P.Config.t) =
  match t with
  | Chat -> { c with default_chat_mode = true }
  | Approval approval_mode ->
    { c with approval_mode; default_chat_mode = false }
;;

let fields = function
  | Approval _ -> [ "approval_mode"; "default_chat_mode" ]
  | Chat -> [ "default_chat_mode" ]
;;

let json_fields t config =
  match P.Config.to_json config with
  | `Object all ->
    List.filter all ~f:(fun (name, _) ->
      List.mem (fields t) name ~equal:String.equal)
  | _ -> []
;;

let to_string = function
  | Approval m -> P.Approval_mode.to_string m
  | Chat -> "chat"
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "chat" -> Some Chat
  | "on" | "true" -> Some (Approval Ask)
  | "off" | "false" -> Some (Approval Auto)
  | s -> Option.map (P.Approval_mode.of_string s) ~f:(fun m -> Approval m)
;;

let session_mode (c : P.Config.t) : Session_mode.t =
  { approval = c.approval_mode; plan = false; chat = c.default_chat_mode }
;;

let summary (c : P.Config.t) =
  if c.default_chat_mode
  then
    sprintf "chat (leaving it: %s)" (P.Approval_mode.to_string c.approval_mode)
  else P.Approval_mode.to_string c.approval_mode
;;

let describe = function
  | Approval approval ->
    Session_mode.describe { approval; plan = false; chat = false }
  | Chat ->
    "a chat without the tool host; leaving it returns to the approval mode \
     underneath"
;;

let arguments =
  [ "on", describe (Approval Ask) ^ " (mode ask)"
  ; "off", describe (Approval Auto) ^ " (mode auto)"
  ]
  @ List.map all ~f:(fun t -> to_string t, describe t)
;;
