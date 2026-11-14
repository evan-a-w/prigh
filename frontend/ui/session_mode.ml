open! Core
open! Import

type t =
  { approval : P.Approval_mode.t
  ; plan : bool
  ; chat : bool
  }
[@@deriving sexp_of, equal]

let of_state (s : P.State.t) =
  { approval = s.approval_mode; plan = s.plan_mode; chat = s.chat_mode }
;;

let cycle t =
  if t.chat
  then { approval = Ask; plan = false; chat = false }
  else if t.plan
  then { t with chat = true }
  else (
    match t.approval with
    | Ask -> { t with approval = Auto_edit }
    | Auto_edit -> { t with approval = Auto }
    | Auto -> { t with plan = true })
;;

let label t =
  if t.chat
  then "chat"
  else if t.plan
  then "plan"
  else P.Approval_mode.to_string t.approval
;;

let names = [ "ask"; "auto-edit"; "auto"; "plan"; "chat" ]

let choices ~current =
  List.map P.Approval_mode.all ~f:(fun approval ->
    { approval; plan = false; chat = false })
  @ [ { current with plan = true; chat = false }; { current with chat = true } ]
;;

let of_name ~current name =
  match String.lowercase (String.strip name) with
  | "plan" -> Some { current with plan = true; chat = false }
  | "chat" -> Some { current with chat = true }
  | name ->
    Option.map (P.Approval_mode.of_string name) ~f:(fun approval ->
      { approval; plan = false; chat = false })
;;

let leaving_chat t = { t with chat = false }

(* The backend's classification: its test "which built-in tools ask in each
   mode" lists the tools that ask. *)
let others_ask = "bash, job_input and MCP tools ask first"
let leave = "/plan off or Alt+M"
let leave_chat = "/chat off or Alt+M"

let title t =
  if t.chat
  then "Chat: no tool host"
  else if t.plan
  then "Plan mode"
  else (
    match t.approval with
    | Ask -> "Asks: edits & commands"
    | Auto_edit -> "Asks: commands"
    | Auto -> "Auto: never asks")
;;

let describe t =
  if t.chat
  then
    "a chat without the tool host: no files, commands or project, only the \
     web and other host-free tools, until you leave it ("
    ^ leave_chat
    ^ ")"
  else if t.plan
  then
    "read-only planning, until you leave it ("
    ^ leave
    ^ "): no file edits; "
    ^ others_ask
  else (
    match t.approval with
    | Ask -> "bash, job_input, edits and MCP tools ask first"
    | Auto_edit -> "file edits run; " ^ others_ask
    | Auto -> "tools run without asking")
;;

let params t =
  [ "approval_mode", P.Json.str (P.Approval_mode.to_string t.approval)
  ; "plan_mode", P.Json.bool t.plan
  ; "chat_mode", P.Json.bool t.chat
  ]
;;
