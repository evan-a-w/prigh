open! Core
open! Import

type t =
  { approval : P.Approval_mode.t
  ; plan : bool
  }
[@@deriving sexp_of, equal]

let of_state (s : P.State.t) =
  { approval = s.approval_mode; plan = s.plan_mode }
;;

let cycle t =
  if t.plan
  then { approval = Ask; plan = false }
  else (
    match t.approval with
    | Ask -> { t with approval = Auto_edit }
    | Auto_edit -> { t with approval = Auto }
    | Auto -> { t with plan = true })
;;

let label t = if t.plan then "plan" else P.Approval_mode.to_string t.approval
let names = [ "ask"; "auto-edit"; "auto"; "plan" ]

let choices ~current =
  List.map P.Approval_mode.all ~f:(fun approval -> { approval; plan = false })
  @ [ { current with plan = true } ]
;;

let of_name ~current name =
  match String.lowercase (String.strip name) with
  | "plan" -> Some { current with plan = true }
  | name ->
    Option.map (P.Approval_mode.of_string name) ~f:(fun approval ->
      { approval; plan = false })
;;

(* The backend's classification: its test "which built-in tools ask in each
   mode" lists the tools that ask. *)
let others_ask = "bash, job_input and MCP tools ask first"
let leave = "/plan off or Alt+M"

let title t =
  if t.plan
  then "Plan mode"
  else (
    match t.approval with
    | Ask -> "Asks: edits & commands"
    | Auto_edit -> "Asks: commands"
    | Auto -> "Auto: never asks")
;;

let describe t =
  if t.plan
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
  ]
;;
