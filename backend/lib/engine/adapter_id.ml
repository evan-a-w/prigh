open! Core
open! Import

type t =
  | Codex
  | Claude
  | Acp of string
  | Prigh
[@@deriving sexp, equal, compare]

let to_string = function
  | Codex -> "codex"
  | Claude -> "claude"
  | Acp name -> "acp:" ^ name
  | Prigh -> "prigh"
;;

let of_string s =
  match s with
  | "codex" -> Ok Codex
  | "claude" -> Ok Claude
  | "prigh" -> Ok Prigh
  | _ ->
    (match String.chop_prefix s ~prefix:"acp:" with
     | Some name when not (String.is_empty (String.strip name)) -> Ok (Acp name)
     | Some _ | None ->
       Or_error.errorf
         "unknown adapter %S: expected codex, claude, prigh or acp:<name>"
         s)
;;

let label = function
  | Codex -> "Codex"
  | Claude -> "Claude Code"
  | Acp name ->
    (match Acp_agent.find_known name with
     | Some agent -> agent.display_name
     | None -> name ^ " (ACP)")
  | Prigh -> "prigh"
;;
