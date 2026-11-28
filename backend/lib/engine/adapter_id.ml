open! Core
open! Import

type t =
  | Codex
  | Claude
  | Acp of string
  | Backend of t
  | Prigh
[@@deriving sexp, equal, compare]

let rec to_string = function
  | Codex -> "codex"
  | Claude -> "claude"
  | Acp name -> "acp:" ^ name
  | Backend inner -> "backend:" ^ to_string inner
  | Prigh -> "prigh"
;;

let of_inner s =
  match s with
  | "codex" -> Ok Codex
  | "claude" -> Ok Claude
  | "prigh" -> Ok Prigh
  | _ ->
    (match String.chop_prefix s ~prefix:"acp:" with
     | Some name when not (String.is_empty (String.strip name)) -> Ok (Acp name)
     | Some _ | None ->
       Or_error.errorf
         "unknown adapter %S: expected codex, claude, prigh, acp:<name> or \
          backend:acp:<name>"
         s)
;;

let of_string s =
  match String.chop_prefix s ~prefix:"backend:" with
  | Some rest ->
    (match of_inner rest with
     | Ok (Prigh | Backend _) ->
       Or_error.errorf
         "invalid backend adapter %S: prigh's own harness already runs on the \
          backend"
         s
     | Ok inner -> Ok (Backend inner)
     | Error _ as e -> e)
  | None -> of_inner s
;;

let rec label = function
  | Codex -> "Codex"
  | Claude -> "Claude Code"
  | Acp name ->
    (match Acp_agent.find_known name with
     | Some agent -> agent.display_name
     | None -> name ^ " (ACP)")
  | Backend inner -> label inner ^ " (backend)"
  | Prigh -> "prigh"
;;

let on_backend = function
  | Backend _ -> true
  | Codex | Claude | Acp _ | Prigh -> false
;;

let rec inner = function
  | Backend a -> inner a
  | (Codex | Claude | Acp _ | Prigh) as a -> a
;;

let is_external = function
  | Prigh -> false
  | Codex | Claude | Acp _ | Backend _ -> true
;;

let acp_name t =
  match inner t with
  | Acp name -> Some name
  | Codex | Claude | Prigh | Backend _ -> None
;;
