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

let known_acp_names () =
  String.concat ~sep:", " (List.map Acp_agent.known ~f:(fun a -> a.name))
;;

let of_inner s =
  match s with
  | "codex" -> Ok Codex
  | "claude" -> Ok Claude
  | "prigh" -> Ok Prigh
  | _ ->
    (match Acp_agent.find_known s with
     | Some agent -> Ok (Acp agent.name)
     | None ->
       (match String.chop_prefix s ~prefix:"acp:" with
        | Some name when not (String.is_empty (String.strip name)) ->
          Ok (Acp name)
        | Some _ | None ->
          Or_error.errorf
            "unknown adapter %S: expected codex, claude, prigh, %s, \
             bridge:<adapter>, acp:<command> or acp:<name>=<command>"
            s
            (known_acp_names ())))
;;

let strip_bridge_prefix s =
  match String.chop_prefix s ~prefix:"bridge:" with
  | Some rest -> Some rest
  | None -> String.chop_prefix s ~prefix:"backend:"
;;

let of_string s =
  match strip_bridge_prefix s with
  | Some rest ->
    (match of_inner rest with
     | Ok (Prigh | Backend _) ->
       Or_error.errorf
         "invalid bridge adapter %S: prigh's own harness already runs on the \
          backend"
         s
     | Ok inner -> Ok (Backend inner)
     | Error _ as e -> e)
  | None -> of_inner s
;;

let rec harness_label = function
  | Codex -> "Codex"
  | Claude -> "Claude Code"
  | Acp name ->
    (match Acp_agent.find_known name with
     | Some agent -> agent.display_name
     | None -> name ^ " (ACP)")
  | Backend inner -> harness_label inner
  | Prigh -> "prigh"
;;

let label = function
  | Backend inner -> harness_label inner ^ " (bridge)"
  | (Codex | Claude | Acp _ | Prigh) as a -> harness_label a
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
