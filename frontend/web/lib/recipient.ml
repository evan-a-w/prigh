open! Core
open! Import

type t =
  | Main
  | Agent of string
[@@deriving sexp_of, equal]

let refusal (agents : Agents.t) id =
  match Agents.find_agent agents id with
  | None ->
    Some
      (sprintf
         "There is no subagent %s in this session: /agents lists them."
         id)
  | Some { parent = Some parent; _ } ->
    Some
      (sprintf
         "%s runs inside %s (a subagent's own subagent) and takes no messages: \
          steer %s instead."
         id
         parent
         parent)
  | Some { status = Agents.Status.(Complete | Failed); _ } ->
    Some
      (sprintf
         "%s has finished and takes no more messages: tell the main agent \
          instead."
         id)
  | Some _ -> None
;;
