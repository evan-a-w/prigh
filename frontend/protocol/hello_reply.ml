open! Core

type t =
  { client_id : string
  ; namespace : string option
  ; user : string option
  ; superuser : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind client_id = Json.string_field j "client_id" in
  let%bind host_id = Json.string_opt_field j "host_id" in
  let client_id = Option.value host_id ~default:client_id in
  let%bind namespace = Json.string_opt_field j "namespace" in
  let%bind user = Json.string_opt_field j "user" in
  let%map superuser = Json.bool_default_field j "superuser" ~default:false in
  { client_id; namespace; user; superuser }
;;

let acting_as t =
  match t.user, t.namespace with
  | Some user, Some namespace when not (String.equal user namespace) ->
    Some namespace
  | _ -> None
;;
