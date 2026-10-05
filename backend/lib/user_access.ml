open! Core
open! Import

type t =
  { namespaces : Namespace.t list
  ; host_tokens : Namespace.t list
  ; superusers : String.Set.t
  }

let parse_names spec =
  String.split spec ~on:','
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let known namespaces name =
  List.exists namespaces ~f:(fun (n : Namespace.t) -> String.equal n.name name)
;;

let create namespaces ~host_tokens ~superusers =
  let unknown flag names =
    List.find names ~f:(Fn.non (known namespaces))
    |> Option.map ~f:(fun name -> Or_error.errorf "%s: no user %S" flag name)
  in
  let tokens =
    List.map namespaces ~f:(fun (n : Namespace.t) -> n.token)
    @ List.map host_tokens ~f:(fun (n : Namespace.t) -> n.token)
  in
  match
    ( unknown "-superusers" superusers
    , unknown
        "-host-tokens"
        (List.map host_tokens ~f:(fun (n : Namespace.t) -> n.name)) )
  with
  | Some e, _ | None, Some e -> e
  | None, None ->
    if List.contains_dup tokens ~compare:String.compare
    then Or_error.error_string "-host-tokens: a host token is not unique"
    else
      Ok { namespaces; host_tokens; superusers = String.Set.of_list superusers }
;;

let namespaces t = t.namespaces
let unauthorised = "unauthorised: bad user name or password"
let no_users = "user switching needs users (prigh serve -tokens)"

module Signed_in = struct
  type access = t

  type t =
    { access : access
    ; user : string
    ; superuser : bool
    }

  let switch t as_user =
    if String.equal as_user t.user
    then Ok as_user
    else if not t.superuser
    then Or_error.errorf "unauthorised: %s is not a superuser" t.user
    else if known t.access.namespaces as_user
    then Ok as_user
    else Or_error.errorf "no user %S" as_user
  ;;

  let users t =
    if t.superuser
    then Ok (List.map t.access.namespaces ~f:(fun (n : Namespace.t) -> n.name))
    else Or_error.errorf "unauthorised: %s is not a superuser" t.user
  ;;
end

let authenticate t ?user ?as_user token =
  let by_token namespaces =
    List.find namespaces ~f:(fun (n : Namespace.t) ->
      Option.exists token ~f:(String.equal n.token)
      && Option.for_all user ~f:(String.equal n.name))
  in
  let signed_in =
    match by_token t.namespaces, by_token t.host_tokens with
    | Some n, _ ->
      Ok
        { Signed_in.access = t
        ; user = n.name
        ; superuser = Set.mem t.superusers n.name
        }
    | None, Some n ->
      Ok { Signed_in.access = t; user = n.name; superuser = false }
    | None, None -> Or_error.error_string unauthorised
  in
  Or_error.bind signed_in ~f:(fun signed_in ->
    match Option.filter as_user ~f:(Fn.non String.is_empty) with
    | None -> Ok (signed_in, signed_in.user)
    | Some as_user ->
      Or_error.map (Signed_in.switch signed_in as_user) ~f:(fun user ->
        signed_in, user))
;;
