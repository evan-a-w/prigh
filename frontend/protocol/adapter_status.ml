open! Core

module Installed = struct
  type t =
    | Yes
    | No
    | Unknown of string
  [@@deriving sexp_of, equal]
end

module Auth = struct
  type t =
    | Logged_in
    | Logged_out
    | Not_needed
    | Unknown
  [@@deriving sexp_of, equal]
end

type t =
  { adapter : string
  ; label : string
  ; host : string
  ; host_name : string
  ; installed : Installed.t
  ; version : string option
  ; path : string option
  ; login_shell_path : string option
  ; auth : Auth.t
  ; account : string option
  ; problem : string option
  ; hint : string option
  ; checked_at : float
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind adapter = Json.string_field j "adapter" in
  let%bind label = Json.string_opt_field j "label" in
  let%bind host = Json.string_field j "host" in
  let%bind host_name = Json.string_opt_field j "host_name" in
  let%bind unknown = Json.string_opt_field j "unknown" in
  let installed : Installed.t =
    match Json.field j "installed" with
    | Some `True -> Yes
    | Some `False -> No
    | _ -> Unknown (Option.value unknown ~default:"")
  in
  let%bind version = Json.string_opt_field j "version" in
  let%bind path = Json.string_opt_field j "path" in
  let%bind login_shell_path = Json.string_opt_field j "login_shell_path" in
  let%bind auth = Json.string_opt_field j "auth" in
  let auth : Auth.t =
    match auth with
    | Some "logged_in" -> Logged_in
    | Some "logged_out" -> Logged_out
    | Some "not_needed" -> Not_needed
    | _ -> Unknown
  in
  let%bind account = Json.string_opt_field j "account" in
  let%bind problem = Json.string_opt_field j "problem" in
  let%bind hint = Json.string_opt_field j "hint" in
  let checked_at =
    Or_error.ok (Json.float_field j "checked_at") |> Option.value ~default:0.
  in
  return
    { adapter
    ; label = Option.value label ~default:adapter
    ; host
    ; host_name = Option.value host_name ~default:host
    ; installed
    ; version
    ; path
    ; login_shell_path
    ; auth
    ; account
    ; problem
    ; hint
    ; checked_at
    }
;;

let list_of_json j = Json.list_field j "adapters" ~f:of_json

let summary t =
  let installed =
    match t.installed, t.version with
    | Yes, Some v -> v
    | Yes, None -> "installed"
    | No, _ -> "not installed"
    | Unknown _, _ -> "unknown"
  in
  let auth =
    match t.auth, t.account with
    | Logged_in, Some account -> Some (sprintf "logged in (%s)" account)
    | Logged_in, None -> Some "logged in"
    | Logged_out, _ -> Some "not logged in"
    | (Not_needed | Unknown), _ -> None
  in
  String.concat ~sep:" · " (installed :: Option.to_list auth)
;;
