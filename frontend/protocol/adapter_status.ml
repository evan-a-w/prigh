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

module Via = struct
  type t =
    | Status_command
    | Prigh_login of string
    | Session of
        { at : float
        ; ago : string option
        }
    | Login_file of string
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
  ; via : Via.t option
  ; checking : bool
  ; checkable : bool
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
  let%bind via = Json.string_opt_field j "via" in
  let%bind prigh_login = Json.string_opt_field j "prigh_login" in
  let%bind login_file = Json.string_opt_field j "login_file" in
  let%bind ago = Json.string_opt_field j "confirmed" in
  let confirmed_at = Or_error.ok (Json.float_field j "confirmed_at") in
  let via : Via.t option =
    match via, prigh_login, confirmed_at, login_file with
    | Some "status", _, _, _ -> Some Status_command
    | Some "prigh_login", Some label, _, _ -> Some (Prigh_login label)
    | Some "session", _, Some at, _ -> Some (Session { at; ago })
    | Some "login_file", _, _, Some path -> Some (Login_file path)
    | _ -> None
  in
  let flag name = Option.equal Json.equal (Json.field j name) (Some `True) in
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
    ; via
    ; checking = flag "checking"
    ; checkable = flag "checkable"
    ; problem
    ; hint
    ; checked_at
    }
;;

let list_of_json j = Json.list_field j "adapters" ~f:of_json

(* How we know it is logged in, for one without an account. *)
let logged_in t =
  match t.via with
  | Some (Prigh_login label) -> "logged in (prigh login " ^ label ^ ")"
  | Some (Session { ago = Some ago; _ }) -> "logged in (confirmed " ^ ago ^ ")"
  | Some (Login_file _) -> "logged in (token file found)"
  | Some (Status_command | Session { ago = None; _ }) | None -> "logged in"
;;

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
    | Logged_in, None -> Some (logged_in t)
    | Logged_out, _ -> Some "not logged in"
    | Unknown, _ when t.checking -> Some "checking login…"
    | Unknown, _ when t.checkable -> Some "login not checked yet"
    | (Not_needed | Unknown), _ -> None
  in
  String.concat ~sep:" · " (installed :: Option.to_list auth)
;;
