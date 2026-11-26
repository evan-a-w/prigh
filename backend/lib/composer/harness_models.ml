open! Core
open! Import

module Problem = struct
  type t =
    | Not_installed
    | Not_logged_in
    | Login_refused
    | Unreachable
    | Failed
  [@@deriving sexp_of, equal]

  let to_string = function
    | Not_installed -> "not_installed"
    | Not_logged_in -> "not_logged_in"
    | Login_refused -> "login_refused"
    | Unreachable -> "unreachable"
    | Failed -> "failed"
  ;;
end

module Fix = struct
  type t =
    | Login of { provider : string }
    | Engines
    | Retry
  [@@deriving sexp_of, equal]

  let to_json : t -> Json.t = function
    | Login { provider } ->
      `Object [ "kind", `String "login"; "provider", `String provider ]
    | Engines -> `Object [ "kind", `String "engines" ]
    | Retry -> `Object [ "kind", `String "retry" ]
  ;;
end

type t =
  | Loading
  | Failed of
      { problem : Problem.t
      ; summary : string
      ; message : string
      ; fixes : Fix.t list
      }
[@@deriving sexp_of, equal]

let says text ~any =
  let text = String.lowercase text in
  List.exists any ~f:(fun substring -> String.is_substring text ~substring)
;;

let problem ~prigh_login ~install why : Problem.t =
  let text = Error.to_string_hum why in
  match (install : Adapter_health.Install.t option) with
  | _ when says text ~any:[ "is not connected" ] -> Unreachable
  | Some (Not_installed _) -> Not_installed
  | _
    when says
           text
           ~any:[ "command not found"; "not installed"; "no such file or directory" ]
    -> Not_installed
  | _
    when Option.is_some prigh_login
         && says text ~any:[ "refused prigh's"; "gave it no token" ] ->
    Login_refused
  | _
    when Acp_adapter.is_not_logged_in why
         || says text ~any:[ "not logged in"; "authentication required"; "log in" ]
    -> Not_logged_in
  | Some (Installed _ | Unknown _) | None -> Failed
;;

let failed ~label ~host_name ~prigh_login ~install why =
  let problem = problem ~prigh_login ~install why in
  let summary =
    match problem with
    | Not_installed -> sprintf "not installed on %s" host_name
    | Not_logged_in -> sprintf "not logged in on %s" host_name
    | Login_refused -> "login refused"
    | Unreachable -> sprintf "%s not connected" host_name
    | Failed -> "couldn't list models"
  in
  let what_next =
    match problem, prigh_login with
    | Unreachable, _ -> Some "Start prigh's tool host there, or pick another host."
    | Not_installed, _ ->
      Some
        "Install it there (prigh never installs it), then retry; Settings → \
         Engines says what is missing."
    | Login_refused, Some _ -> None (* [why] says what to do *)
    | _, None ->
      Some "Check that it is installed and logged in there (Settings → Engines)."
    | _, Some _ -> Some "Check that it is installed there (Settings → Engines)."
  in
  let message =
    sprintf
      "%s on %s could not list its models: %s.%s"
      label
      host_name
      (Error.to_string_hum why)
      (Option.value_map what_next ~default:"" ~f:(fun s -> " " ^ s))
  in
  let fixes : Fix.t list =
    match problem, prigh_login with
    | Unreachable, _ -> [ Retry ]
    | Login_refused, Some provider -> [ Login { provider }; Engines; Retry ]
    | (Not_installed | Not_logged_in | Login_refused | Failed), _ ->
      [ Engines; Retry ]
  in
  Failed { problem; summary; message; fixes }
;;

let to_json : t -> Json.t = function
  | Loading -> `Object [ "state", `String "loading" ]
  | Failed { problem; summary; message; fixes } ->
    `Object
      [ "state", `String "failed"
      ; "problem", `String (Problem.to_string problem)
      ; "summary", `String summary
      ; "message", `String message
      ; "fixes", `Array (List.map fixes ~f:Fix.to_json)
      ]
;;
