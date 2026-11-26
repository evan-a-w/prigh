open! Core
open! Import

module Auth = struct
  type t =
    | Logged_in of
        { method_ : string option
        ; email : string option
        }
    | Logged_out
    | Unknown of string
  [@@deriving sexp_of, equal]
end

let parse_version text =
  String.split (String.strip text) ~on:' '
  |> List.find ~f:(fun word ->
    (not (String.is_empty word))
    && Char.is_digit word.[0]
    && String.contains word '.')
;;

let parse_auth_status ~stdout ~exit_code : Auth.t =
  let str json key = Option.bind (Json.member key json) ~f:Json.string in
  match Json.parse (String.strip stdout) with
  | Ok json ->
    let method_ = str json "authMethod" in
    let logged_in =
      match Option.bind (Json.member "loggedIn" json) ~f:Json.bool, method_ with
      | Some b, _ -> b
      | None, Some "none" -> false
      | None, Some _ -> true
      | None, None -> exit_code = 0
    in
    if logged_in
    then Logged_in { method_; email = str json "email" }
    else Logged_out
  | Error _ ->
    (match exit_code with
     | 1 -> Logged_out
     | _ -> Unknown (String.prefix (String.strip stdout) 200))
;;

let problem ~host ~version ~(auth : Auth.t) =
  match version, auth with
  | None, _ ->
    Some
      (sprintf
         "Claude Code is not installed on %s: install it there (e.g. `npm \
          install -g @anthropic-ai/claude-code`), then try again."
         host)
  | Some _, Logged_out ->
    Some
      (sprintf
         "Claude Code is not logged in on %s: run `claude auth login` there, \
          then try again."
         host)
  | Some _, (Logged_in _ | Unknown _) -> None
;;
