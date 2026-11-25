open! Core
open! Import

type t =
  { credential : Cli_credential.t
  ; resolved : Provider_auth.Resolved.t
  }

let sexp_of_t t =
  [%sexp
    { credential : Cli_credential.t = t.credential
    ; method_ : Login_method.t = t.resolved.method_
    }]
;;

let margin_ms = 60 * 60 * 1000

(* Every variable Claude Code prefers to the one prigh sets. *)
let competing =
  [ "ANTHROPIC_API_KEY"; "ANTHROPIC_AUTH_TOKEN"; "CLAUDE_CODE_OAUTH_TOKEN" ]
;;

let variable : Login_method.t -> string = function
  | Oauth -> "CLAUDE_CODE_OAUTH_TOKEN"
  | Api_key -> "ANTHROPIC_API_KEY"
;;

let usable ~refused (resolved : Provider_auth.Resolved.t) =
  match refused with
  | Some refused when String.equal refused resolved.token ->
    Error "prigh has no newer token than the one refused"
  | Some _ | None -> Ok resolved
;;

let env credential ~refused =
  match
    Result.bind
      (Cli_credential.fetch credential ?refused ~valid_for_ms:margin_ms ()
       |> Result.map_error ~f:Error.to_string_hum)
      ~f:(usable ~refused)
  with
  | Error _ -> [], []
  | Ok resolved ->
    let name = variable resolved.method_ in
    ( [ name, resolved.token ]
    , List.filter competing ~f:(fun v -> not (String.equal v name)) )
;;

let own =
  "this session runs on Claude Code's own login on its tool host (claude \
   /login there)"
;;

let of_launch credential ~refused =
  let label = Cli_credential.label credential in
  match Cli_credential.last credential with
  | None ->
    Error (sprintf "prigh's Anthropic login %S was not asked: %s" label own)
  | Some last ->
    (match
       Result.bind
         (Result.map_error last ~f:Error.to_string_hum)
         ~f:(usable ~refused)
     with
     | Ok resolved -> Ok { credential; resolved }
     | Error why ->
       Error
         (sprintf
            "prigh's Anthropic login %S has no token for Claude Code (%s): %s."
            label
            why
            own))
;;

let credential t = t.credential
let token t = t.resolved.token

let expiring t ~now_ms =
  match t.resolved.expires_ms with
  | Some expires_ms -> now_ms + margin_ms >= expires_ms
  | None -> false
;;

let overridden t ~api_key_source =
  let expected =
    match t.resolved.method_ with
    | Oauth -> [ "none" ]
    | Api_key -> [ "none"; "ANTHROPIC_API_KEY" ]
  in
  match api_key_source with
  | None -> None
  | Some source when List.mem expected source ~equal:String.equal -> None
  | Some source ->
    Some
      (sprintf
         "Claude Code uses its %s instead of prigh's Anthropic login %S, which \
          it prefers to a token handed to it: remove %s from its settings \
          (~/.claude/settings.json on its tool host), or choose Claude Code's \
          own login in Settings → Engines."
         source
         (Cli_credential.label t.credential)
         source)
;;

let refused_hint t =
  sprintf
    "Anthropic refused prigh's login %S: prigh refreshes it and restarts \
     Claude Code on your next message (/login anthropic if it is refused \
     again)"
    (Cli_credential.label t.credential)
;;
