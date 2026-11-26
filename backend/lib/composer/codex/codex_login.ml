open! Core
open! Import

type t =
  { credential : Cli_credential.t
  ; mutable handed : (string * int option) option
    (** the access token last handed, and when it expires *)
  }

let sexp_of_t t = [%sexp_of: Cli_credential.t] t.credential
let create credential = { credential; handed = None }
let credential t = t.credential
let margin_ms = 10 * 60 * 1000

let tokens t ~refused =
  let open Or_error.Let_syntax in
  let refused = if refused then Option.map t.handed ~f:fst else None in
  let%bind resolved =
    Cli_credential.fetch t.credential ?refused ~valid_for_ms:margin_ms ()
  in
  let%map account_id =
    match resolved.account_id with
    | Some id -> Ok id
    | None ->
      Or_error.errorf
        "ChatGPT login %S has no account id: log in again (/login openai-codex)"
        (Cli_credential.label t.credential)
  in
  t.handed <- Some (resolved.token, resolved.expires_ms);
  { Codex_codec.Chatgpt_tokens.access_token = resolved.token
  ; account_id
  ; plan = Oauth_openai_codex.plan_of_token resolved.token
  }
;;

let expiring t ~now_ms =
  match t.handed with
  | Some (_, Some expires_ms) -> now_ms + margin_ms >= expires_ms
  | Some (_, None) | None -> false
;;

let fallback_notice t ~version why =
  let label = Cli_credential.label t.credential in
  let own =
    "this session runs on Codex's own login on its tool host (codex login \
     there)"
  in
  match why with
  | `Refused reason ->
    sprintf
      "%s did not take prigh's ChatGPT login %S (%s): %s. Update Codex there \
       (npm install -g @openai/codex) to run on prigh's, or choose Codex's own \
       login in Settings → Engines."
      (match version with
       | Some v -> "Codex " ^ v
       | None -> "Codex")
      label
      reason
      own
  | `No_token e ->
    sprintf
      "prigh's ChatGPT login %S has no token for Codex (%s): %s."
      label
      (Error.to_string_hum e)
      own
;;
