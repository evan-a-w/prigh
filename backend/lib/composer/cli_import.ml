open! Core
open! Import

type exec = name:string -> Json.t -> Tool.Result.t

let cli_name = function
  | "codex" -> "Codex"
  | "claude" -> "Claude Code"
  | "acp:antigravity" -> "Antigravity"
  | a -> a
;;

module Found = struct
  type t =
    { adapter : string
    ; provider : Provider_id.t
    ; host : string
    ; path : string
    ; credential : (Credential.Oauth.t[@sexp.opaque])
    ; keyring : bool
    ; client : Oauth_antigravity.Client.t option
    }
  [@@deriving sexp_of]

  let label t = "imported from " ^ t.host
  let cli t = cli_name t.adapter

  let warning t =
    match t.provider, t.keyring with
    | Google_antigravity, true ->
      sprintf
        "Antigravity on %s will stop being logged in: prigh takes its Google \
         login from the keyring (service gemini, username antigravity) and \
         clears it there, as Google does not retire other copies when prigh \
         refreshes it. Its sessions there then run on prigh's login."
        t.host
    | Google_antigravity, false ->
      sprintf
        "Antigravity on %s will stop being logged in: prigh takes its Google \
         login from %s (renamed to %s.moved-to-prigh, its tokens blanked, as \
         Google does not retire other copies when prigh refreshes it). Its \
         sessions there then run on prigh's login."
        t.host
        t.path
        t.path
    | _ ->
      sprintf
        "%s on %s will stop being logged in: prigh takes its login from %s \
         (renamed to %s.moved-to-prigh) and refreshes it, which retires the \
         copy there. Its sessions there then run on prigh's login."
        (cli t)
        t.host
        t.path
        t.path
  ;;
end

let suffix = ".moved-to-prigh"

(* Prints the file's path, whether it is there, and (macOS) whether the
   keychain has Claude Code's login. *)
let locate_script = function
  | "codex" -> Some {|p="${CODEX_HOME:-$HOME/.codex}/auth.json"|}
  | "claude" ->
    Some
      {|p="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/.credentials.json"
if [ "$(uname)" = Darwin ] && security find-generic-password -s 'Claude Code-credentials' >/dev/null 2>&1; then k=keychain; fi|}
  | "acp:antigravity" ->
    Some
      (sprintf
         {|p="$HOME/.gemini/antigravity-cli/antigravity-oauth-token"
if [ ! -f "$p" ]; then
  if [ "$(uname)" = Darwin ]; then
    if security find-generic-password -s gemini -a antigravity >/dev/null 2>&1; then k=keychain; fi
  else
    k=$(
%s
)
  fi
fi
c=$(%s)|}
         Agy_keyring.script
         Oauth_antigravity.Client.find_script)
  | _ -> None
;;

let script body =
  body
  ^ {|
printf '%s\n' "$p"
if [ -f "$p" ]; then echo file; else echo none; fi
echo "${k:-}"
printf '%s' "${c:-}"|}
;;

let result_text (r : Tool.Result.t) =
  if r.is_error then Or_error.error_string r.text else Ok r.text
;;

let str json key =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal key with
     | Some (`String s) when not (String.is_empty s) -> Some s
     | _ -> None)
  | _ -> None
;;

let obj json key =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal key
  | _ -> None
;;

let oauth ~access ~refresh ~account_id =
  (* Expired, so that taking it refreshes it at once. *)
  { Credential.Oauth.access; refresh; expires_ms = 0; account_id }
;;

let parse_antigravity ~host ~where text =
  let open Or_error.Let_syntax in
  let%bind (token : Oauth_antigravity.Agy_token.t) =
    Oauth_antigravity.Agy_token.parse text
    |> Result.map_error ~f:(fun e ->
      Error.createf "%s on %s: %s" where host (Error.to_string_hum e))
  in
  match token with
  | { auth_method = Some m; _ } when not (String.equal m "consumer") ->
    Or_error.errorf
      "Antigravity on %s signs in with %S, not a personal Google login, which \
       is the one prigh takes: sign agy in with Google there, or use /login \
       google-antigravity"
      host
      m
  | { access; refresh = Some refresh; _ } ->
    Ok
      (oauth
         ~access:(Option.value access ~default:"")
         ~refresh
         ~account_id:None)
  | { refresh = None; _ } ->
    Or_error.errorf
      "%s on %s has no refresh token (agy is not signed in there): /login \
       google-antigravity logs prigh in"
      where
      host
;;

let parse ~adapter ~host ~path text =
  let open Or_error.Let_syntax in
  if String.equal adapter "acp:antigravity"
  then parse_antigravity ~host ~where:path text
  else (
    let%bind json =
      Json.parse text
      |> Result.map_error ~f:(fun _ ->
        Error.createf "%s on %s is not JSON: nothing imported" path host)
    in
    match adapter with
    | "codex" ->
      let tokens = Option.value (obj json "tokens") ~default:`Null in
      (match str tokens "access_token", str tokens "refresh_token" with
       | Some access, Some refresh ->
         Ok (oauth ~access ~refresh ~account_id:(str tokens "account_id"))
       | _ ->
         if Option.is_some (str json "OPENAI_API_KEY")
         then
           Or_error.errorf
             "Codex on %s is logged in with an API key, not ChatGPT: add the \
              key in prigh (/login openai) instead"
             host
         else
           Or_error.errorf
             "%s on %s has no ChatGPT login: /login openai-codex logs prigh in"
             path
             host)
    | _ ->
      let o = Option.value (obj json "claudeAiOauth") ~default:`Null in
      (match str o "accessToken", str o "refreshToken" with
       | Some access, Some refresh ->
         Ok (oauth ~access ~refresh ~account_id:None)
       | Some _, None ->
         Or_error.errorf
           "Claude Code on %s has a long-lived token (claude setup-token), \
            which cannot be refreshed or moved: /login anthropic logs prigh in"
           host
       | None, _ ->
         Or_error.errorf
           "%s on %s has no Claude login: /login anthropic logs prigh in"
           path
           host))
;;

let read ~exec ~path =
  let open Or_error.Let_syntax in
  let%bind reply =
    result_text
      (exec
         ~name:Host_files.read_op
         (`Object [ "path", `String path; "offset", `Number "0" ]))
  in
  let%bind json = Json.parse reply in
  match str json "data" with
  | None -> Ok ""
  | Some data ->
    (match Base64.decode data with
     | Ok s -> Ok s
     | Error _ -> Or_error.error_string "bad reply from the tool host")
;;

let login_google = "log prigh in with /login google-antigravity"

(* Antigravity's login is not in its token file: the keyring, maybe.
   [logged_in]: agy started a session on its own login there lately. *)
let find_in_keyring ~exec ~host ~path ~logged_in ~client probe =
  let open Or_error.Let_syntax in
  let couldnt_read (why : Agy_keyring.Unreachable.t) =
    let advice = Agy_keyring.Unreachable.advice why in
    let why = Agy_keyring.Unreachable.to_string why ~host in
    if logged_in
    then
      Or_error.errorf
        "Antigravity is logged in on %s, but prigh couldn't read its login \
         there: %s. To import it, %s; or %s"
        host
        why
        advice
        login_google
    else
      Or_error.errorf
        "prigh couldn't tell whether Antigravity is logged in on %s: its login \
         isn't in %s, and %s. To import it, %s; or %s"
        host
        path
        why
        advice
        login_google
  in
  match (probe : Agy_keyring.Probe.t) with
  | Found { readable = true } ->
    let%bind text =
      match
        exec ~name:"bash" (`Object [ "command", `String Agy_keyring.lookup ])
      with
      | { Tool.Result.is_error = false; text; _ }
        when not (String.is_empty (String.strip text)) -> Ok text
      | { text; _ } ->
        couldnt_read
          (Secret_tool
             (let text = String.strip text in
              if String.is_empty text then "it printed nothing" else text))
    in
    let%map credential =
      parse_antigravity ~host ~where:"The keyring's Antigravity login" text
    in
    { Found.adapter = "acp:antigravity"
    ; provider = Google_antigravity
    ; host
    ; path = "keyring"
    ; credential
    ; keyring = true
    ; client
    }
  | Found { readable = false } ->
    Or_error.errorf
      "Antigravity's login on %s is in the keyring, but prigh needs \
       secret-tool to read it and %s has neither secret-tool nor nix: install \
       libsecret there and Import again, or %s"
      host
      host
      login_google
  | Locked ->
    Or_error.errorf
      "Antigravity's login on %s is in a locked keyring: unlock it (log in to \
       the desktop session there) and Import again, or %s"
      host
      login_google
  | Empty when logged_in ->
    Or_error.errorf
      "Antigravity is logged in on %s, but not with a Google login prigh can \
       find (none in the keyring or %s; an API key or gcloud credentials?): %s"
      host
      path
      login_google
  | Empty ->
    Or_error.errorf
      "Antigravity on %s has no login in the keyring or %s: run `agy acp \
       --login` there, or %s"
      host
      path
      login_google
  | Unreachable why -> couldnt_read why
;;

let find ?(logged_in = false) ~exec ~adapter ~host () =
  let open Or_error.Let_syntax in
  let%bind provider, body =
    match Cli_login.provider ~adapter, locate_script adapter with
    | Some p, Some body -> Ok (p, body)
    | _ ->
      Or_error.errorf
        "prigh imports only the logins of: %s"
        (String.concat ~sep:", " Cli_login.adapters)
  in
  let%bind out =
    exec ~name:"bash" (`Object [ "command", `String (script body) ])
    |> result_text
    |> Result.map_error ~f:(fun e ->
      Error.tag e ~tag:(sprintf "can't look for %s's login on %s" adapter host))
  in
  let%bind path, exists, rest =
    match String.split_lines out with
    | path :: exists :: rest -> Ok (path, String.equal exists "file", rest)
    | _ -> Or_error.errorf "bad reply from %s: %S" host out
  in
  let keychain = List.exists rest ~f:(String.equal "keychain") in
  let keyring = List.find_map rest ~f:Agy_keyring.Probe.of_line in
  let client =
    Oauth_antigravity.Client.of_candidates
      (List.filter_map rest ~f:(String.chop_prefix ~prefix:"client "))
  in
  match exists, keyring with
  | true, _ ->
    let%bind text = read ~exec ~path in
    let%map credential = parse ~adapter ~host ~path text in
    { Found.adapter; provider; host; path; credential; keyring = false; client }
  | false, Some probe ->
    find_in_keyring ~exec ~host ~path ~logged_in ~client probe
  | false, None ->
    if keychain && Provider_id.equal provider Google_antigravity
    then
      Or_error.errorf
        "Antigravity on %s keeps its login in the macOS keychain, which prigh \
         does not read: /login google-antigravity logs prigh in instead"
        host
    else if keychain
    then
      Or_error.errorf
        "Claude Code on %s keeps its login in the macOS keychain, which prigh \
         does not read: /login anthropic logs prigh in instead"
        host
    else if Provider_id.equal provider Google_antigravity
    then
      (* macOS without a keychain item, or a host that did not say *)
      if logged_in
      then
        Or_error.errorf
          "Antigravity is logged in on %s, but prigh couldn't find its login \
           there (not in %s): %s"
          host
          path
          login_google
      else
        Or_error.errorf
          "Antigravity on %s has no login in %s: run `agy acp --login` there, \
           or %s"
          host
          path
          login_google
    else
      Or_error.errorf
        "%s is not logged in on %s (no %s): log prigh in with /login %s"
        (cli_name adapter)
        host
        path
        (Provider_id.to_string provider)
;;

let take ~exec ~refresh store (found : Found.t) =
  let open Or_error.Let_syntax in
  let%bind (fresh : Credential.Oauth.t) =
    refresh found.provider ~refresh_token:found.credential.refresh
    |> Result.map_error ~f:(fun e ->
      Error.tag
        e
        ~tag:
          (sprintf
             "%s's login on %s could not be refreshed, so nothing was moved"
             (Found.cli found)
             found.host))
  in
  let (fresh : Credential.Oauth.t) =
    { fresh with
      account_id =
        Option.first_some fresh.account_id found.credential.account_id
    }
  in
  let%map id =
    Provider_accounts.save_login
      ~label:(Found.label found)
      store
      found.provider
      (Oauth fresh)
  in
  let bash command =
    exec ~name:"bash" (`Object [ "command", `String command ])
  in
  let (moved : Tool.Result.t) =
    match found.provider, found.keyring with
    | Google_antigravity, true -> bash Agy_keyring.clear
    | Google_antigravity, false ->
      (* Google's refresh tokens do not rotate: the renamed copy keeps a
         note, not the tokens. *)
      let target = found.path ^ suffix in
      bash
        (sprintf
           "mv -f %s %s && printf '%%s\\n' %s > %s"
           (Filename.quote found.path)
           (Filename.quote target)
           (Filename.quote {|{"moved_to_prigh":true}|})
           (Filename.quote target))
    | _ ->
      exec
        ~name:Host_files.rename_op
        (`Object
            [ "from", `String found.path
            ; "to", `String (found.path ^ suffix)
            ; "overwrite", `True
            ])
  in
  ( id
  , if moved.is_error
    then
      Some
        (match found.provider, found.keyring with
         | Google_antigravity, true ->
           sprintf
             "the keyring on %s could not be cleared (%s): run `%s` there, as \
              that copy still works"
             found.host
             moved.text
             Agy_keyring.clear_by_hand
         | Google_antigravity, false ->
           sprintf
             "%s on %s could not be moved (%s): delete it there, as that copy \
              still works"
             found.path
             found.host
             moved.text
         | _ ->
           sprintf
             "%s on %s could not be renamed (%s): delete it there, it no \
              longer works"
             found.path
             found.host
             moved.text)
    else None )
;;

module For_testing = struct
  let locate_command adapter = Option.map (locate_script adapter) ~f:script
end
