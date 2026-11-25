open! Core
open! Import

type exec = name:string -> Json.t -> Tool.Result.t

module Found = struct
  type t =
    { adapter : string
    ; provider : Provider_id.t
    ; host : string
    ; path : string
    ; credential : (Credential.Oauth.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let label t = "imported from " ^ t.host

  let cli t =
    match t.adapter with
    | "codex" -> "Codex"
    | "claude" -> "Claude Code"
    | a -> a
  ;;

  let warning t =
    sprintf
      "%s on %s will stop being logged in: prigh takes its login from %s \
       (renamed to %s.moved-to-prigh) and refreshes it, which retires the copy \
       there. Its sessions there then run on prigh's login."
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
  | _ -> None
;;

let script body =
  body
  ^ {|
printf '%s\n' "$p"
if [ -f "$p" ]; then echo file; else echo none; fi
echo "${k:-}"|}
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

let parse ~adapter ~host ~path text =
  let open Or_error.Let_syntax in
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
           "Codex on %s is logged in with an API key, not ChatGPT: add the key \
            in prigh (/login openai) instead"
           host
       else
         Or_error.errorf
           "%s on %s has no ChatGPT login: /login openai-codex logs prigh in"
           path
           host)
  | _ ->
    let o = Option.value (obj json "claudeAiOauth") ~default:`Null in
    (match str o "accessToken", str o "refreshToken" with
     | Some access, Some refresh -> Ok (oauth ~access ~refresh ~account_id:None)
     | Some _, None ->
       Or_error.errorf
         "Claude Code on %s has a long-lived token (claude setup-token), which \
          cannot be refreshed or moved: /login anthropic logs prigh in"
         host
     | None, _ ->
       Or_error.errorf
         "%s on %s has no Claude login: /login anthropic logs prigh in"
         path
         host)
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

let find ~exec ~adapter ~host =
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
  let%bind path, exists, keychain =
    match String.split_lines out with
    | path :: exists :: rest ->
      Ok
        ( path
        , String.equal exists "file"
        , List.exists rest ~f:(String.equal "keychain") )
    | _ -> Or_error.errorf "bad reply from %s: %S" host out
  in
  if not exists
  then
    if keychain
    then
      Or_error.errorf
        "Claude Code on %s keeps its login in the macOS keychain, which prigh \
         does not read: /login anthropic logs prigh in instead"
        host
    else
      Or_error.errorf
        "%s is not logged in on %s (no %s): log prigh in with /login %s"
        adapter
        host
        path
        (Provider_id.to_string provider)
  else (
    let%bind text = read ~exec ~path in
    let%map credential = parse ~adapter ~host ~path text in
    { Found.adapter; provider; host; path; credential })
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
  let (renamed : Tool.Result.t) =
    exec
      ~name:Host_files.rename_op
      (`Object
          [ "from", `String found.path
          ; "to", `String (found.path ^ suffix)
          ; "overwrite", `True
          ])
  in
  ( id
  , if renamed.is_error
    then
      Some
        (sprintf
           "%s on %s could not be renamed (%s): delete it there, it no longer \
            works"
           found.path
           found.host
           renamed.text)
    else None )
;;

module For_testing = struct
  let locate_command adapter = Option.map (locate_script adapter) ~f:script
end
