open! Core
open! Import

type t =
  { credential : Cli_credential.t
  ; resolved : Provider_auth.Resolved.t
  }

let sexp_of_t t = [%sexp { credential : Cli_credential.t = t.credential }]
let minute_ms = 60 * 1000

(* A new process gets a token valid for [valid_for]; it is replaced before a
   turn once fewer than [margin] remain. Google's last an hour. *)
let valid_for_ms = 40 * minute_ms
let margin_ms = 20 * minute_ms

(* Variables that would make agy sign in another way. *)
let competing =
  [ "GEMINI_API_KEY"
  ; "GOOGLE_API_KEY"
  ; "GOOGLE_APPLICATION_CREDENTIALS"
  ; "GOOGLE_GENAI_USE_VERTEXAI"
  ; "GOOGLE_CLOUD_PROJECT"
  ; "ANTIGRAVITY_PROJECT_ID"
  ]
;;

let fd_env = "PRIGH_AGY_TOKEN_FD"
let token_env = "PRIGH_AGY_TOKEN"

let wrapper =
  {|set -u
m=$(umask)
umask 077
base=${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}
{ [ -d "$base" ] && [ -w "$base" ]; } || base=/tmp
for old in "$base"/prigh-agy.*; do
  [ -d "$old" ] && [ -O "$old" ] || continue
  pid=${old#"$base"/prigh-agy.}
  pid=${pid%%.*}
  kill -0 "$pid" 2>/dev/null || rm -rf "$old"
done
d=$(mktemp -d "$base/prigh-agy.$$.XXXXXX") || exit 1
trap 'rm -rf "$d"' EXIT
g=$HOME/.gemini
mkdir -p "$g/antigravity-cli/conversations" "$g/antigravity-cli/brain" "$g/antigravity-cli/implicit" "$g/antigravity-cli/log" "$d/antigravity-cli"
for f in "$g"/* "$g"/.[!.]*; do
  { [ -e "$f" ] || [ -L "$f" ]; } || continue
  [ "${f##*/}" = antigravity-cli ] || ln -s "$f" "$d/"
done
for f in "$g/antigravity-cli"/* "$g/antigravity-cli"/.[!.]*; do
  { [ -e "$f" ] || [ -L "$f" ]; } || continue
  case ${f##*/} in
    antigravity-oauth-token*) ;;
    *) ln -s "$f" "$d/antigravity-cli/" ;;
  esac
done
t=$d/antigravity-cli/antigravity-oauth-token
if [ -n "${PRIGH_AGY_TOKEN_FD:-}" ]; then
  cat <&"$PRIGH_AGY_TOKEN_FD" >"$t"
  eval "exec $PRIGH_AGY_TOKEN_FD<&-"
  unset PRIGH_AGY_TOKEN_FD
else
  printf '%s\n' "${PRIGH_AGY_TOKEN:-}" >"$t"
fi
unset PRIGH_AGY_TOKEN
umask "$m"
DBUS_SESSION_BUS_ADDRESS=unix:path=/nonexistent/prigh-agy "$@" --gemini-dir "$d" <&0 &
c=$!
trap 'kill -TERM "$c" 2>/dev/null' TERM HUP INT
wait "$c"
s=$?
while kill -0 "$c" 2>/dev/null; do wait "$c"; s=$?; done
exit "$s"|}
;;

module Launch = struct
  type t =
    { argv : string list
    ; env_set : (string * string) list
    ; env_unset : string list
    ; secret : Adapter.Launch.Secret.t option
    }
end

let usable ~refused (resolved : Provider_auth.Resolved.t) =
  match refused, resolved.expires_ms with
  | Some refused, _ when String.equal refused resolved.token ->
    Error "prigh has no newer token than the one refused"
  | _, None -> Error "the login has no expiry, so it is not a Google login"
  | (Some _ | None), Some _ -> Ok resolved
;;

let fetch credential ~refused =
  Result.bind
    (Cli_credential.fetch credential ?refused ~valid_for_ms ()
     |> Result.map_error ~f:Error.to_string_hum)
    ~f:(usable ~refused)
;;

let launch credential ~refused ~command : Launch.t =
  match fetch credential ~refused with
  | Error _ -> { argv = command; env_set = []; env_unset = []; secret = None }
  | Ok resolved ->
    { argv = [ "bash"; "-c"; wrapper; "agy" ] @ command
    ; env_set = []
    ; env_unset = competing
    ; secret =
        Some
          { fd_env
          ; env = token_env
          ; value =
              Oauth_antigravity.agy_token_document
                ~access:resolved.token
                ~expires_ms:(Option.value_exn resolved.expires_ms)
          }
    }
;;

let own = "this session runs on agy's own login on its tool host"

let of_launch credential ~refused =
  let label = Cli_credential.label credential in
  match Cli_credential.last credential with
  | None ->
    Error (sprintf "prigh's Antigravity login %S was not asked: %s" label own)
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
            "prigh's Antigravity login %S has no token for agy (%s): %s (agy \
             acp --login there)."
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

let refused_hint t =
  sprintf
    "Google refused prigh's Antigravity login %S: prigh refreshes it and \
     restarts agy on your next message (/login google-antigravity if it is \
     refused again)"
    (Cli_credential.label t.credential)
;;

module For_testing = struct
  let wrapper = wrapper
end
