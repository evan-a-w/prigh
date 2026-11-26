open! Core
open! Import

module Unreachable = struct
  type t =
    | No_bus
    | No_tool
    | Busctl of string
    | Secret_tool of string
  [@@deriving sexp_of, equal]

  let to_string t ~host =
    match t with
    | No_bus ->
      sprintf
        "prigh's tool host on %s has no D-Bus session bus to reach the keyring \
         (DBUS_SESSION_BUS_ADDRESS is unset and /run/user/UID/bus is missing)"
        host
    | No_tool ->
      sprintf
        "neither secret-tool (libsecret) nor busctl is installed on %s to read \
         the keyring"
        host
    | Busctl why ->
      sprintf
        "secret-tool isn't installed on %s and busctl couldn't reach the \
         Secret Service (%s)"
        host
        why
    | Secret_tool why ->
      sprintf "secret-tool couldn't read the keyring on %s (%s)" host why
  ;;

  let advice = function
    | No_bus ->
      "run prigh's tool host from the desktop session there (or log in to it) \
       and Import again"
    | No_tool -> "install libsecret (secret-tool) there and Import again"
    | Busctl _ ->
      "install libsecret (secret-tool) there, check that a keyring (GNOME \
       Keyring, KWallet) runs, and Import again"
    | Secret_tool _ ->
      "check that the keyring runs and is unlocked there, and Import again"
  ;;
end

module Probe = struct
  type t =
    | Found of { readable : bool }
    | Empty
    | Locked
    | Unreachable of Unreachable.t
  [@@deriving sexp_of, equal]

  let of_line line =
    let word, rest =
      match String.lsplit2 (String.strip line) ~on:' ' with
      | Some (w, r) -> w, String.strip r
      | None -> String.strip line, ""
    in
    match word with
    | "keyring" -> Some (Found { readable = not (String.equal rest "-") })
    | "keyring-empty" -> Some Empty
    | "keyring-locked" -> Some Locked
    | "keyring-error" ->
      let kind, why =
        match String.lsplit2 rest ~on:' ' with
        | Some (k, w) -> k, String.strip w
        | None -> rest, ""
      in
      let why = if String.is_empty why then "no answer" else why in
      Some
        (Unreachable
           (match kind with
            | "nobus" -> No_bus
            | "busctl" -> Busctl why
            | "secret-tool" -> Secret_tool why
            | _ -> No_tool))
    | _ -> None
  ;;
end

let prelude =
  {|have() { command -v "$1" >/dev/null 2>&1; }
if [ -z "${DBUS_SESSION_BUS_ADDRESS:-}" ] && [ -e "/run/user/$(id -u)/bus" ]; then
  export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"
fi
st() {
  if have secret-tool; then secret-tool "$@"
  else timeout 120 nix --extra-experimental-features 'nix-command flakes' shell nixpkgs#libsecret -c secret-tool "$@"; fi
}
|}
;;

let script =
  prelude
  ^ {|one() { printf '%s' "$1" | tr '\n' ' '; }
if have secret-tool; then rd=secret-tool; elif have nix; then rd=nix; else rd=-; fi
if [ -z "${DBUS_SESSION_BUS_ADDRESS:-}" ]; then echo "keyring-error nobus"
elif have busctl; then
  if r=$(busctl --user call org.freedesktop.secrets /org/freedesktop/secrets org.freedesktop.Secret.Service SearchItems 'a{ss}' 2 service gemini username antigravity 2>&1); then
    set -- $r
    n=${2:-0}; shift 2 2>/dev/null; shift "$n" 2>/dev/null
    if [ "$n" -gt 0 ] 2>/dev/null; then echo "keyring $rd"
    elif [ "${1:-0}" -gt 0 ] 2>/dev/null; then echo keyring-locked
    else echo keyring-empty; fi
  elif [ "$rd" = secret-tool ]; then
    if e=$(st lookup service gemini username antigravity 2>&1 >/dev/null); then echo "keyring $rd"
    elif [ -z "$e" ]; then echo keyring-empty
    else echo "keyring-error secret-tool $(one "$e")"; fi
  else echo "keyring-error busctl $(one "$r")"; fi
elif [ "$rd" != - ]; then
  if e=$(st lookup service gemini username antigravity 2>&1 >/dev/null); then echo "keyring $rd"
  elif [ -z "$e" ]; then echo keyring-empty
  else echo "keyring-error secret-tool $(one "$e")"; fi
else echo "keyring-error none"; fi|}
;;

let lookup = prelude ^ "st lookup service gemini username antigravity"
let clear = prelude ^ "st clear service gemini username antigravity"
let clear_by_hand = "secret-tool clear service gemini username antigravity"
