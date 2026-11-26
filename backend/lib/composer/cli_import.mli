open! Core
open! Import

(** Moving a tool host's own CLI login (Codex's [auth.json], Claude Code's
    [.credentials.json]) into prigh, so its sessions run on one of prigh's
    logins ({!Cli_login}): read through the host's operations, refreshed at
    once (the refresh token rotates, so the host's copy stops working, and
    prigh owns the login), stored as a new login, and the host's file
    renamed to [*.moved-to-prigh]. The CLI's logout is never run: it would
    revoke the tokens. *)

(** Runs a host operation (a tool or [$fs_*]) on the chosen host. *)
type exec = name:string -> Json.t -> Tool.Result.t

module Found : sig
  type t =
    { adapter : string
    ; provider : Provider_id.t
    ; host : string (** the host's name, for messages and the label *)
    ; path : string (** the credentials file on the host *)
    ; credential : Credential.Oauth.t
    ; keyring : bool (** Antigravity's, in the Linux keyring, not [path] *)
    ; client : Oauth_antigravity.Client.t option
      (** Antigravity's OAuth client, found in the host's agy *)
    }
  [@@deriving sexp_of]

  (** The login's label in prigh: the host's name, ["laptop"]. *)
  val label : t -> string

  (** What the user consents to. *)
  val warning : t -> string
end

(** Finds and reads [adapter]'s login on the host ([CODEX_HOME],
    [CLAUDE_CONFIG_DIR] respected; Antigravity's in its token file or the
    Linux keyring, {!Agy_keyring}), saying what to do when there is none, it
    is an API key, (macOS) only the keychain has it or the keyring cannot be
    read. [logged_in]: the CLI started a session on its own login on that
    host lately, so a login prigh cannot find is not called missing. *)
val find
  :  ?logged_in:bool
  -> exec:exec
  -> adapter:string
  -> host:string
  -> unit
  -> Found.t Or_error.t

(** Takes the login: refreshes it ([refresh]: the provider's token
    endpoint), stores it as a new login labelled {!Found.label} (or the
    login of the same account, logged in again) and renames the host's
    file. A failed rename is reported in the answer's [note] (the login is
    prigh's by then). *)
val take
  :  exec:exec
  -> refresh:
       (Provider_id.t -> refresh_token:string -> Credential.Oauth.t Or_error.t)
  -> Auth_store.t
  -> Found.t
  -> (Account_id.t * string option) Or_error.t

module For_testing : sig
  (** The shell command that prints where [adapter]'s login file is,
      whether it exists, and [keychain] when only macOS's keychain has it. *)
  val locate_command : string -> string option
end
