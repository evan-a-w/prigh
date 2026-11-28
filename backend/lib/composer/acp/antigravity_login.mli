open! Core
open! Import

(** Antigravity ([agy acp]) on one of prigh's Google logins
    ({!Cli_credential}). agy reads its login from the OS keyring or, failing
    that, from [antigravity-oauth-token] in its [--gemini-dir]; it has no
    variable or ACP method that takes a token. So prigh runs it through a
    small bash wrapper that:
    - makes a private temporary directory ([mktemp -d] under
      [$XDG_RUNTIME_DIR], else [$TMPDIR] or [/tmp]) holding links to every
      entry of the host's [~/.gemini] and [~/.gemini/antigravity-cli] (its
      conversations, so sessions resume, settings, MCP config) but the token;
    - writes there, mode 600, a token document with the access token only,
      read from the descriptor prigh hands it (or, on an older tool host,
      from a variable it then unsets): agy can use the token but not renew
      it, and the refresh token stays with prigh;
    - runs [agy ... --gemini-dir DIR] with the keyring unreachable
      ([DBUS_SESSION_BUS_ADDRESS] pointing nowhere), so a login of the host's
      own does not take precedence;
    - deletes the directory when agy exits (and, at its next start, any a
      killed one left).

    Google's access tokens last an hour: the process is replaced (its session
    resumed) before a turn when its token expires within twenty minutes
    ({!Adapter.Login_renewal}). *)

type t [@@deriving sexp_of]

module Launch : sig
  type t =
    { argv : string list
    ; env_set : (string * string) list
    ; env_unset : string list
    ; secret : Adapter.Launch.Secret.t option
    }
end

(** How to run [command] ([agy acp]) on [credential]'s token, valid for
    forty minutes at least ([refused]: the last process was refused with
    that one, so a newer one). Without a token: [command] as it is, on agy's
    own login. *)
val launch
  :  Cli_credential.t
  -> refused:string option
  -> command:string list
  -> Launch.t

(** How to run [command] ([agy acp]) on the backend with its built-in tools
    disabled (using a generated custom agent with
    [excludeDefaultComponents: true], [inheritCustomizations: false],
    [tools: []], [--include-default-customizations=false], and
    [--dangerously-skip-permissions]), an isolated empty working directory,
    and either prigh's [credential] token (when [Some] and usable) or the
    backend's own agy login. *)
val launch_backend
  :  credential:Cli_credential.t option
  -> refused:string option
  -> command:string list
  -> Launch.t

(** What {!launch} handed the process, read back from [credential];
    [Error]: nothing, saying why the session runs on agy's own login. *)
val of_launch
  :  Cli_credential.t
  -> refused:string option
  -> (t, string) Result.t

val credential : t -> Cli_credential.t

(** The access token handed. *)
val token : t -> string

(** Whether the token expires within twenty minutes of [now_ms]. *)
val expiring : t -> now_ms:int -> bool

(** A turn was refused with the token: what the user is told. *)
val refused_hint : t -> string

module For_testing : sig
  val wrapper : string
  val backend_wrapper : string
  val backend_agent_name : string
end
