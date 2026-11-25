open! Core
open! Import

(** Claude Code on one of prigh's Anthropic logins ({!Cli_credential}): a
    Claude Pro/Max login's access token in [CLAUDE_CODE_OAUTH_TOKEN], or an
    API key in [ANTHROPIC_API_KEY], in the process's environment (and so in
    that of the commands it runs: Claude Code reads no token from anywhere
    prigh could hand one to without its host's prigh's help). The refresh
    token stays with prigh. Claude Code takes the token only when it
    starts: before a turn, when it expires within an hour, the process is
    replaced, its thread resumed ({!Adapter.Login_renewal}). *)

type t [@@deriving sexp_of]

(** The variables to set and unset for the process: [credential]'s token,
    valid for an hour at least ([refused]: the last process was refused with
    that one, so a newer one), and none of the variables that would take
    precedence over it. Without a token (prigh has none, or none newer than
    [refused]): nothing, and the process runs on Claude Code's own login. *)
val env
  :  Cli_credential.t
  -> refused:string option
  -> (string * string) list * string list

(** What {!env} handed the process, read back from [credential]; [Error]:
    nothing, saying why the session runs on Claude Code's own login. *)
val of_launch
  :  Cli_credential.t
  -> refused:string option
  -> (t, string) Result.t

val credential : t -> Cli_credential.t

(** The token handed. *)
val token : t -> string

(** Whether the token expires within an hour of [now_ms] (Unix ms). *)
val expiring : t -> now_ms:int -> bool

(** Claude Code says ([apiKeySource]) it uses another credential, which takes
    precedence over the token ([apiKeyHelper] in its settings, an
    [ANTHROPIC_API_KEY] in its settings' [env]): why it does not run on
    prigh's login, and what to do. *)
val overridden : t -> api_key_source:string option -> string option

(** A turn was refused with the token: what the user is told. *)
val refused_hint : t -> string
