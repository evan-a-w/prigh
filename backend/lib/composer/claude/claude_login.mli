open! Core
open! Import

(** Claude Code on one of prigh's Anthropic logins ({!Cli_credential}): a
    Claude Pro/Max login's access token on a descriptor named by
    [CLAUDE_CODE_OAUTH_TOKEN_FILE_DESCRIPTOR] (Claude Code reads it when it
    starts and closes it; on a tool host whose prigh cannot hand one, in
    [CLAUDE_CODE_OAUTH_TOKEN], which the commands it runs inherit), or an API
    key in [ANTHROPIC_API_KEY] (Claude Code reads its descriptor variant only
    in Anthropic's remote sessions). The refresh token stays with prigh.
    Claude Code takes the token only when it starts: before a turn, when it
    expires within an hour, the process is replaced, its thread resumed
    ({!Adapter.Login_renewal}). *)

type t [@@deriving sexp_of]

module Env : sig
  type t =
    { set : (string * string) list
    ; unset : string list
    ; secret : Adapter.Launch.Secret.t option
    }
end

(** The variables to set and unset for the process and the secret it gets
    ({!Adapter.Launch.Secret}): [credential]'s token,
    valid for an hour at least ([refused]: the last process was refused with
    that one, so a newer one), and none of the variables that would take
    precedence over it. Without a token (prigh has none, or none newer than
    [refused]): nothing, and the process runs on Claude Code's own login. *)
val env : Cli_credential.t -> refused:string option -> Env.t

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
