open! Core
open! Import

(** [prigh desktop]: the process the desktop app's window (the Tauri shell
    in [desktop/]) runs and shows. Like [prigh web] it serves prigh-web on
    this machine for a backend elsewhere, relayed, with the page signed in
    and a {!Tool_host} running this machine's tools; unlike it, it starts
    without a working connection too: the page then shows a connect form,
    and the connection typed there is checked, saved and used.

    The page's URL carries a per-run key in its fragment ([#desktop=KEY]),
    which the page keeps (in sessionStorage) and sends with every request to
    the [/desktop/] endpoints below, so that no other local process or web
    page can redirect this machine's tool host to another backend or stop
    the app. Each is a [POST] of a JSON object with ["key"]; a wrong key gets
    403:

    - [/desktop/status]: once any check under way is done,
      [{"state": "connect", "address", "user", "error"}] (the form's
      prefill, each a string or null) or
      [{"state": "connected", "address", "code"}] ([code] the
      {!Signin_handoff}'s, null once traded);
    - [/desktop/connect] with ["address"], ["user"] and ["token"]: checks
      them ({!Web_connect.Check}), and if the backend accepts them saves
      them and answers [{"ok": true, "code"}], else
      [{"ok": false, "error"}] (what is wrong and what to do);
    - [/desktop/quit]: answers [{"ok": true}] and ends {!run}. *)

module Initial : sig
  type t =
    | Form of
        { address : string option
        ; user : string option
        ; error : string option
        } (** no connection to try: the form, prefilled *)
    | Check of
        { remote : Backend_address.t
        ; credentials : Web_connect.Credentials.t
        ; source : Web_connect.Check.Source.t
        ; save : Saved_connection.t option (** once it works *)
        }
end

module Config : sig
  type t =
    { initial : Initial.t
    ; tools : Web_connect.Tools.t
    ; name : string (** the tool host's *)
    ; host_id : string
    ; cwd : string (** the tool host's *)
    ; listen : Web_connect.Listen.t
    ; root : string (** the built site *)
    ; save : Saved_connection.t -> unit Or_error.t
    }
end

module Ended : sig
  type t =
    | Quit (** the page's Quit *)
    | Stdin_closed (** the window that ran us is gone *)
  [@@deriving sexp_of]
end

(** [/desktop/] *)
val path_prefix : string

(** Serves the site, calls [on_url] with the page's URL once listening, and
    runs until the page quits or [stdin] reaches its end; then stops the
    site and the tool host and returns. Raises what listening raises. *)
val run
  :  env:Env.t
  -> ?tool_host_wait:Time_ns.Span.t (** default 10s *)
  -> ?check_timeout:Time_ns.Span.t (** default 15s *)
  -> log:(string -> unit)
  -> on_url:(string -> unit)
  -> stdin:_ Eio.Flow.source
  -> Config.t
  -> Ended.t
