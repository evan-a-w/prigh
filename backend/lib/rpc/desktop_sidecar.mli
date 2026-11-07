open! Core
open! Import

(** [prigh desktop]: the process the desktop app's window (the Tauri shell
    in [desktop/]) runs and shows, with a {!Tool_host} running this
    machine's tools for the backend. The window shows either the backend's
    own prigh-web ({!Ui.Remote}: the user signs in there) or, like
    [prigh web], prigh-web served on this machine, relayed and signed in
    ({!Ui.Local}). Unlike [prigh web] it starts without a working
    connection too: the window then shows a connect form served here, and
    the connection typed there is checked, saved and used.

    It says where the window should be as JSON lines on stdout
    ({!Page.t}): first the page here (or, under {!Ui.Remote} with a
    connection that works at once, the backend's), then the backend's once
    the form has connected. The backend's URL's fragment is
    [#app=desktop&host=ID]: the page then offers Quit (by going to
    [about:blank#prigh-desktop-quit], which the window takes as quitting)
    and prefers this machine's tool host ([ID]; absent with
    [-tools remote]).

    The page's URL carries a per-run key in its fragment ([#desktop=KEY]),
    which the page keeps (in sessionStorage) and sends with every request to
    the [/desktop/] endpoints below, so that no other local process or web
    page can redirect this machine's tool host to another backend or stop
    the app. Each is a [POST] of a JSON object with ["key"] ([Content-Type:
    application/json], so another page's request needs a CORS preflight,
    which is refused, as is any request from another origin or under another
    host name: {!Local_site}); a wrong key gets 403:

    - [/desktop/status]: once any check under way is done,
      [{"state": "connect", "address", "user", "error"}] (the form's
      prefill, each a string or null),
      [{"state": "connected", "address", "code"}] ([code] the
      {!Signin_handoff}'s, null once traded) or, under {!Ui.Remote},
      [{"state": "remote", "url"}] (the window is on its way there) or
      [{"state": "signin", "site", "login", "waiting", "error",
      "expired"}] (see below). With ["signin": true] (the window was sent
      back to [#desktop=KEY&signin=1] by the site's sign-in proxy: the
      sign-in expired), a connection under {!Ui.Remote} goes back to the
      sign-in choice first;
    - [/desktop/connect] with ["address"], ["user"] and ["token"]: checks
      them ({!Web_connect.Check}), and if the backend accepts them saves
      them and answers [{"ok": true, "code"}] ([{"ok": true, "remote":
      url}] under {!Ui.Remote}), else
      [{"ok": false, "error"}] (what is wrong and what to do). An address
      without TLS to another machine is refused with
      [{"ok": false, "error", "plaintext": true}] (the risk, and the
      alternatives) unless the request has ["plaintext": true];
    - [/desktop/quit]: answers [{"ok": true}] and ends {!run};
    - [/desktop/signin] with ["how"], while signing in (below):
      ["browser"] signs in through the default browser
      ({!Browser_signin}, [program=desktop]): the shell is told to open its
      URL there ([{"url", "external": true}] on stdout; a request while
      one is under way joins it without opening it again), and the answer
      comes once it ends: [{"ok": true, "remote": url}] (signed in: the
      cookie is remembered with [remember_signin] and the window goes to
      the site with it), [{"ok": false, "error"}] or [{"ok": false,
      "cancelled": true}]; ["again"] opens the one under way again;
      ["cancel"] cancels it; ["here"] goes to the site without a cookie
      ([{"ok": true, "remote": url}], the page line says ["here": true]):
      the window signs in to the proxy itself.

    Under {!Ui.Remote}, once the backend's [/ws] accepted the connection
    ({!Web_connect.Check}), the site's page is probed ({!Site_probe}):
    when a sign-in proxy guards it (or [/ws] itself: the check's
    [Signin_required]), the page here shows the sign-in choice ([{"state":
    "signin"}]: ["site"] its host, ["login"] the login site's if known,
    ["waiting"] the URL of the sign-in through the browser under way,
    ["error"] what went wrong last) instead of the window going to the
    site. When [/ws] needed no cookie the connection is saved and the tool
    host started before the sign-in (it does not need it); when it did,
    after the sign-in through the browser, with the cookie (signing in
    "here" then runs no tool host: it could not get past the proxy). *)

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

module Ui : sig
  type t =
    | Remote (** the backend's own prigh-web *)
    | Local (** prigh-web served here, relayed and signed in *)
  [@@deriving sexp_of]
end

(** Where the window should go: a line on stdout. *)
module Page : sig
  type t =
    { url : string
    ; remote : bool
      (** the backend's site, which the window is to trust as it trusts
          the page here *)
    ; cookie : string option
      (** on the backend's site: the [Cookie] its sign-in proxy wants (from
          a sign-in through the browser), for the window to send there;
          not printed by [sexp_of_t] *)
    ; signin : string option
      (** on the backend's site: the page here, at the sign-in choice
          ([#desktop=KEY&signin=1]), for when the site's sign-in proxy
          sends the window to its login again *)
    ; here : bool
      (** the user chose to sign in to the site's proxy in the window *)
    ; external_ : bool
      (** not for the window: [url] (a sign-in) is to open in the default
          browser *)
    }
  [@@deriving sexp_of]

  (** [{"url": ...}], with ["remote": true], ["cookie": ...], ["signin":
      ...], ["here": true] and ["external": true] when they are (a contract
      with the shell: [desktop/core/src/ready.rs]). *)
  val to_json_line : t -> string
end

module Config : sig
  type t =
    { initial : Initial.t
    ; ui : Ui.t
    ; tools : Web_connect.Tools.t
    ; name : string (** the tool host's *)
    ; host_id : string
    ; cwd : string (** the tool host's *)
    ; listen : Web_connect.Listen.t
    ; root : string (** the built site *)
    ; save : Saved_connection.t -> unit Or_error.t
    ; remember_signin :
        Backend_address.t -> Browser_signin.Grant.t -> unit Or_error.t
      (** after a sign-in through the browser, with the address with its
          cookie ([Backend_choice.remember_signin]); after [save] for a new
          connection *)
    }
end

module Ended : sig
  type t =
    | Quit (** the page's Quit *)
    | Stdin_closed (** the window that ran us is gone *)
    | Stopped (** [stopped] returned: a signal *)
  [@@deriving sexp_of]
end

(** [/desktop/] *)
val path_prefix : string

(** Serves the site, calls [on_page] with where the window should go once
    listening (and again each time that changes), and runs until the page quits, [stdin] reaches its end (or fails) or
    [stopped] returns; then stops the site and the tool host (killing the
    tools' processes) and returns. The URL logged leaves the key out. Raises
    what listening raises. *)
val run
  :  env:Env.t
  -> ?tool_host_wait:Time_ns.Span.t (** default 10s *)
  -> ?check_timeout:Time_ns.Span.t (** default 15s *)
  -> ?stopped:(unit -> unit) (** default: never *)
  -> log:(string -> unit)
  -> on_page:(Page.t -> unit)
  -> stdin:_ Eio.Flow.source
  -> Config.t
  -> Ended.t
