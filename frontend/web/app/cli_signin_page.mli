open! Core

(** The page's side of {!Prigh_web.Cli_signin}: a confirmation for a program
    signing in through the browser. *)

(** The request in the URL's fragment (which it removes), or the one waiting
    in this tab while the user signed in to prigh. *)
val take : unit -> Prigh_web.Cli_signin.Request.t option

(** Shows the confirmation. [login]: the page's prigh credentials. When the
    server does not accept them, the request waits in this tab and
    [sign_in ~error] shows the sign-in form; after signing in the page loads
    again and asks again. *)
val show
  :  Prigh_web.Cli_signin.Request.t
  -> login:Login.t
  -> sign_in:(error:string -> unit)
  -> unit
