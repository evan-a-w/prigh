open! Core

(** Links to other sites in the desktop app's browser window (a
    Chromium-family [--app] window, see {!Desktop_link.browser_shell_of_hash}):
    the app gives each of prigh's windows a function, {!binding}, which opens
    a link in the system's default browser at once. A click on such a link
    is handed to it instead of the browser, which would open a window of its
    own for the app to close again. Where the function is missing (a browser
    tab, the app's own window, an older app) links are left to the browser. *)

(** [window.prighOpenExternal]. *)
val binding : string

(** The click on a link. *)
module Click : sig
  type t =
    { button : int
      (** 0: the main button ([click]); 1: the middle ([auxclick]) *)
    ; ctrl : bool
    ; meta : bool
    ; shift : bool
    }
  [@@deriving sexp_of]
end

(** The link it opens, and whether the click would open a new window (its
    [target] names one, Ctrl/⌘/Shift+click, a middle click). *)
type t =
  { url : string
  ; new_window : bool
  }
[@@deriving sexp_of]

(** Whether a click on a link to [href] (the absolute address the link's
    [href] property gives) is handed over: one to another site than
    [own_origin] (the page's [location.origin]) over http or https, or a
    [mailto:]; not a link to prigh's own site, a download, another scheme,
    or a click with another button. [target]: the link's [target]
    attribute. *)
val decide
  :  own_origin:string
  -> href:string
  -> target:string
  -> download:bool
  -> Click.t
  -> t option

(** What {!binding} is called with: [{"url": URL, "newWindow": BOOL}]. *)
val payload : t -> string
