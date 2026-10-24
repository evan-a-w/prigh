open! Core
open! Import

module Permission = struct
  type t =
    | Default
    | Granted
    | Denied
    | Unsupported
  [@@deriving sexp_of, equal]

  let of_string = function
    | "default" -> Default
    | "granted" -> Granted
    | "denied" -> Denied
    | _ -> Unsupported
  ;;
end

type t =
  { enabled : bool
  ; permission : Permission.t
  ; hidden : bool
  ; unseen : int
  }
[@@deriving sexp_of, equal]

let init = { enabled = true; permission = Default; hidden = false; unseen = 0 }

let set_hidden t hidden =
  { t with hidden; unseen = (if hidden then t.unseen else 0) }
;;

let on_event t event ~where =
  if not t.hidden
  then t, None
  else (
    match Prigh_ui.Notify.of_event event ~where with
    | None -> t, None
    | Some n ->
      ( { t with unseen = t.unseen + 1 }
      , Option.some_if (t.enabled && Permission.equal t.permission Granted) n ))
;;

let what =
  "when a turn ends, a tool waits for you or background work finishes while \
   this tab is hidden"
;;

let describe t =
  match t.enabled, t.permission with
  | false, _ -> "Notifications are off: /notify on turns them on"
  | true, Granted -> "Notifications are on: the browser tells you " ^ what
  | true, Default ->
    "Notifications are on, but the browser has not been asked yet: /notify on \
     asks it"
  | true, Denied ->
    "The browser blocks notifications for this page: allow them in its site \
     settings (the icon left of the address), then /notify on"
  | true, Unsupported ->
    "This browser cannot show notifications here (they need https or \
     localhost); the tab's title still counts what happened"
;;

let title t = if t.unseen > 0 then sprintf "(%d) prigh" t.unseen else "prigh"
