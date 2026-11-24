open! Core
open! Import
open Html
module Logins = Prigh_ui.Provider_logins
module Text = Prigh_ui.Login_usage_text

let limits (m : App.Model.t) (l : Logins.Login.t) =
  match m.usage.usage with
  | Some (Ok u) ->
    Option.map (Text.limits_of (Some u) ~ref_:l.account.ref_) ~f:(fun s ->
      s, Usage_view.now m u)
  | Some (Error _) | None -> None
;;

let window m ~now (w : Subscription_usage.Window.t) =
  div
    ~cls:"login-usage-window flex flex-col gap-1"
    [ div
        ~cls:
          "flex flex-wrap items-baseline justify-between gap-x-3 text-xs \
           text-muted-foreground"
        [ span ~cls:"font-medium text-foreground" w.label
        ; span ~cls:"login-usage-resets" (Usage_view.resets m ~now w)
        ]
    ; Usage_view.meter w
    ]
;;

let periods (l : Logins.Login.t) =
  match l.account.usage with
  | Some u when u.last_30_days.requests > 0 ->
    let cost = Text.billed l.account in
    let cell name (t : Login_usage.Totals.t) =
      div
        ~cls:"login-usage-period flex min-w-0 flex-col"
        [ span ~cls:"text-[11px] uppercase tracking-wide" name
        ; span ~cls:"tabular-nums text-foreground" (Text.compact ~cost t)
        ]
    in
    Some
      (div
         ~cls:
           "login-usage-periods grid grid-cols-3 gap-x-3 text-xs \
            text-muted-foreground narrow:grid-cols-1 narrow:gap-y-1"
         [ cell "Today" u.today
         ; cell "7 days" u.last_7_days
         ; cell "30 days" u.last_30_days
         ])
  | _ -> None
;;

let users (m : App.Model.t) (l : Logins.Login.t) =
  match Map.find m.login_users l.account.ref_ with
  | None | Some [] -> None
  | Some users ->
    let cost = Text.billed l.account in
    Option.some
    @@ Node.ul
         ~attrs:
           [ Attr.class_
               "login-usage-users flex flex-col gap-0.5 text-xs \
                text-muted-foreground"
           ; Attr.create "aria-label" "Usage by user"
           ]
         (List.map users ~f:(fun (u : Login_usage.User.t) ->
            Node.li
              ~attrs:[ Attr.class_ "login-usage-user flex flex-wrap gap-x-2" ]
              [ span
                  ~cls:"min-w-[6ch] font-medium text-foreground"
                  (if u.you then "You" else u.user)
              ; span
                  ~cls:"tabular-nums"
                  (if u.usage.last_30_days.requests = 0
                   then "nothing in 30 days"
                   else
                     sprintf
                       "today %s · 30 days %s"
                       (Text.compact ~cost u.usage.today)
                       (Text.compact ~cost u.usage.last_30_days))
              ]))
;;

let details m (l : Logins.Login.t) =
  let windows =
    match limits m l with
    | Some (Limits { windows; _ }, now) -> List.map windows ~f:(window m ~now)
    | Some (Failed e, _) ->
      [ span
          ~cls:"login-usage-failed text-xs text-primary"
          ("Limits unknown: " ^ e)
      ]
    | Some ((Not_signed_in | Api_key), _) | None -> []
  in
  match windows, periods l, users m l with
  | [], None, None -> Node.none
  | windows, periods, users ->
    div
      ~cls:"login-usage col-span-full flex min-w-0 flex-col gap-2"
      (windows @ List.filter_opt [ periods; users ])
;;

let session_login (m : App.Model.t) (state : State.t) =
  match Logins.of_session m.auth state with
  | Some l -> Some l
  | None ->
    List.find (Logins.all m.auth) ~f:(fun l ->
      String.equal l.provider.provider state.model.provider)
;;

let card_lines m state ~(line : ?cls:string -> string -> string -> Node.t) =
  match session_login m state with
  | None -> []
  | Some l ->
    let windows =
      match limits m l with
      | Some (Limits { windows; _ }, now) ->
        List.map windows ~f:(fun (w : Subscription_usage.Window.t) ->
          let resets =
            match w.resets_at with
            | Some at when Time_ns.( > ) at now ->
              " · " ^ Text.until (Time_ns.diff at now)
            | _ -> ""
          in
          line ~cls:"login-limit" w.label (Text.percent w.used_percent ^ resets))
      | _ -> []
    in
    let today =
      match l.account.usage with
      | Some u when u.today.requests > 0 ->
        [ line
            ~cls:"login-today"
            "Login today"
            (Text.compact ~cost:(Text.billed l.account) u.today)
        ]
      | _ -> []
    in
    line ~cls:"login-line" "Login" (Logins.label l.account) :: (windows @ today)
;;
