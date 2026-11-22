open! Core
open! Import
open Html
open Settings_row
module Action = App.Action
module U = Subscription_usage

let until span =
  let minutes = Float.iround_up_exn (Time_ns.Span.to_min span) in
  let days = minutes / (24 * 60)
  and hours = minutes / 60 % 24
  and mins = minutes % 60 in
  if minutes <= 0
  then "under a minute"
  else if days > 0
  then if hours = 0 then sprintf "%dd" days else sprintf "%dd %dh" days hours
  else if hours > 0
  then if mins = 0 then sprintf "%dh" hours else sprintf "%dh %dm" hours mins
  else sprintf "%dm" mins
;;

(* The clock moves on while the page is open; never before the answer. *)
let now (m : App.Model.t) (usage : U.t) =
  match m.now with
  | Some now when Time_ns.( > ) now usage.fetched_at -> now
  | _ -> usage.fetched_at
;;

let resets (m : App.Model.t) ~now (w : U.Window.t) =
  match w.resets_at with
  | None -> "Reset time not reported"
  | Some at when Time_ns.( <= ) at now -> "Has reset: Refresh to see it"
  | Some at ->
    sprintf
      "Resets in %s · %s"
      (until (Time_ns.diff at now))
      (Message_time.short (App.Model.message_time m) at)
;;

let percent p =
  if Float.( < ) p 10. && not (Float.is_integer p)
  then sprintf "%.1f%%" p
  else sprintf "%.0f%%" p
;;

let meter (w : U.Window.t) =
  let used = Float.clamp_exn w.used_percent ~min:0. ~max:100. in
  let level =
    if Float.( >= ) used 90.
    then "full"
    else if Float.( >= ) used 70.
    then "high"
    else "ok"
  in
  div
    ~cls:("usage-meter " ^ level)
    [ Node.div
        ~attrs:
          [ Attr.class_ "usage-track"
          ; Attr.create "role" "progressbar"
          ; Attr.create "aria-label" (w.label ^ " used")
          ; Attr.create "aria-valuemin" "0"
          ; Attr.create "aria-valuemax" "100"
          ; Attr.create "aria-valuenow" (sprintf "%.0f" used)
          ]
        [ Node.div
            ~attrs:
              [ Attr.class_ "usage-fill"
              ; Attr.style
                  (Css_gen.width (`Percent (Percent.of_percentage used)))
              ]
            []
        ]
    ; span ~cls:"usage-percent" (percent w.used_percent ^ " used")
    ]
;;

let window m ~now (w : U.Window.t) =
  row
    ~cls:"usage-window"
    ~wide:true
    ~name:w.label
    ~about:(resets m ~now w)
    [ meter w ]
;;

let sign_in ~inject (p : U.Provider_usage.t) ~about =
  let command = "/login " ^ p.provider in
  row
    ~cls:"usage-signin"
    ~name:"Not signed in"
    ~about
    [ button
        ~cls:"small"
        ~title:command
        ~on_click:(inject (Action.Run command))
        [ Node.text "Sign in" ]
    ]
;;

let provider (m : App.Model.t) ~inject ~now (p : U.Provider_usage.t) =
  match p.status with
  | Not_signed_in ->
    group
      ~title:p.name
      [ sign_in
          ~inject
          p
          ~about:
            (sprintf
               "Sign in with your %s subscription to see its limits: /login %s."
               p.name
               p.provider)
      ]
  | Api_key ->
    group
      ~title:p.name
      [ sign_in
          ~inject
          p
          ~about:
            (sprintf
               "Signed in with an API key, billed per token: no subscription \
                limits. To see them, sign in with the subscription: /login %s."
               p.provider)
      ]
  | Failed e ->
    group
      ~title:p.name
      [ row
          ~cls:"usage-failed"
          ~note:("warm", e)
          ~name:"Could not read the limits"
          ~about:""
          []
      ]
  | Limits { plan; windows = [] } ->
    group
      ~title:p.name
      ?note:(Option.map plan ~f:String.capitalize)
      [ row
          ~name:"No limits reported"
          ~about:"The provider reported no usage windows for this plan."
          []
      ]
  | Limits { plan; windows } ->
    group
      ~title:p.name
      ?note:(Option.map plan ~f:String.capitalize)
      (List.map windows ~f:(window m ~now))
;;

let refresh_row (m : App.Model.t) ~inject =
  let page = m.usage in
  let about =
    match page.usage, page.loading with
    | None, _ -> "Reading your usage from the providers…"
    | Some (Ok u), _ ->
      let now = now m u in
      sprintf
        "Read %s (%s ago). Updated when you open this page or press Refresh."
        (Message_time.short (App.Model.message_time m) u.fetched_at)
        (until (Time_ns.diff now u.fetched_at))
      |> String.substr_replace_all
           ~pattern:"(under a minute ago)"
           ~with_:"(just now)"
    | Some (Error _), _ -> "Could not read your usage."
  in
  let note =
    match page.usage with
    | Some (Error e) -> Some ("warm", String.capitalize e ^ ": try Refresh.")
    | Some (Ok _) | None -> None
  in
  row
    ~cls:"usage-refresh"
    ?note
    ~name:"Subscription limits"
    ~about
    [ (if page.loading
       then
         button
           ~cls:"small"
           ~disabled:true
           ~attrs:[ Attr.create "aria-busy" "true" ]
           ~on_click:Effect.Ignore
           [ Html.lamp (); Node.text "Reading…" ]
       else
         button
           ~cls:"small"
           ~title:"Ask the providers again (/usage)"
           ~on_click:(inject Action.Usage_refresh)
           [ Node.text "Refresh" ])
    ]
;;

let groups (m : App.Model.t) ~inject =
  group [ refresh_row m ~inject ]
  ::
  (match m.usage.usage with
   | Some (Ok u) ->
     let now = now m u in
     List.map u.providers ~f:(provider m ~inject ~now)
   | Some (Error _) | None -> [])
;;
