open! Core
open! Import
open Html
open Settings_row
module Action = App.Action
module U = Subscription_usage
module Logins = Prigh_ui.Provider_logins
module Text = Prigh_ui.Login_usage_text

let window m ~now (w : U.Window.t) =
  row
    ~cls:"usage-window gap-2"
    ~wide:true
    ~name:w.label
    ~about:(Usage_meter.resets m ~now w)
    [ Usage_meter.meter w ]
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
      let now = Usage_meter.now m u in
      sprintf
        "Read %s (%s ago). Updated when you open this page or press Refresh."
        (Message_time.short (App.Model.message_time m) u.fetched_at)
        (Text.until (Time_ns.diff now u.fetched_at))
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

(* The provider's logins with usage to show, in a card under its name; a
   provider with none says why (not signed in, an API key, an error). *)
let provider_cards (m : App.Model.t) ~inject ~limits (id : string) =
  let logins =
    List.filter
      (Logins.all
         (List.filter m.auth ~f:(fun (p : Auth_status.t) ->
            String.equal p.provider id && Option.is_none p.custom)))
      ~f:(Login_usage_view.shown m)
  in
  match logins, limits with
  | [], Some (u, p) -> [ provider m ~inject ~now:(Usage_meter.now m u) p ]
  | [], None -> []
  | (l :: _ as logins), _ ->
    [ group
        ~title:l.provider.name
        (List.map logins ~f:(Login_usage_view.row m ~inject))
    ]
;;

let groups (m : App.Model.t) ~inject =
  let usage =
    match m.usage.usage with
    | Some (Ok u) -> Some u
    | Some (Error _) | None -> None
  in
  let limits id =
    Option.bind usage ~f:(fun u ->
      List.find_map u.providers ~f:(fun (p : U.Provider_usage.t) ->
        Option.some_if (String.equal p.provider id) (u, p)))
  in
  (* The providers with limits first, as the backend lists them. *)
  let ids =
    Option.value_map usage ~default:[] ~f:(fun u ->
      List.map u.providers ~f:(fun p -> p.provider))
    @ List.map m.auth ~f:(fun p -> p.provider)
    |> List.fold ~init:[] ~f:(fun seen id ->
      if List.mem seen id ~equal:String.equal then seen else id :: seen)
    |> List.rev
  in
  group [ refresh_row m ~inject ]
  :: List.concat_map ids ~f:(fun id ->
    provider_cards m ~inject ~limits:(limits id) id)
;;
