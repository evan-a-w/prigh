open! Core
open! Import

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

let percent p =
  if Float.( < ) p 10. && not (Float.is_integer p)
  then sprintf "%.1f%%" p
  else sprintf "%.0f%%" p
;;

let tokens n =
  if n < 1000
  then Int.to_string n
  else if n < 10000
  then sprintf "%.1fk" (Float.of_int n /. 1e3)
  else if n < 1_000_000
  then sprintf "%dk" (Int.of_float (Float.round (Float.of_int n /. 1e3)))
  else if n < 10_000_000
  then sprintf "%.1fM" (Float.of_int n /. 1e6)
  else sprintf "%dM" (Int.of_float (Float.round (Float.of_int n /. 1e6)))
;;

let limits_of (usage : P.Subscription_usage.t option) ~ref_ =
  Option.bind usage ~f:(fun usage ->
    List.find_map usage.providers ~f:(fun p ->
      List.find_map p.accounts ~f:(fun a ->
        Option.some_if (String.equal a.ref_ ref_) a.status)))
;;

let window ~now (w : P.Subscription_usage.Window.t) =
  let resets =
    match w.resets_at with
    | None -> ""
    | Some at when Time_ns.( <= ) at now -> ", has reset"
    | Some at -> ", resets in " ^ until (Time_ns.diff at now)
  in
  sprintf "%s %s%s" w.label (percent w.used_percent) resets
;;

let limits ?max ~now (status : P.Subscription_usage.Status.t) =
  match status with
  | Not_signed_in | Api_key -> None
  | Failed e -> Some ("limits unknown: " ^ e)
  | Limits { windows = []; _ } -> Some "no limits reported"
  | Limits { windows; _ } ->
    let windows =
      match max with
      | Some n -> List.take windows n
      | None -> windows
    in
    Some (String.concat ~sep:" · " (List.map windows ~f:(window ~now)))
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")

let totals ~cost (t : P.Login_usage.Totals.t) =
  String.concat
    ~sep:", "
    ([ plural t.requests "request"; tokens (t.input + t.output) ^ " tokens" ]
     @ if cost then [ sprintf "$%.2f" t.cost_usd ] else [])
;;

let periods ?(all = false) ~cost (usage : P.Login_usage.t option) =
  match usage with
  | None -> "nothing in the last 30 days"
  | Some u when u.last_30_days.requests = 0 -> "nothing in the last 30 days"
  | Some u ->
    List.filter_opt
      [ Some ("today " ^ totals ~cost u.today)
      ; Option.some_if all ("7 days " ^ totals ~cost u.last_7_days)
      ; Some ("30 days " ^ totals ~cost u.last_30_days)
      ]
    |> String.concat ~sep:" · "
;;

let period_lines ~cost (usage : P.Login_usage.t option) =
  match usage with
  | Some u when u.last_30_days.requests > 0 ->
    [ "today    " ^ totals ~cost u.today
    ; "7 days   " ^ totals ~cost u.last_7_days
    ; "30 days  " ^ totals ~cost u.last_30_days
    ]
  | _ -> [ "nothing in the last 30 days" ]
;;

let billed (a : P.Auth_status.Account.t) = not (String.equal a.method_ "oauth")

let compact ~cost (t : P.Login_usage.Totals.t) =
  String.concat
    ~sep:" "
    ([ sprintf "%d req" t.requests; tokens (t.input + t.output) ^ " tok" ]
     @ if cost then [ sprintf "$%.2f" t.cost_usd ] else [])
;;

let brief ~now ~limits:status (a : P.Auth_status.Account.t) =
  let status =
    match status with
    | Some (P.Subscription_usage.Status.Failed _) -> None
    | s -> s
  in
  match Option.bind status ~f:(limits ~max:1 ~now), a.usage with
  | Some text, _ -> Some text
  | None, Some u when u.today.requests > 0 ->
    Some ("today " ^ compact ~cost:(billed a) u.today)
  | None, Some u when u.last_30_days.requests > 0 ->
    Some ("30d " ^ compact ~cost:(billed a) u.last_30_days)
  | None, _ -> None
;;

let user ~cost (u : P.Login_usage.User.t) =
  sprintf
    "%s: %s"
    (if u.you then "you" else u.user)
    (periods ~cost (Some u.usage))
;;
