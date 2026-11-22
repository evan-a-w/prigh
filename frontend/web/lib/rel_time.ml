open! Core

let parse s = Option.try_with (fun () -> Time_ns.of_string_with_utc_offset s)

let span ~now time ~just_now ~suffix =
  let seconds = Time_ns.Span.to_sec (Time_ns.diff now time) in
  let minutes = Float.iround_down_exn (seconds /. 60.) in
  let hours = minutes / 60 in
  let days = hours / 24 in
  if Float.(seconds < 45.)
  then just_now
  else if minutes < 60
  then sprintf "%dm%s" (Int.max 1 minutes) suffix
  else if hours < 24
  then sprintf "%dh%s" hours suffix
  else if days < 30
  then sprintf "%dd%s" days suffix
  else if days < 365
  then sprintf "%dmo%s" (days / 30) suffix
  else sprintf "%dy%s" (days / 365) suffix
;;

let ago ~now time = span ~now time ~just_now:"just now" ~suffix:" ago"
let short ~now time = span ~now time ~just_now:"now" ~suffix:""
