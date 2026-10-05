open! Core

let parse s = Option.try_with (fun () -> Time_ns.of_string_with_utc_offset s)

let ago ~now time =
  let seconds = Time_ns.Span.to_sec (Time_ns.diff now time) in
  let minutes = Float.iround_down_exn (seconds /. 60.) in
  let hours = minutes / 60 in
  let days = hours / 24 in
  if Float.(seconds < 45.)
  then "just now"
  else if minutes < 60
  then sprintf "%dm ago" (Int.max 1 minutes)
  else if hours < 24
  then sprintf "%dh ago" hours
  else if days < 30
  then sprintf "%dd ago" days
  else if days < 365
  then sprintf "%dmo ago" (days / 30)
  else sprintf "%dy ago" (days / 365)
;;
