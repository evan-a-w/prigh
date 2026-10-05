open! Core

type t =
  { today : Date.t
  ; utc_offset : Time_ns.Span.t
  }
[@@deriving sexp_of, equal]

let local t time =
  Time_ns.to_date_ofday (Time_ns.add time t.utc_offset) ~zone:Time_ns.Zone.utc
;;

let date t time = fst (local t time)

let create ~now ~utc_offset =
  { today = date { today = Date.unix_epoch; utc_offset } now; utc_offset }
;;

let months =
  [| "January"
   ; "February"
   ; "March"
   ; "April"
   ; "May"
   ; "June"
   ; "July"
   ; "August"
   ; "September"
   ; "October"
   ; "November"
   ; "December"
  |]
;;

let month_name date = months.(Month.to_int (Date.month date) - 1)
let short_month date = String.prefix (month_name date) 3
let day_month date = sprintf "%d %s" (Date.day date) (short_month date)

let with_year t date s =
  if Date.year date = Date.year t.today
  then s
  else sprintf "%s %d" s (Date.year date)
;;

let hh_mm ofday =
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf "%02d:%02d" parts.hr parts.min
;;

let is_yesterday t date = Date.equal date (Date.add_days t.today (-1))

let short t time =
  let date, ofday = local t time in
  if Date.equal date t.today
  then hh_mm ofday
  else if is_yesterday t date
  then "Yesterday " ^ hh_mm ofday
  else sprintf "%s %s" (with_year t date (day_month date)) (hh_mm ofday)
;;

let full t time =
  let date, ofday = local t time in
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf
    "%s %d %s %d, %02d:%02d:%02d"
    (String.capitalize
       (String.lowercase (Day_of_week.to_string_long (Date.day_of_week date))))
    (Date.day date)
    (month_name date)
    (Date.year date)
    parts.hr
    parts.min
    parts.sec
;;

let day_label t date =
  if Date.equal date t.today
  then "Today"
  else if is_yesterday t date
  then "Yesterday"
  else
    with_year
      t
      date
      (sprintf
         "%s %s"
         (String.capitalize
            (String.lowercase (Day_of_week.to_string (Date.day_of_week date))))
         (day_month date))
;;
