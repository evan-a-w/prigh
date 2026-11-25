open! Core
open! Import
open Html
module U = Subscription_usage
module Text = Prigh_ui.Login_usage_text

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
      (Text.until (Time_ns.diff at now))
      (Message_time.short (App.Model.message_time m) at)
;;

(* loon's Progress: the bar is the muted text's colour until 70% used,
   then the accent, then from 90% the destructive colour. *)
let meter (w : U.Window.t) =
  let used = Float.clamp_exn w.used_percent ~min:0. ~max:100. in
  let level, bar =
    if Float.( >= ) used 90.
    then "full", "[&>[data-slot=progress-indicator]]:bg-destructive"
    else if Float.( >= ) used 70.
    then "high", "[&>[data-slot=progress-indicator]]:bg-primary"
    else "ok", "[&>[data-slot=progress-indicator]]:bg-muted-foreground"
  in
  div
    ~cls:("usage-meter flex w-full items-center gap-3 " ^ level)
    [ Loon.Progress.view
        ~class_:
          ("usage-track h-1.5 flex-1 \
            [&>[data-slot=progress-indicator]]:motion-reduce:transition-none "
           ^ bar)
        ~attrs:[ Attr.create "aria-label" (w.label ^ " used") ]
        ~value:used
        ()
    ; span
        ~cls:
          (if String.equal level "full"
           then
             "usage-percent min-w-[8ch] text-right text-[13px] tabular-nums \
              text-destructive"
           else
             "usage-percent min-w-[8ch] text-right text-[13px] tabular-nums \
              text-muted-foreground")
        (Text.percent w.used_percent ^ " used")
    ]
;;
