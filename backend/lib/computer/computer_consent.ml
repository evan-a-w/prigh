open! Core
open! Import

type t = { mutable host : string option }

let create () = { host = None }
let granted t = t.host
let is_granted t ~host = Option.equal String.equal t.host (Some host)
let grant t ~host = t.host <- Some host

let revoke t =
  let was = Option.is_some t.host in
  t.host <- None;
  was
;;

let do_not_retry =
  "Do not use the computer tool again unless the user asks you to."
;;

let declined ~host ~reason =
  sprintf
    "[computer use declined] The user did not allow prigh to control %s's \
     screen. %s%s"
    host
    do_not_retry
    (match reason with
     | None -> ""
     | Some reason -> " Their reason: " ^ reason)
;;

let timed_out ~host ~after =
  sprintf
    "[computer use not allowed] Nobody allowed prigh to control %s's screen \
     within %s (the user may be away). %s"
    host
    (User_question.duration_hum after)
    do_not_retry
;;

let unavailable ~host why =
  sprintf
    "[computer use not allowed] %s, so nobody can allow prigh to control %s's \
     screen. %s"
    why
    host
    do_not_retry
;;

let timed_out_notice ~host ~after =
  sprintf
    "computer use on %s was not allowed within %s: the agent was told not to \
     use it again unless you ask"
    host
    (User_question.duration_hum after)
;;
