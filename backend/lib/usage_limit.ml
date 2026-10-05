open! Core

let marker = "(usage limit reached)"

let patterns =
  [ marker
  ; "usage_limit_reached"
  ; "usage limit"
  ; "insufficient_quota"
  ; "exceeded your current quota"
  ; "insufficient balance"
  ; "credit balance is too low"
  ; "quota exceeded"
  ; "not logged in to"
  ]
;;

let unavailable message =
  String.is_prefix message ~prefix:"HTTP 402"
  || List.exists patterns ~f:(fun substring ->
    String.Caseless.is_substring message ~substring)
;;
