open! Core
open! Import

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

let credentials_missing message =
  String.Caseless.is_substring message ~substring:"not logged in to"
;;

let number json =
  match json with
  | `Number n -> Float.of_string_opt n
  | `String s -> Float.of_string_opt (String.strip s)
  | _ -> None
;;

(* Epoch seconds, or an ISO 8601 time. *)
let time_of_string s =
  let s = String.strip s in
  match Float.of_string_opt s with
  | Some f -> Some f
  | None ->
    (match Time_float.of_string_with_utc_offset s with
     | t -> Some (Time_float.to_span_since_epoch t |> Time_float.Span.to_sec)
     | exception _ -> None)
;;

let of_body ~now body =
  match Json.parse body with
  | Error _ -> None
  | Ok json ->
    let error = Option.value (Json.member "error" json) ~default:json in
    (match
       ( Option.bind (Json.member "resets_at" error) ~f:number
       , Option.bind (Json.member "resets_in_seconds" error) ~f:number )
     with
     | Some at, _ -> Some at
     | None, Some seconds -> Some (now +. seconds)
     | None, None -> None)
;;

let of_headers ~now headers =
  let header name =
    List.find_map headers ~f:(fun (k, v) ->
      Option.some_if (String.Caseless.equal k name) v)
  in
  let time name = Option.bind (header name) ~f:time_of_string in
  let latest = List.max_elt ~compare:Float.compare in
  let codex =
    let windows =
      List.filter_map [ "primary"; "secondary" ] ~f:(fun window ->
        Option.map
          (time (sprintf "x-codex-%s-reset-at" window))
          ~f:(fun at ->
            let used =
              Option.bind
                (header (sprintf "x-codex-%s-used-percent" window))
                ~f:Float.of_string_opt
            in
            ( at
            , Option.value_map used ~default:false ~f:(fun u ->
                Float.( >= ) u 100.) )))
    in
    match List.filter windows ~f:snd with
    | [] -> latest (List.map windows ~f:fst)
    | exhausted -> latest (List.map exhausted ~f:fst)
  in
  let anthropic =
    match time "anthropic-ratelimit-unified-reset" with
    | Some at -> Some at
    | None ->
      let windows =
        List.filter_map [ "5h"; "7d"; "7d_opus"; "7d_sonnet" ] ~f:(fun window ->
          Option.map
            (time (sprintf "anthropic-ratelimit-unified-%s-reset" window))
            ~f:(fun at ->
              ( at
              , Option.equal
                  String.equal
                  (header
                     (sprintf "anthropic-ratelimit-unified-%s-status" window))
                  (Some "rejected") )))
      in
      (match List.filter windows ~f:snd with
       | [] -> latest (List.map windows ~f:fst)
       | rejected -> latest (List.map rejected ~f:fst))
  in
  let retry_after =
    Option.bind (header "retry-after") ~f:(fun v ->
      Option.map (Float.of_string_opt (String.strip v)) ~f:(fun s -> now +. s))
  in
  List.find_map [ codex; anthropic; retry_after ] ~f:Fn.id
;;

let reset_time ~now ~headers ~body =
  Option.first_some (of_body ~now body) (of_headers ~now headers)
;;

let note_prefix = " (resets at "

let utc_string at =
  let time =
    Time_float.of_span_since_epoch
      (Time_float.Span.of_sec (Float.round_down at))
  in
  let date, ofday = Time_float.to_date_ofday time ~zone:Time_float.Zone.utc in
  let parts = Time_float.Ofday.to_parts ofday in
  sprintf
    "%sT%02d:%02d:%02dZ"
    (Date.to_string date)
    parts.hr
    parts.min
    parts.sec
;;

let reset_note at = note_prefix ^ utc_string at ^ ")"

let resets_at message =
  match String.substr_index message ~pattern:note_prefix with
  | None -> None
  | Some i ->
    let rest = String.drop_prefix message (i + String.length note_prefix) in
    Option.bind (String.lsplit2 rest ~on:')') ~f:(fun (time, _) ->
      time_of_string time)
;;
