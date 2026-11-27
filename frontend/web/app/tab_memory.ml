open! Core
open Js_of_ocaml

let scroll_key = "prigh-web.scroll"
let draft_key = "prigh-web.draft"

let storage () =
  Option.try_with (fun () ->
    Js.Optdef.to_option Dom_html.window##.sessionStorage)
  |> Option.join
;;

let session () = Browser.query_param "session"

let save_for ~session key fields =
  Option.iter (storage ()) ~f:(fun storage ->
    try
      storage##setItem
        (Js.string key)
        (Js.string
           (Jsonaf.to_string (`Object (("session", `String session) :: fields))))
    with
    | _ -> ())
;;

let save key fields =
  Option.iter (session ()) ~f:(fun session -> save_for ~session key fields)
;;

(* Read as the page starts: the chat's scroll events while it loads
   overwrite it. *)
let load key ~f =
  let open Option.Let_syntax in
  let%bind storage = storage () in
  let%bind text =
    Option.try_with (fun () ->
      Js.Opt.to_option (storage##getItem (Js.string key)))
    |> Option.join
  in
  let%bind json = Jsonaf.parse (Js.to_string text) |> Result.ok in
  match Jsonaf.member "session" json with
  | Some (`String session) ->
    Option.map (f json) ~f:(fun value -> session, value)
  | _ -> None
;;

let for_this_session = function
  | Some (saved, value) when Option.equal String.equal (Some saved) (session ())
    -> Some value
  | _ -> None
;;

(* From the end rather than the top: only the newest page loads at first,
   and the older ones loaded to get back there keep this in place. *)
let save_scroll (el : Dom_html.element Js.t) =
  let from_end = Float.of_int el##.scrollHeight -. Js.to_float el##.scrollTop in
  let at_end = Float.(from_end -. of_int el##.clientHeight < 40.) in
  save
    scroll_key
    [ "from_end", `Number (sprintf "%.0f" from_end)
    ; ("at_end", if at_end then `True else `False)
    ]
;;

let scroll =
  load scroll_key ~f:(fun json ->
    match Jsonaf.member "from_end" json, Jsonaf.member "at_end" json with
    | Some (`Number from_end), Some `False -> Some (Float.of_string from_end)
    | _ -> None)
;;

let taken = ref false

let take_scroll () =
  if !taken
  then None
  else (
    taken := true;
    for_this_session scroll)
;;

(* The session is the one the draft was typed in: the page may be in another
   by the time it is written. *)
let pending_draft = ref None

let flush_draft () =
  Option.iter !pending_draft ~f:(fun (session, draft) ->
    pending_draft := None;
    save_for ~session draft_key [ "draft", `String draft ])
;;

let save_draft_soon draft =
  Option.iter (session ()) ~f:(fun session ->
    let scheduled = Option.is_some !pending_draft in
    pending_draft := Some (session, draft);
    if String.is_empty draft
    then flush_draft ()
    else if not scheduled
    then
      ignore (Dom_html.setTimeout flush_draft 300. : Dom_html.timeout_id_safe))
;;

let draft =
  load draft_key ~f:(fun json ->
    match Jsonaf.member "draft" json with
    | Some (`String draft) when not (String.is_empty draft) -> Some draft
    | _ -> None)
;;

let saved_draft () = for_this_session draft
let temporary_key = "prigh-web.temporary"

let remember_temporary = function
  | Some (session, key) ->
    Option.iter (storage ()) ~f:(fun storage ->
      try
        storage##setItem
          (Js.string temporary_key)
          (Js.string
             (Jsonaf.to_string
                (`Object [ "session", `String session; "key", `String key ])))
      with
      | _ -> ())
  | None ->
    Option.iter (storage ()) ~f:(fun storage ->
      try storage##removeItem (Js.string temporary_key) with
      | _ -> ())
;;

let temporary_key_of session =
  let open Option.Let_syntax in
  let%bind storage = storage () in
  let%bind text =
    Option.try_with (fun () ->
      Js.Opt.to_option (storage##getItem (Js.string temporary_key)))
    |> Option.join
  in
  let%bind json = Jsonaf.parse (Js.to_string text) |> Result.ok in
  match Jsonaf.member "session" json, Jsonaf.member "key" json with
  | Some (`String saved), Some (`String key) when String.equal saved session ->
    Some key
  | _ -> None
;;
