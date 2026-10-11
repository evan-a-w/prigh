open! Core
open Js_of_ocaml

let key = "prigh-web.scroll"

let storage () =
  Option.try_with (fun () -> Js.Optdef.to_option Dom_html.window##.sessionStorage)
  |> Option.join
;;

let session () = Prigh_ui_web_app.Browser.query_param "session"

let save (el : Dom_html.element Js.t) =
  Option.iter (Option.both (storage ()) (session ())) ~f:(fun (storage, session) ->
    let top = Js.to_float el##.scrollTop in
    let at_end =
      Float.(of_int el##.scrollHeight -. top -. of_int el##.clientHeight < 40.)
    in
    storage##setItem
      (Js.string key)
      (Js.string
         (Jsonaf.to_string
            (`Object
                [ "session", `String session
                ; "top", `Number (sprintf "%.0f" top)
                ; "at_end", if at_end then `True else `False
                ]))))
;;

let saved =
  lazy
    (let open Option.Let_syntax in
     let%bind storage = storage () in
     let%bind text = Js.Opt.to_option (storage##getItem (Js.string key)) in
     let%bind json = Jsonaf.parse (Js.to_string text) |> Result.ok in
     let field name = Jsonaf.member name json in
     match field "session", field "top", field "at_end" with
     | Some (`String session), Some (`Number top), Some `False ->
       Some (session, Float.of_string top)
     | _ -> None)
;;

let taken = ref false

let take () =
  if !taken
  then None
  else (
    taken := true;
    match Lazy.force saved, session () with
    | Some (saved, top), Some session when String.equal saved session -> Some top
    | _ -> None)
;;
