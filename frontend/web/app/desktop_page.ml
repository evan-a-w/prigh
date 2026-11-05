open! Core
open Js_of_ocaml
open Prigh_web

let storage () =
  Option.try_with (fun () ->
    Js.Optdef.to_option Dom_html.window##.sessionStorage)
  |> Option.join
;;

let stored () =
  Option.bind (storage ()) ~f:(fun storage ->
    Js.Opt.to_option (storage##getItem (Js.string Desktop_link.storage_key))
    |> Option.map ~f:Js.to_string)
;;

let key () =
  match Desktop_link.key_of_hash (Browser.hash ()) with
  | Some key ->
    Option.iter (storage ()) ~f:(fun storage ->
      storage##setItem (Js.string Desktop_link.storage_key) (Js.string key));
    (* The sign-in code, if any, is the sidecar's to give: only the key was
       in the fragment. *)
    Browser.clear_hash ();
    Some key
  | None -> stored ()
;;

let active () = Option.is_some (stored ())

let status ~key ~f =
  Browser.post
    ~url:Desktop_link.status_path
    ~body:(Desktop_link.status_body ~key)
    ~f:(fun reply -> f (Option.bind reply ~f:Desktop_link.Status.of_json))
;;

let connect ~key ~address ~user ~token ~f =
  Browser.post
    ~url:Desktop_link.connect_path
    ~body:(Desktop_link.connect_body ~key ~address ~user ~token)
    ~f:(fun reply -> f (Option.bind reply ~f:Desktop_link.Connect_reply.of_json))
;;

let quit () =
  Option.iter (stored ()) ~f:(fun key ->
    Browser.set_app_html
      {|<div class="signin-page"><div class="signin"><div class="brand"><span class="logo">prigh</span></div><h1>Quitting prigh…</h1><p class="signin-sub">The window closes once this computer's tool host has stopped.</p></div></div>|};
    Browser.post
      ~url:Desktop_link.quit_path
      ~body:(Desktop_link.quit_body ~key)
      ~f:(fun _ -> ()))
;;
