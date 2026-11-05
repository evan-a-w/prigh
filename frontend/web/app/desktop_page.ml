open! Core
open Js_of_ocaml
open Prigh_web

let storage () =
  Option.try_with (fun () ->
    Js.Optdef.to_option Dom_html.window##.sessionStorage)
  |> Option.join
;;

let stored_at name =
  Option.bind (storage ()) ~f:(fun storage ->
    Js.Opt.to_option (storage##getItem (Js.string name))
    |> Option.map ~f:Js.to_string)
;;

let store name value =
  Option.iter (storage ()) ~f:(fun storage ->
    storage##setItem (Js.string name) (Js.string value))
;;

let stored () = stored_at Desktop_link.storage_key

let key () =
  match Desktop_link.key_of_hash (Browser.hash ()) with
  | Some key ->
    store Desktop_link.storage_key key;
    (* The sign-in code, if any, is the sidecar's to give: only the key was
       in the fragment. *)
    Browser.clear_hash ();
    Some key
  | None -> stored ()
;;

(* On the backend's own site: the hint says which tool host is this
   machine's, which the page then prefers, as a [prigh web] sign-in does. *)
let take_hint () =
  match Desktop_link.Hint.of_hash (Browser.hash ()) with
  | Some { host } ->
    store Desktop_link.hint_storage_key "1";
    Option.iter host ~f:(Browser.set_item Signin_link.prefer_host_key);
    Browser.clear_hash ()
  | None -> ()
;;

let take_shell () =
  if Desktop_link.browser_shell_of_hash (Browser.hash ())
  then store Desktop_link.shell_storage_key "browser"
;;

let in_browser () = Option.is_some (stored_at Desktop_link.shell_storage_key)
let follow url = Desktop_link.follow ~browser:(in_browser ()) url

let on_backend_site () =
  Option.is_some (stored_at Desktop_link.hint_storage_key)
;;

let active () = Option.is_some (stored ()) || on_backend_site ()

let status ~key ~f =
  Browser.post
    ~content_type:"application/json"
    ~url:Desktop_link.status_path
    ~body:(Desktop_link.status_body ~key)
    ~f:(fun reply -> f (Option.bind reply ~f:Desktop_link.Status.of_json))
;;

let connect ~key ~address ~user ~token ~plaintext ~f =
  Browser.post
    ~content_type:"application/json"
    ~url:Desktop_link.connect_path
    ~body:(Desktop_link.connect_body ~key ~address ~user ~token ~plaintext)
    ~f:(fun reply ->
      f (Option.bind reply ~f:Desktop_link.Connect_reply.of_json))
;;

(* Over whatever the page shows (the app, which keeps redrawing its root, or
   the connect form) until the window closes. *)
let quitting_html what =
  {|<div class="signin"><div class="brand"><span class="logo">prigh</span><span class="brand-sub">desktop</span></div><h1>Quitting prigh…</h1><p class="signin-sub">|}
  ^ what
  ^ {|</p></div>|}
;;

let show_quitting
      ?(what = "The window closes once this computer's tool host has stopped.")
      ()
  =
  let overlay = Dom_html.createDiv Dom_html.document in
  overlay##.className := Js.string "signin-page desktop-quitting";
  overlay##setAttribute (Js.string "role") (Js.string "status");
  overlay##.innerHTML := Js.string (quitting_html what);
  Dom.appendChild Dom_html.document##.body overlay;
  overlay
;;

let quit () =
  match stored () with
  | Some key ->
    ignore (show_quitting () : Dom_html.divElement Js.t);
    Browser.post
      ~content_type:"application/json"
      ~url:Desktop_link.quit_path
      ~body:(Desktop_link.quit_body ~key)
      ~f:(fun _ -> ())
  | None when on_backend_site () && in_browser () ->
    let overlay =
      show_quitting
        ~what:"Closing the window stops this computer's tool host."
        ()
    in
    Dom_html.window##close;
    (* A browser may refuse to close a window it did not open by script. *)
    ignore
      (Dom_html.setTimeout
         (fun () ->
            overlay##.innerHTML
            := Js.string (quitting_html "Close this window to quit prigh."))
         500.
       : Dom_html.timeout_id_safe)
  | None when on_backend_site () ->
    ignore (show_quitting () : Dom_html.divElement Js.t);
    Browser.navigate Desktop_link.quit_url
  | None -> ()
;;
