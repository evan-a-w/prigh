open! Core
open Js_of_ocaml
module Theme = Prigh_web.Theme

(* Every variable a theme may set, so that the previous one's go. *)
let vars = [ "--sodium"; "--sodium-ink" ]

let apply theme =
  let root = Dom_html.document##.documentElement in
  List.iter (Theme.attributes theme) ~f:(fun (name, value) ->
    root##setAttribute (Js.string name) (Js.string value));
  let style = root##.style in
  List.iter vars ~f:(fun var ->
    ignore (style##removeProperty (Js.string var) : Js.js_string Js.t));
  List.iter (Theme.vars theme) ~f:(fun (var, value) ->
    ignore
      (style##setProperty (Js.string var) (Js.string value) Js.undefined
       : Js.js_string Js.t));
  Pref_cache.save_theme Login.Storage.browser theme;
  Terminal_widget.set_theme theme
;;
