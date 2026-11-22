open! Core
open Bonsai_web
open Js_of_ocaml

(* The screens, by URL hash; the first is the default. *)
let screens =
  [ ( "chat"
    , fun ~tip (local_ graph) ->
        Shell.component ~tip ~main:(Screen_chat.component ~tip graph) graph )
  ; ("review", fun ~tip (local_ graph) -> Screen_review.component ~tip graph)
  ; ("terminal", fun ~tip (local_ graph) -> Screen_terminal.component ~tip graph)
  ; ("agents", fun ~tip (local_ graph) -> Screen_agents.component ~tip graph)
  ; ("files", fun ~tip (local_ graph) -> Screen_files.component ~tip graph)
  ; ("confirm", fun ~tip (local_ graph) -> Screen_confirm.component ~tip graph)
  ; ("question", fun ~tip (local_ graph) -> Screen_confirm.question ~tip graph)
  ]
;;

let () =
  let hash =
    String.chop_prefix_if_exists (Js.to_string Dom_html.window##.location##.hash) ~prefix:"#"
  in
  let screen =
    match List.Assoc.find screens hash ~equal:String.equal with
    | Some s -> s
    | None -> snd (List.hd_exn screens)
  in
  Start.start (fun (local_ graph) ->
    let tip = Tip.component graph in
    screen ~tip graph)
;;
