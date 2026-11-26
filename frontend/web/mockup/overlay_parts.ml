open! Core
open Bonsai_web
open Loon_ui
module Node = Vdom.Node
module Attr = Vdom.Attr

let div cls children = Node.div ~attrs:[ Attr.class_ cls ] children
let span cls children = Node.span ~attrs:[ Attr.class_ cls ] children

let highlight ~query text =
  let query = String.lowercase query in
  let n = String.length query in
  let rec go i qi acc run =
    let flush acc =
      match run with
      | [] -> acc
      | _ -> Node.text (String.of_char_list (List.rev run)) :: acc
    in
    if i >= String.length text
    then List.rev (flush acc)
    else (
      let c = text.[i] in
      if qi < n && Char.equal (Char.lowercase c) query.[qi]
      then
        go
          (i + 1)
          (qi + 1)
          (span "font-semibold text-foreground" [ Node.text (String.of_char c) ]
           :: flush acc)
          []
      else go (i + 1) qi acc (c :: run))
  in
  go 0 0 [] []
;;

let footer_hints hints =
  div
    "flex items-center gap-4 border-t px-3 py-2 text-[11px] \
     text-muted-foreground"
    (List.map hints ~f:(fun (keys, label) ->
       span
         "flex items-center gap-1.5"
         [ Kbd.group (List.map keys ~f:(fun k -> Kbd.view [ Node.text k ]))
         ; Node.text label
         ]))
;;
