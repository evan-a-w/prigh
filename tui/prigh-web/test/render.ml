open! Core
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

let select ?selector node =
  let node = Node_helpers.unsafe_convert_exn node in
  match selector with
  | None -> [ node ]
  | Some selector -> Node_helpers.select node ~selector
;;

let html ?selector node =
  List.iter (select ?selector node) ~f:(fun node ->
    print_endline (Node_helpers.to_string_html node))
;;

let mark_glyph (node : Node_helpers.t) =
  match node with
  | Element e when Node_helpers.has_class ~cls:"mark" node ->
    List.find_map e.children ~f:(fun icon ->
      List.find_map
        [ "check", "✓"
        ; "cross", "✕"
        ; "dash", "–"
        ; "square", "■"
        ; "warning", "⚠"
        ; "next", "▸"
        ; "ring", "○"
        ]
        ~f:(fun (name, glyph) ->
          Option.some_if
            (Node_helpers.is_tag ~tag:"icon" icon
             && Node_helpers.has_class ~cls:name icon)
            glyph))
  | _ -> None
;;

let text ?selector node =
  List.iter (select ?selector node) ~f:(fun node ->
    Node_helpers.map node ~f:(fun node ->
      match node, mark_glyph node with
      | Element e, Some glyph ->
        `Replace_with (Element { e with children = [ Text glyph ] })
      | _ -> `Continue)
    |> Node_helpers.inner_text
    |> print_endline)
;;
