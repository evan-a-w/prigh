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

let text ?selector node =
  List.iter (select ?selector node) ~f:(fun node ->
    print_endline (Node_helpers.inner_text node))
;;
