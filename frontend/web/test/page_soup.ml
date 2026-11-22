open! Core
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

type t =
  { root : Node_helpers.t
  ; soup : (Soup.soup Soup.node * Node_helpers.t Int.Table.t) Lazy.t
  }

let id_key = "soup-id"

(* As [Node_helpers.select] converts, but once per page rather than once per
   query, and with int ids: its [Unique_id.Int] is an Int63, boxed int64
   arithmetic under js_of_ocaml. *)
let convert root =
  let by_id = Int.Table.create () in
  let next = ref 0 in
  let rec convert : Node_helpers.t -> _ = function
    | Text s -> Soup.coerce (Soup.create_text s)
    | Widget -> Soup.coerce (Soup.create_element "widget" ~attributes:[])
    | Element { tag_name; attributes; handlers; key; children; hooks; _ } as t
      ->
      let id = !next in
      incr next;
      Hashtbl.add_exn by_id ~key:id ~data:t;
      let attributes =
        List.concat
          [ List.map hooks ~f:(fun (name, _) -> name, "<hook>")
          ; Option.to_list (Option.map key ~f:(fun key -> "key", key))
          ; [ id_key, Stdlib.string_of_int id ]
          ; List.map handlers ~f:(fun (name, _) -> name, "<event-handler>")
          ; attributes
          ]
        |> String.Map.of_alist_exn
        |> Map.to_alist
      in
      let element = Soup.create_element tag_name ~attributes in
      List.iter children ~f:(fun child -> Soup.append_child element (convert child));
      Soup.coerce element
  in
  let soup = Soup.create_soup () in
  Soup.append_root soup (convert root);
  soup, by_id
;;

let of_node root = { root; soup = lazy (convert root) }
let root t = t.root

let select t ~selector =
  let soup, by_id = force t.soup in
  Soup.select selector soup
  |> Soup.to_list
  |> List.map ~f:(fun element ->
    Hashtbl.find_exn
      by_id
      (Stdlib.int_of_string (Soup.R.attribute id_key element)))
;;
