open! Core
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

module Element = struct
  type t =
    { node : Node_helpers.t
    ; parent : int option
    ; tag : string
    ; id : string option
    ; classes : string list
    }
end

module Compound = struct
  type t =
    { tag : string option
    ; ids : string list
    ; classes : string list
    }

  let matches t (e : Element.t) =
    Option.for_all t.tag ~f:(String.equal e.tag)
    && List.for_all t.ids ~f:(fun id ->
      Option.equal String.equal e.id (Some id))
    && List.for_all t.classes ~f:(List.mem e.classes ~equal:String.equal)
  ;;

  let is_name_char c =
    Char.is_alphanum c || Char.equal c '-' || Char.equal c '_'
  ;;

  (* [tag], [.class] and [#id]s only; anything else is for lambdasoup. *)
  let parse s =
    let n = String.length s in
    let name i =
      let j = ref i in
      while !j < n && is_name_char s.[!j] do
        incr j
      done;
      if !j = i then None else Some (String.sub s ~pos:i ~len:(!j - i), !j)
    in
    let rec parts i (t : t) =
      if i = n
      then Some t
      else (
        match s.[i] with
        | '.' ->
          Option.bind
            (name (i + 1))
            ~f:(fun (c, j) -> parts j { t with classes = c :: t.classes })
        | '#' ->
          Option.bind
            (name (i + 1))
            ~f:(fun (id, j) -> parts j { t with ids = id :: t.ids })
        | _ -> None)
    in
    let empty = { tag = None; ids = []; classes = [] } in
    match name 0 with
    | Some (tag, i) ->
      if String.exists tag ~f:Char.is_uppercase || String.equal tag "widget"
      then None
      else parts i { empty with tag = Some tag }
    | None -> parts 0 empty
  ;;
end

type t =
  { root : Node_helpers.t
  ; elements : Element.t array Lazy.t
  ; soup : (Soup.soup Soup.node * Node_helpers.t Int.Table.t) Lazy.t
  }

let index root =
  let elements = Queue.create () in
  let rec go parent : Node_helpers.t -> unit = function
    | Text _ | Widget -> ()
    | Element { tag_name; attributes; children; _ } as node ->
      let attribute name =
        List.Assoc.find attributes ~equal:String.equal name
      in
      let i = Queue.length elements in
      Queue.enqueue
        elements
        { Element.node
        ; parent
        ; tag = tag_name
        ; id = attribute "id"
        ; classes =
            Option.value_map (attribute "class") ~default:[] ~f:(fun c ->
              String.split_on_chars c ~on:[ ' '; '\t'; '\n'; '\r'; '\012' ]
              |> List.filter ~f:(Fn.non String.is_empty))
        };
      List.iter children ~f:(go (Some i))
  in
  go None root;
  Queue.to_array elements
;;

let soup_id = "soup-id"

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
          ; [ soup_id, Stdlib.string_of_int id ]
          ; List.map handlers ~f:(fun (name, _) -> name, "<event-handler>")
          ; attributes
          ]
        |> String.Map.of_alist_exn
        |> Map.to_alist
      in
      let element = Soup.create_element tag_name ~attributes in
      List.iter children ~f:(fun child ->
        Soup.append_child element (convert child));
      Soup.coerce element
  in
  let soup = Soup.create_soup () in
  Soup.append_root soup (convert root);
  soup, by_id
;;

let of_node root =
  { root; elements = lazy (index root); soup = lazy (convert root) }
;;

let root t = t.root

(* Descendant combinators only: the nearest ancestor matching each compound
   is as good as any. *)
let select_simple t compounds =
  let elements = force t.elements in
  let rec ancestors parent = function
    | [] -> true
    | compound :: rest ->
      (match parent with
       | None -> false
       | Some i ->
         let e = elements.(i) in
         if Compound.matches compound e
         then ancestors e.parent rest
         else ancestors e.parent (compound :: rest))
  in
  match List.rev compounds with
  | [] -> []
  | last :: rest ->
    Array.filter elements ~f:(fun (e : Element.t) ->
      Compound.matches last e && ancestors e.parent rest)
    |> Array.to_list
    |> List.map ~f:(fun (e : Element.t) -> e.node)
;;

let select t ~selector =
  let compounds =
    String.split_on_chars selector ~on:[ ' ' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Compound.parse
    |> Option.all
  in
  match compounds with
  | Some (_ :: _ as compounds) -> select_simple t compounds
  | Some [] | None ->
    let soup, by_id = force t.soup in
    Soup.select selector soup
    |> Soup.to_list
    |> List.map ~f:(fun element ->
      Hashtbl.find_exn
        by_id
        (Stdlib.int_of_string (Soup.R.attribute soup_id element)))
;;
