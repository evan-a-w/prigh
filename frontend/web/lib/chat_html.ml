open! Core
open! Import

let present nodes = List.filter nodes ~f:(fun n -> not (phys_equal n Node.none))
let div cls children = Node.div ~attrs:[ Attr.class_ cls ] (present children)
let span cls text = Node.span ~attrs:[ Attr.class_ cls ] [ Node.text text ]

let folded ~cls ~label ~preview body =
  Node.details
    ~attrs:[ Attr.class_ cls ]
    [ Node.summary [ span "label" label; span "preview" preview ]; body ]
;;

let first_line s =
  String.split_lines s
  |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
  |> Option.value_map ~default:"" ~f:String.strip
;;

let plural n word = sprintf "%d %s%s" n word (if n = 1 then "" else "s")
