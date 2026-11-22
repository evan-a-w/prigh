open! Import

let rec merge ?(defaults = []) attrs (node : Node.t) : Node.t =
  match node with
  | Element e ->
    Element
      (Node.Element.map_attrs e ~f:(fun a ->
         let defaults =
           List.filter_map defaults ~f:(fun (name, attr) ->
             if String.equal name "data-slot"
             then Some (Attr.suppress_merge_warnings attr)
             else if Attr.Expert.contains_name name a
             then None
             else Some attr)
         in
         (* The part's attributes replace the child's of the same name (a
            collapsible trigger's [data-state] over a tooltip trigger's):
            on purpose, so without virtual_dom's warning. *)
         Attr.many
           ((a :: List.map attrs ~f:Attr.suppress_merge_warnings) @ defaults)))
  | Lazy { t; _ } -> merge ~defaults attrs (Lazy.force t)
  | Fragment (((Element _ | Lazy _) as first) :: rest) ->
    Node.fragment (merge ~defaults attrs first :: rest)
  | Text _ | Fragment _ | None | Widget _ ->
    Node.span ~attrs:(List.map defaults ~f:snd @ attrs) [ node ]
;;
