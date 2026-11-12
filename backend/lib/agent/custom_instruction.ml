open! Core
open! Import

type t =
  { id : string
  ; title : string option [@sexp.option] [@jsonaf.option]
  ; text : string
  }
[@@deriving sexp, jsonaf, equal]

let line t =
  let head =
    match t.title with
    | Some title -> sprintf "[%s] (%s) " t.id title
    | None -> sprintf "[%s] " t.id
  in
  match String.split_lines (String.strip t.text) with
  | [] -> String.rstrip head
  | first :: rest ->
    String.concat
      ~sep:"\n"
      ((head ^ first)
       :: List.map rest ~f:(fun l -> if String.is_empty l then l else "  " ^ l)
      )
;;

let heading =
  "Custom instructions (the user's standing instructions; follow them):"
;;

let section = function
  | [] -> None
  | ts -> Some (String.concat ~sep:"\n" (heading :: List.map ts ~f:line))
;;

module Change = struct
  type instruction = t [@@deriving sexp_of]

  type t =
    | Added of instruction
    | Edited of instruction
    | Removed of instruction
  [@@deriving sexp_of]
end

let find ts id = List.find ts ~f:(fun t -> String.equal t.id id)

let changes ~told now =
  let removed =
    List.filter_map told ~f:(fun t ->
      Option.some_if (Option.is_none (find now t.id)) (Change.Removed t))
  in
  let changed =
    List.filter_map now ~f:(fun t ->
      match find told t.id with
      | None -> Some (Change.Added t)
      | Some before -> Option.some_if (not (equal before t)) (Change.Edited t))
  in
  removed @ changed
;;

let note ~told now =
  match changes ~told now with
  | [] -> None
  | changes ->
    (match now with
     | [] ->
       Some
         "[Custom instructions: the user has no standing instructions any \
          more: none of the earlier ones apply.]"
     | _ ->
       let kept =
         List.length now
         - List.count changes ~f:(function
           | Added _ | Edited _ -> true
           | Removed _ -> false)
       in
       if kept * 2 < List.length now || List.is_empty told
       then
         Some
           (sprintf
              "[%s\n%s\nThese replace any earlier custom instructions.]"
              (String.chop_suffix_exn heading ~suffix:":" ^ " are now:")
              (String.concat ~sep:"\n" (List.map now ~f:line)))
       else
         Some
           (sprintf
              "[Custom instructions changed (the user's standing instructions; \
               follow them; the others are unchanged):\n\
               %s]"
              (String.concat
                 ~sep:"\n"
                 (List.map changes ~f:(function
                    | Added t -> "Added " ^ line t
                    | Edited t -> "Edited, now " ^ line t
                    | Removed t -> sprintf "Removed [%s]" t.id)))))
;;

let apply ts ~id note =
  match note, find ts id with
  | None, _ -> List.filter ts ~f:(fun t -> not (String.equal t.id id))
  | Some note, Some _ ->
    List.map ts ~f:(fun t -> if String.equal t.id id then note else t)
  | Some note, None -> ts @ [ note ]
;;
