open! Core
open! Import

let of_json json =
  let open Or_error.Let_syntax in
  let%bind head = Json.string_opt_field json "head" in
  let%map entries = Json.list_field json "entries" ~f:Entry.of_json in
  head, entries
;;

let line text =
  String.split_lines text
  |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
  |> Option.value_map ~default:"" ~f:String.strip
;;

let user_line ({ text; images; at = _ } : Message.User.t) =
  match line text, images with
  | "", image :: _ -> Image.to_string_hum image
  | line, _ -> line
;;

let first_line (message : Message.t) =
  match message with
  | User user -> user_line user
  | Assistant { content; _ } ->
    List.find_map content ~f:(function
      | Text text when not (String.is_empty (String.strip text)) ->
        Some (line text)
      | Tool_call call -> Some (call.name ^ " …")
      | Text _ | Thinking _ -> None)
    |> Option.value ~default:"(assistant)"
  | Tool_result { tool_name; text; _ } ->
    (match line text with
     | "" -> tool_name
     | line -> tool_name ^ ": " ^ line)
;;

let users entries =
  List.filter_map entries ~f:(fun (entry : Entry.t) ->
    match entry.kind with
    | Message (User user) -> Some (entry, user)
    | _ -> None)
;;

let user_items entries =
  let users = users entries in
  let count = List.length users in
  List.mapi users ~f:(fun i ((entry : Entry.t), user) ->
    Picker.Item.create
      ~id:entry.id
      ~detail:(sprintf "#%d" (i + 1))
      ~search:user.text
      ~marked:(i = count - 1)
      (user_line user))
;;

let user_text entries id =
  List.find_map (users entries) ~f:(fun ((entry : Entry.t), user) ->
    Option.some_if (String.equal entry.id id) user.text)
;;

let tree_items entries ~head =
  let messages =
    List.filter_map entries ~f:(fun (e : Entry.t) ->
      match e.kind with
      | Message m -> Some (e, m)
      | _ -> None)
  in
  let by_id = String.Table.create () in
  List.iter entries ~f:(fun (e : Entry.t) ->
    Hashtbl.set by_id ~key:e.id ~data:e);
  (* Non-message entries (model changes, names, …) are skipped: a message
     hangs off its nearest message ancestor. *)
  let rec message_parent (e : Entry.t) =
    match Option.bind e.parent ~f:(Hashtbl.find by_id) with
    | None -> None
    | Some ({ kind = Message _; _ } as p) -> Some p.id
    | Some p -> message_parent p
  in
  let children =
    String.Map.of_alist_multi
      (List.filter_map messages ~f:(fun (e, m) ->
         Option.map (message_parent e) ~f:(fun parent -> parent, (e, m))))
  in
  let roots =
    List.filter messages ~f:(fun (e, _) -> Option.is_none (message_parent e))
  in
  let active =
    let rec go id acc =
      match Hashtbl.find by_id id with
      | None -> acc
      | Some (e : Entry.t) ->
        let acc = Set.add acc id in
        Option.value_map e.parent ~default:acc ~f:(fun p -> go p acc)
    in
    Option.value_map head ~default:String.Set.empty ~f:(fun h ->
      go h String.Set.empty)
  in
  let rec walk depth ((e : Entry.t), (m : Message.t)) =
    let glyph =
      match m with
      | User _ -> ">"
      | Assistant _ -> "·"
      | Tool_result _ -> "⚙"
    in
    let text = first_line m in
    Picker.Item.create
      ~id:e.id
      ~search:text
      ~marked:(Set.mem active e.id)
      (String.make (2 * depth) ' ' ^ glyph ^ " " ^ text)
    (* A conversation stays at its depth; a branch point indents its
       branches. *)
    ::
    (match Option.value (Map.find children e.id) ~default:[] with
     | [ only ] -> walk depth only
     | branches -> List.concat_map branches ~f:(walk (depth + 1)))
  in
  match roots with
  | [ root ] -> walk 0 root
  | roots -> List.concat_map roots ~f:(walk 0)
;;
