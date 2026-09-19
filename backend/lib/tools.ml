open! Core
open! Import

let all =
  [ Tool_bash.tool
  ; Tool_read.tool
  ; Tool_write.tool
  ; Tool_edit.tool
  ; Tool_ls.tool
  ; Tool_grep.tool
  ; Tool_find.tool
  ]
;;

let find name = List.find all ~f:(fun t -> String.equal (Tool.name t) name)
let specs tools = List.map tools ~f:(fun (t : Tool.t) -> t.spec)

let for_context ~parent ~depth ?only () =
  let available =
    List.filter parent ~f:(fun t ->
      not (depth >= 2 && String.equal (Tool.name t) "subagent"))
  in
  match only with
  | None -> Ok available
  | Some names ->
    let valid = List.map available ~f:Tool.name in
    let unknown =
      List.filter names ~f:(fun name ->
        not (List.mem valid name ~equal:String.equal))
    in
    if not (List.is_empty unknown)
    then
      Or_error.errorf
        "unknown tool%s %s; valid tools: %s"
        (if List.length unknown = 1 then "" else "s")
        (String.concat ~sep:", " (List.map unknown ~f:(sprintf "%S")))
        (String.concat ~sep:", " valid)
    else
      Ok
        (List.filter available ~f:(fun t ->
           List.mem names (Tool.name t) ~equal:String.equal))
;;
