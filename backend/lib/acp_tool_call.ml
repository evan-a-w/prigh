open! Core
open! Import

let kind = function
  | "bash" | "job_input" | "job_kill" | "job_output" | "job_status" | "job_wait"
    -> "execute"
  | "read" | "ls" -> "read"
  | "grep" | "find" -> "search"
  | "edit" | "write" | "apply_patch" -> "edit"
  | "web_fetch" | "web_search" -> "fetch"
  | "update_plan" -> "think"
  | "exit_plan_mode" -> "switch_mode"
  | _ -> "other"
;;

let edits_files = function
  | "edit" | "write" | "apply_patch" -> true
  | _ -> false
;;

let parse_args text =
  match Json.parse text with
  | Ok (`Object _ as json) -> json
  | Ok _ | Error _ -> `Object []
;;

let string_arg args key = Option.bind (Json.member key args) ~f:Json.string
let int_arg args key = Option.bind (Json.member key args) ~f:Json.int

let first_line ?(max = 80) s =
  let line = Option.value (List.hd (String.split_lines s)) ~default:"" in
  let line =
    if List.length (String.split_lines s) > 1 then line ^ " …" else line
  in
  if String.length line > max then String.prefix line (max - 1) ^ "…" else line
;;

let patch_paths args =
  match Option.map (string_arg args "input") ~f:Apply_patch.parse with
  | Some (Ok operations) ->
    List.concat_map operations ~f:(fun (op : Apply_patch.Operation.t) ->
      match op with
      | Add { path; _ } | Delete { path } -> [ path ]
      | Update { path; move_to; _ } -> path :: Option.to_list move_to)
  | Some (Error _) | None -> []
;;

let shown ~cwd path = Tool_edit.relative_path ~cwd (Tool.resolve ~cwd path)

let title ~name ~args ~cwd =
  let path key = Option.map (string_arg args key) ~f:(shown ~cwd) in
  let with_ verb = function
    | Some x -> verb ^ " " ^ x
    | None -> verb
  in
  match name with
  | "bash" ->
    Option.value_map (string_arg args "command") ~default:"bash" ~f:first_line
  | "read" -> with_ "Read" (path "path")
  | "write" -> with_ "Write" (path "path")
  | "edit" -> with_ "Edit" (path "path")
  | "ls" -> with_ "List" (Some (Option.value (path "path") ~default:"."))
  | "apply_patch" ->
    (match patch_paths args with
     | [] -> "Patch"
     | paths ->
       "Patch " ^ String.concat ~sep:", " (List.map paths ~f:(shown ~cwd)))
  | "grep" | "find" ->
    sprintf
      "%s %s%s"
      name
      (Option.value_map
         (string_arg args "pattern")
         ~default:""
         ~f:(sprintf "%S"))
      (Option.value_map (path "path") ~default:"" ~f:(sprintf " in %s"))
  | "web_fetch" -> with_ "Fetch" (string_arg args "url")
  | "web_search" -> with_ "Search the web for" (string_arg args "query")
  | "subagent" ->
    with_
      "Subagent:"
      (Option.map (string_arg args "task") ~f:(first_line ~max:70))
  | "ask_user" -> "Question for you"
  | "update_plan" -> "Update the plan"
  | "exit_plan_mode" -> "Leave plan mode"
  | name -> name
;;

let location ?line path =
  `Object
    (("path", `String path)
     :: Option.value_map line ~default:[] ~f:(fun l ->
       [ "line", `Number (Int.to_string l) ]))
;;

let locations ~name ~args ~cwd =
  let resolve path = Tool.resolve ~cwd path in
  match name with
  | "read" ->
    Option.value_map (string_arg args "path") ~default:[] ~f:(fun p ->
      [ location ?line:(int_arg args "offset") (resolve p) ])
  | "edit" | "write" | "ls" ->
    Option.value_map (string_arg args "path") ~default:[] ~f:(fun p ->
      [ location (resolve p) ])
  | "apply_patch" ->
    List.map (patch_paths args) ~f:(fun p -> location (resolve p))
  | _ -> []
;;

let diff ~path ~old_text ~new_text =
  `Object
    [ "type", `String "diff"
    ; "path", `String path
    ; ( "oldText"
      , Option.value_map old_text ~default:`Null ~f:(fun s -> `String s) )
    ; "newText", `String new_text
    ]
;;

let diffs_of_args ~name ~args ~cwd =
  let resolve path = Tool.resolve ~cwd path in
  match name with
  | "write" ->
    (match string_arg args "path", string_arg args "content" with
     | Some path, Some content ->
       [ diff ~path:(resolve path) ~old_text:None ~new_text:content ]
     | _ -> [])
  | "edit" ->
    (match
       ( string_arg args "path"
       , Option.bind (Json.member "edits" args) ~f:Json.list )
     with
     | Some path, Some edits ->
       List.filter_map edits ~f:(fun edit ->
         match string_arg edit "old_text", string_arg edit "new_text" with
         | Some old_text, Some new_text ->
           Some (diff ~path:(resolve path) ~old_text:(Some old_text) ~new_text)
         | _ -> None)
     | _ -> [])
  | "apply_patch" ->
    (match Option.map (string_arg args "input") ~f:Apply_patch.parse with
     | Some (Ok operations) ->
       List.concat_map operations ~f:(fun (op : Apply_patch.Operation.t) ->
         match op with
         | Add { path; contents } ->
           [ diff ~path:(resolve path) ~old_text:None ~new_text:contents ]
         | Delete _ -> []
         | Update { path; move_to; chunks } ->
           let path = resolve (Option.value move_to ~default:path) in
           List.map chunks ~f:(fun (chunk : Apply_patch.Chunk.t) ->
             diff
               ~path
               ~old_text:(Some (String.concat_lines chunk.old_lines))
               ~new_text:(String.concat_lines chunk.new_lines)))
     | Some (Error _) | None -> [])
  | _ -> []
;;

let text ?kind text =
  let text =
    match kind with
    | Some ("execute" | "search") when not (String.is_empty text) ->
      let fence =
        if String.is_substring text ~substring:"```" then "````" else "```"
      in
      sprintf
        "%s\n%s%s%s"
        fence
        text
        (if String.is_suffix text ~suffix:"\n" then "" else "\n")
        fence
    | Some _ | None -> text
  in
  `Object
    [ "type", `String "content"
    ; "content", `Object [ "type", `String "text"; "text", `String text ]
    ]
;;
