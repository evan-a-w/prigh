open! Core
open! Import

let str json key = Option.bind (Json.member key json) ~f:Json.string

let kind name : Adapter_event.Tool_kind.t =
  match name with
  | "Bash" -> Command
  | "Edit" | "MultiEdit" | "Write" | "NotebookEdit" -> File_change
  | "Read" | "NotebookRead" -> Read
  | "Grep" | "Glob" -> Search
  | "WebSearch" | "WebFetch" -> Web_search
  | "TodoWrite" -> Todo
  | "Task" | "Agent" -> Subagent
  | _ ->
    (match String.chop_prefix name ~prefix:"mcp__" with
     | Some rest ->
       (match String.substr_index rest ~pattern:"__" with
        | Some i ->
          Mcp
            { server = String.prefix rest i
            ; tool = String.drop_prefix rest (i + 2)
            }
        | None -> Other name)
     | None -> Other name)
;;

let first_line ?(max = 120) s =
  let lines = String.split_lines s in
  let line = Option.value (List.hd lines) ~default:"" in
  let line = if List.length lines > 1 then line ^ " …" else line in
  if String.length line > max then String.prefix line (max - 1) ^ "…" else line
;;

let title ~name ~input =
  let with_ key =
    Option.map (str input key) ~f:(fun v -> name ^ " " ^ first_line v)
  in
  let title =
    match kind name with
    | Command ->
      Option.map (str input "command") ~f:(fun c -> "Bash: " ^ first_line c)
    | File_change | Read -> with_ "file_path"
    | Search ->
      Option.map (str input "pattern") ~f:(fun p ->
        name
        ^ " "
        ^ first_line p
        ^
        match str input "path" with
        | Some path -> " in " ^ path
        | None -> "")
    | Web_search ->
      (match with_ "query" with
       | Some t -> Some t
       | None -> with_ "url")
    | Subagent -> with_ "description"
    | Mcp { server; tool } -> Some (server ^ ": " ^ tool)
    | Todo -> Some "Update todos"
    | Other _ -> None
  in
  Option.value title ~default:name
;;

let hunk_header json =
  let int key =
    Option.value (Option.bind (Json.member key json) ~f:Json.int) ~default:0
  in
  sprintf
    "@@ -%d,%d +%d,%d @@"
    (int "oldStart")
    (int "oldLines")
    (int "newStart")
    (int "newLines")
;;

(* Claude Code's [structuredPatch]: hunks whose lines carry their ' ', '-'
   or '+' prefix. *)
let of_structured_patch ~path ~added hunks =
  let hunks =
    List.map hunks ~f:(fun hunk ->
      let lines =
        Option.value
          (Option.bind (Json.member "lines" hunk) ~f:Json.list)
          ~default:[]
        |> List.filter_map ~f:Json.string
      in
      String.concat ~sep:"\n" (hunk_header hunk :: lines))
  in
  match hunks with
  | [] -> None
  | hunks ->
    let side prefix =
      if String.is_prefix path ~prefix:"/" then path else prefix ^ path
    in
    let old = if added then "/dev/null" else side "a/" in
    Some
      (String.concat
         ~sep:"\n"
         ((sprintf "--- %s" old :: sprintf "+++ %s" (side "b/") :: hunks)
          @ [ "" ]))
;;

let snippet_diff ~path ~old ~new_ = (Text_diff.unified ~path ~old ~new_).diff

let changes ~name ~input ~result : Adapter_event.Change.t list =
  match name, str input "file_path" with
  | ("Edit" | "MultiEdit" | "Write"), Some path ->
    let result_str key = Option.bind result ~f:(fun r -> str r key) in
    let added =
      match name, result_str "type" with
      | "Write", Some "create" -> true
      | _ -> false
    in
    let patch =
      Option.bind result ~f:(fun r ->
        match
          Option.bind (Json.member "gitDiff" r) ~f:(fun g -> str g "patch")
        with
        | Some patch when not (String.is_empty patch) -> Some patch
        | _ ->
          Option.bind (Json.member "structuredPatch" r) ~f:Json.list
          |> Option.bind ~f:(of_structured_patch ~path ~added))
    in
    let from_input () =
      match name with
      | "Write" ->
        let old = Option.value (result_str "originalFile") ~default:"" in
        snippet_diff
          ~path
          ~old
          ~new_:(Option.value (str input "content") ~default:"")
      | "Edit" ->
        snippet_diff
          ~path
          ~old:(Option.value (str input "old_string") ~default:"")
          ~new_:(Option.value (str input "new_string") ~default:"")
      | _ ->
        Option.value
          (Option.bind (Json.member "edits" input) ~f:Json.list)
          ~default:[]
        |> List.map ~f:(fun edit ->
          snippet_diff
            ~path
            ~old:(Option.value (str edit "old_string") ~default:"")
            ~new_:(Option.value (str edit "new_string") ~default:""))
        |> String.concat
    in
    let diff =
      match patch with
      | Some patch -> patch
      | None -> from_input ()
    in
    [ { path; kind = (if added then Add else Update { move_to = None }); diff }
    ]
  | ("NotebookEdit" | _), _ ->
    (match name, str input "notebook_path" with
     | "NotebookEdit", Some path ->
       [ { path; kind = Update { move_to = None }; diff = "" } ]
     | _ -> [])
;;

let plan_of_todos input =
  let steps =
    Option.value
      (Option.bind (Json.member "todos" input) ~f:Json.list)
      ~default:[]
    |> List.filter_map ~f:(fun todo ->
      let%map.Option step = str todo "content" in
      let status : Plan.Status.t =
        match str todo "status" with
        | Some "in_progress" -> In_progress
        | Some "completed" -> Completed
        | _ -> Pending
      in
      { Plan.Step.step; status })
  in
  match steps with
  | [] -> None
  | steps -> Some { Plan.explanation = None; steps }
;;
