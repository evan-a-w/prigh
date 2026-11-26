open! Core
open! Import

type t =
  { name : string
  ; arguments : Json.t
  ; native_name : string
  }
[@@deriving sexp_of]

(* [name], or failing that any field named so in another case ([Query]). *)
let field (json : Json.t) name =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | Some v -> Some v
     | None -> List.Assoc.find fields ~equal:String.Caseless.equal name)
  | _ -> None
;;

let string_field json name =
  match field json name with
  | Some (`String s) -> Some s
  | _ -> None
;;

let first_string json names = List.find_map names ~f:(string_field json)

let list_field json name =
  match field json name with
  | Some (`Array items) -> Some items
  | _ -> None
;;

(* An object of the fields present. *)
let obj fields : Json.t = `Object (List.filter_opt fields)
let str s = `String s
let path_names = [ "file_path"; "path"; "notebook_path" ]

let shells =
  [ "bash"
  ; "sh"
  ; "zsh"
  ; "/bin/bash"
  ; "/bin/sh"
  ; "/bin/zsh"
  ; "/usr/bin/bash"
  ; "/usr/bin/zsh"
  ]
;;

(* [s] as one shell word (bare, single- or double-quoted, in pieces), if it
   is exactly one: else [None]. *)
let one_word s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n
    then Some (Buffer.contents b)
    else (
      match s.[i] with
      | '\'' ->
        (match String.index_from s (i + 1) '\'' with
         | Some j ->
           Buffer.add_string b (String.sub s ~pos:(i + 1) ~len:(j - i - 1));
           go (j + 1)
         | None -> None)
      | '"' ->
        let rec dq j =
          if j >= n
          then None
          else (
            match s.[j] with
            | '"' -> go (j + 1)
            | '\\' when j + 1 < n && String.mem "\"\\$`" s.[j + 1] ->
              Buffer.add_char b s.[j + 1];
              dq (j + 2)
            | '$' | '`' -> None
            | c ->
              Buffer.add_char b c;
              dq (j + 1))
        in
        dq (i + 1)
      | '\\' when i + 1 < n ->
        Buffer.add_char b s.[i + 1];
        go (i + 2)
      | ' '
      | '\t'
      | '\n'
      | ';'
      | '&'
      | '|'
      | '<'
      | '>'
      | '('
      | ')'
      | '$'
      | '`'
      | '*'
      | '?'
      | '['
      | '#'
      | '~' -> None
      | c ->
        Buffer.add_char b c;
        go (i + 1))
  in
  if String.is_empty s then None else go 0
;;

(* [bash -lc 'script'] (as Codex wraps a command) is [script]. *)
let unwrap_shell s =
  match String.lsplit2 (String.strip s) ~on:' ' with
  | Some (shell, rest) when List.mem shells shell ~equal:String.equal ->
    (match String.lsplit2 (String.lstrip rest) ~on:' ' with
     | Some (("-c" | "-lc"), script) ->
       Option.value (one_word (String.strip script)) ~default:s
     | _ -> s)
  | _ -> s
;;

let command input =
  match field input "command" with
  | Some (`String s) -> Some (unwrap_shell s)
  | Some (`Array parts) ->
    let parts =
      List.filter_map parts ~f:(function
        | `String s -> Some s
        | _ -> None)
    in
    (match parts with
     | [ shell; ("-c" | "-lc"); script ]
       when List.mem shells shell ~equal:String.equal -> Some script
     | parts -> Some (String.concat ~sep:" " parts))
  | _ -> None
;;

module Patch_file = struct
  type t =
    { path : string
    ; kind : Adapter_event.Change.Kind.t
    ; diff : string
    }
end

let kind_of_json json : Adapter_event.Change.Kind.t option =
  let of_type type_ ~move_to : Adapter_event.Change.Kind.t option =
    match String.lowercase type_ with
    | "add" | "create" | "added" -> Some Add
    | "delete" | "remove" | "deleted" -> Some Delete
    | "update" | "modify" | "updated" -> Some (Update { move_to })
    | _ -> None
  in
  match json with
  | `String type_ -> of_type type_ ~move_to:None
  | `Object _ ->
    Option.bind (string_field json "type") ~f:(fun type_ ->
      of_type
        type_
        ~move_to:(first_string json [ "move_path"; "movePath"; "move_to" ]))
  | _ -> None
;;

(* Codex's file changes: a list of [{path, kind, diff}], or an object from
   path to [{kind, diff}]. *)
let patch_files input =
  let of_entry ~path entry =
    let kind =
      Option.value
        (Option.bind (field entry "kind") ~f:kind_of_json)
        ~default:(Update { move_to = None })
    in
    let diff =
      Option.value (first_string entry [ "diff"; "unified_diff" ]) ~default:""
    in
    { Patch_file.path; kind; diff }
  in
  match field input "changes" with
  | Some (`Array entries) ->
    Some
      (List.filter_map entries ~f:(fun entry ->
         Option.map (string_field entry "path") ~f:(fun path ->
           of_entry ~path entry)))
  | Some (`Object entries) ->
    Some (List.map entries ~f:(fun (path, entry) -> of_entry ~path entry))
  | _ -> None
;;

let is_hunk_header line = String.is_prefix line ~prefix:"@@"

let is_file_header line =
  String.is_prefix line ~prefix:"--- " || String.is_prefix line ~prefix:"+++ "
;;

(* The diff's lines from its first hunk on. *)
let hunk_lines diff =
  String.split_lines diff |> List.drop_while ~f:(Fn.non is_hunk_header)
;;

let patch_of_files (files : Patch_file.t list) =
  let body (file : Patch_file.t) =
    match file.kind with
    | Delete -> [ "*** Delete File: " ^ file.path ]
    | Add ->
      let lines =
        match hunk_lines file.diff with
        | [] -> List.map (String.split_lines file.diff) ~f:(fun l -> "+" ^ l)
        | hunks ->
          List.filter hunks ~f:(fun l ->
            String.is_prefix l ~prefix:"+" && not (is_file_header l))
      in
      ("*** Add File: " ^ file.path) :: lines
    | Update { move_to } ->
      let header =
        ("*** Update File: " ^ file.path)
        :: Option.value_map move_to ~default:[] ~f:(fun to_ ->
          [ "*** Move to: " ^ to_ ])
      in
      header
      @ List.filter_map (hunk_lines file.diff) ~f:(fun line ->
        if is_hunk_header line
        then Some "@@"
        else if String.is_prefix line ~prefix:"\\" || is_file_header line
        then None
        else Some line)
  in
  String.concat
    ~sep:"\n"
    (("*** Begin Patch" :: List.concat_map files ~f:body) @ [ "*** End Patch" ])
  ^ "\n"
;;

let file_change ~native_name input =
  let path = first_string input path_names in
  let edit ~path edits =
    { name = "edit"
    ; arguments =
        obj
          [ Some ("path", str path)
          ; Some
              ( "edits"
              , `Array
                  (List.map edits ~f:(fun (old_text, new_text) ->
                     `Object
                       [ "old_text", str old_text; "new_text", str new_text ]))
              )
          ]
    ; native_name
    }
  in
  let old_new json =
    match
      ( first_string json [ "old_string"; "old_text" ]
      , first_string json [ "new_string"; "new_text" ] )
    with
    | Some o, Some n -> Some (o, n)
    | _ -> None
  in
  match path, old_new input, list_field input "edits", patch_files input with
  | Some path, Some edit_, _, _ -> edit ~path [ edit_ ]
  | Some path, None, Some edits, _ ->
    edit ~path (List.filter_map edits ~f:old_new)
  | Some path, None, None, _ when Option.is_some (field input "content") ->
    { name = "write"
    ; arguments =
        `Object
          [ "path", str path
          ; ( "content"
            , str (Option.value (string_field input "content") ~default:"") )
          ]
    ; native_name
    }
  | _, _, _, Some files ->
    { name = "apply_patch"
    ; arguments = `Object [ "input", str (patch_of_files files) ]
    ; native_name
    }
  | _ -> { name = native_name; arguments = input; native_name }
;;

let plan_step json =
  match
    first_string json [ "step"; "content"; "text" ], string_field json "status"
  with
  | Some step, status ->
    let status =
      match Option.map status ~f:String.lowercase with
      | Some ("in_progress" | "inprogress" | "active") -> "in_progress"
      | Some ("completed" | "done" | "complete") -> "completed"
      | _ -> "pending"
    in
    Some (`Object [ "step", str step; "status", str status ])
  | None, _ -> None
;;

let of_start ~(kind : Adapter_event.Tool_kind.t) ~native_name ~input =
  let keep () = { name = native_name; arguments = input; native_name } in
  match kind with
  | Command ->
    (match command input with
     | Some command ->
       { name = "bash"
       ; arguments = `Object [ "command", str command ]
       ; native_name
       }
     | None -> keep ())
  | File_change -> file_change ~native_name input
  | Read ->
    (match first_string input path_names with
     | Some path ->
       let int name =
         match field input name with
         | Some (`Number n) ->
           Option.map (Int.of_string_opt n) ~f:(fun n ->
             name, `Number (Int.to_string n))
         | _ -> None
       in
       { name = "read"
       ; arguments = obj [ Some ("path", str path); int "offset"; int "limit" ]
       ; native_name
       }
     | None -> keep ())
  | Search ->
    let pattern = first_string input [ "pattern"; "query"; "regex" ] in
    let path =
      first_string
        input
        [ "path"; "directory"; "searchpath"; "searchdirectory" ]
    in
    (match pattern with
     | Some pattern ->
       let name =
         if
           List.mem
             [ "glob"; "find"; "find_by_name"; "find_files" ]
             native_name
             ~equal:String.Caseless.equal
         then "find"
         else "grep"
       in
       { name
       ; arguments =
           obj
             [ Some ("pattern", str pattern)
             ; Option.map path ~f:(fun p -> "path", str p)
             ]
       ; native_name
       }
     | None -> keep ())
  | Web_search ->
    (match
       ( first_string input [ "url"; "uri" ]
       , first_string input [ "query"; "q"; "search_query" ] )
     with
     | Some url, _ ->
       { name = "web_fetch"
       ; arguments = `Object [ "url", str url ]
       ; native_name
       }
     | None, Some query ->
       { name = "web_search"
       ; arguments = `Object [ "query", str query ]
       ; native_name
       }
     | None, None -> keep ())
  | Mcp { server; tool } ->
    { name =
        (match Injected_tools.canonical_of_mcp ~server ~tool with
         | Some name -> name
         | None -> Mcp_tools.tool_name ~server ~tool)
    ; arguments = input
    ; native_name
    }
  | Todo ->
    (match
       Option.first_some (list_field input "todos") (list_field input "plan")
     with
     | Some steps ->
       { name = Plan.tool_name
       ; arguments =
           obj
             [ Some ("plan", `Array (List.filter_map steps ~f:plan_step))
             ; Option.map (string_field input "explanation") ~f:(fun e ->
                 "explanation", str e)
             ]
       ; native_name
       }
     | None -> keep ())
  | Subagent ->
    (match first_string input [ "prompt"; "task"; "description" ] with
     | Some task ->
       { name = Tool_subagent.name
       ; arguments =
           obj
             [ Some ("task", str task)
             ; Option.map (string_field input "description") ~f:(fun d ->
                 "name", str d)
             ]
       ; native_name
       }
     | None -> keep ())
  | Other _ -> keep ()
;;

let with_headers ~path diff =
  let diff =
    if
      List.exists
        (List.take_while (String.split_lines diff) ~f:(Fn.non is_hunk_header))
        ~f:(String.is_prefix ~prefix:"--- ")
    then diff
    else sprintf "--- a/%s\n+++ b/%s\n%s" path path diff
  in
  if String.is_suffix diff ~suffix:"\n" then diff else diff ^ "\n"
;;

let result_text t ~ok ~output ~(changes : Adapter_event.Change.t list) =
  let with_diff =
    List.filter changes ~f:(fun c -> not (String.is_empty c.diff))
  in
  match t.name, ok with
  | "edit", true ->
    (match with_diff with
     | [] -> output
     | changes ->
       String.concat
         (List.map changes ~f:(fun c -> with_headers ~path:c.path c.diff)))
  | "apply_patch", true when not (List.is_empty changes) ->
    let label (c : Adapter_event.Change.t) =
      match c.kind with
      | Add -> "A " ^ c.path
      | Delete -> "D " ^ c.path
      | Update { move_to } -> "M " ^ Option.value move_to ~default:c.path
    in
    let diffs =
      List.filter_map with_diff ~f:(fun c ->
        match c.kind with
        | Update { move_to } ->
          Some
            (with_headers ~path:(Option.value move_to ~default:c.path) c.diff)
        | Add | Delete -> None)
    in
    String.concat
      ~sep:"\n"
      ("Success. Updated the following files:" :: List.map changes ~f:label)
    ^ if List.is_empty diffs then "" else "\n\n" ^ String.concat diffs
  | _ -> output
;;

let of_plan (plan : Plan.t) =
  { name = Plan.tool_name
  ; arguments =
      obj
        [ Some
            ( "plan"
            , `Array
                (List.map plan.steps ~f:(fun (s : Plan.Step.t) ->
                   `Object
                     [ "step", str s.step
                     ; "status", str (Plan.Status.to_string s.status)
                     ])) )
        ; Option.map plan.explanation ~f:(fun e -> "explanation", str e)
        ]
  ; native_name = "plan"
  }
;;
