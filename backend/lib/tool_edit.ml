open! Core
open! Import

let spec =
  { Tool_spec.name = "edit"
  ; parallel_safe = false
  ; on_host = true
  ; destructive = true
  ; description =
      "Edit a file by exact text replacement. Each old_text must occur exactly \
       once in the file, and edits must not overlap. Use enough surrounding \
       context to make old_text unique. All edits are matched against the \
       original file and applied together; if any fails, nothing is changed."
  ; parameters =
      Tool_args.schema
        ~required:[ "path"; "edits" ]
        [ "path", `String, "Path to the file"
        ; ( "edits"
          , `Array
              (Tool_args.schema
                 ~required:[ "old_text"; "new_text" ]
                 [ ( "old_text"
                   , `String
                   , "Exact text to replace (must be unique in the file)" )
                 ; "new_text", `String, "Replacement text"
                 ])
          , "List of replacements" )
        ]
  }
;;

module Edit = struct
  type t =
    { old_text : string
    ; new_text : string
    }

  let of_json json =
    { old_text = Tool_args.string json "old_text"
    ; new_text = Tool_args.string json "new_text"
    }
  ;;
end

let occurrences content pattern =
  let rec go from acc =
    match String.substr_index content ~pos:from ~pattern with
    | None -> List.rev acc
    | Some i -> go (i + 1) (i :: acc)
  in
  go 0 []
;;

(* Returns [(start, end, new_text)] sorted by start, or an error message. *)
let locate content (edits : Edit.t list) =
  let located =
    List.mapi edits ~f:(fun i edit ->
      if String.is_empty edit.old_text
      then Error (sprintf "edit %d: old_text must not be empty" (i + 1))
      else (
        match occurrences content edit.old_text with
        | [ start ] ->
          Ok (start, start + String.length edit.old_text, edit.new_text)
        | [] -> Error (sprintf "edit %d: old_text not found in file" (i + 1))
        | matches ->
          Error
            (sprintf
               "edit %d: old_text occurs %d times; add context to make it \
                unique"
               (i + 1)
               (List.length matches))))
  in
  match Result.all located with
  | Error _ as e -> e
  | Ok located ->
    let sorted =
      List.sort located ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)
    in
    let rec check = function
      | (_, e1, _) :: ((s2, _, _) :: _ as rest) ->
        if e1 > s2 then Error "edits overlap" else check rest
      | _ -> Ok sorted
    in
    check sorted
;;

let apply content located =
  let buf = Buffer.create (String.length content) in
  let pos =
    List.fold located ~init:0 ~f:(fun pos (start, stop, new_text) ->
      Buffer.add_substring buf content ~pos ~len:(start - pos);
      Buffer.add_string buf new_text;
      stop)
  in
  Buffer.add_substring buf content ~pos ~len:(String.length content - pos);
  Buffer.contents buf
;;

let relative_path ~cwd path =
  if Filename.is_relative path
  then path
  else (
    match String.chop_prefix path ~prefix:(cwd ^ "/") with
    | Some rel -> rel
    | None -> path)
;;

let run (context : Tool.Context.t) args =
  let path = Tool.resolve_path context (Tool_args.string args "path") in
  let edits =
    match Tool_args.list_opt args "edits" with
    | None | Some [] ->
      raise (Tool_args.Invalid "edits must be a non-empty array")
    | Some edits -> List.map edits ~f:Edit.of_json
  in
  if not (Sys_unix.file_exists_exn path)
  then Tool.Result.error (sprintf "file not found: %s" path)
  else (
    let content = In_channel.read_all path in
    match locate content edits with
    | Error msg -> Tool.Result.error msg
    | Ok located ->
      let updated = apply content located in
      Out_channel.write_all path ~data:updated;
      Tool.Result.ok
        (Udiff.hunks
           ~path:(relative_path ~cwd:context.cwd path)
           ~before:content
           ~after:updated))
;;

let tool = { Tool.spec; run }
