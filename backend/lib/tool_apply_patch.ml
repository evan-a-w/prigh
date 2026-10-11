open! Core
open! Import

let spec =
  { Tool_spec.name = "apply_patch"
  ; parallel_safe = false
  ; on_host = true
  ; freeform = Some Apply_patch.grammar
  ; destructive = true
  ; description = Apply_patch.description
  ; parameters =
      Tool_args.schema
        ~required:[ "input" ]
        [ ( "input"
          , `String
          , "The entire patch, from *** Begin Patch to *** End Patch" )
        ]
  }
;;

let read context path =
  let path = Tool.resolve_path context path in
  match Sys_unix.is_file path with
  | `Yes -> Some (In_channel.read_all path)
  | `No | `Unknown -> None
;;

let write context path data =
  let path = Tool.resolve_path context path in
  Core_unix.mkdir_p (Filename.dirname path);
  Out_channel.write_all path ~data
;;

let display (context : Tool.Context.t) path =
  Tool_edit.relative_path ~cwd:context.cwd (Tool.resolve_path context path)
;;

(* [*** Add File:] over an existing file, or a move onto one, replaces it:
   only when the agent has seen its current contents. *)
let check_overwrites context seen operations =
  let retry = "send the patch again (or change it with *** Update File)" in
  let check path ~what =
    let resolved = Tool.resolve_path context path in
    match Sys_unix.is_file resolved with
    | `Yes ->
      Seen_files.check_overwrite
        seen
        ~path:resolved
        ~current:(In_channel.read_all resolved)
        ~what:(what resolved)
        ~retry
    | `No | `Unknown -> Ok ()
  in
  List.map operations ~f:(fun (operation : Apply_patch.Operation.t) ->
    match operation with
    | Add { path; contents = _ } ->
      check path ~what:(sprintf "overwrite %s with *** Add File")
    | Update { path; move_to = Some destination; chunks = _ }
      when not
             (String.equal
                (Tool.resolve_path context path)
                (Tool.resolve_path context destination)) ->
      check destination ~what:(sprintf "move %s onto %s" path)
    | Update _ | Delete _ -> Ok ())
  |> List.filter_map ~f:(function
    | Ok () -> None
    | Error message -> Some message)
  |> function
  | [] -> Ok ()
  | messages -> Error (Error.of_string (String.concat ~sep:"\n" messages))
;;

let run (context : Tool.Context.t) args =
  let seen = Seen_files.of_args args in
  match
    Or_error.bind
      (Apply_patch.parse (Tool_args.string args "input"))
      ~f:(fun operations ->
        Or_error.bind (check_overwrites context seen operations) ~f:(fun () ->
          Apply_patch.apply operations ~read:(read context)))
  with
  | Error e -> Tool.Result.error (Error.to_string_hum e)
  | Ok changes ->
    let remember path contents =
      Option.iter
        seen
        ~f:(Seen_files.record ~path:(Tool.resolve_path context path) ~contents)
    in
    let notes = ref [] in
    let summaries, diffs =
      List.map changes ~f:(function
        | Added { path; contents } ->
          write context path contents;
          remember path contents;
          "A " ^ display context path, None
        | Deleted { path; before = _ } ->
          Sys_unix.remove (Tool.resolve_path context path);
          Option.iter
            seen
            ~f:(Seen_files.forget ~path:(Tool.resolve_path context path));
          "D " ^ display context path, None
        | Updated { path; move_to; before; after } ->
          let destination = Option.value move_to ~default:path in
          Option.iter
            (Seen_files.changed_note
               seen
               ~path:(Tool.resolve_path context path)
               ~current:before)
            ~f:(fun note -> notes := note :: !notes);
          write context destination after;
          remember destination after;
          if
            Option.is_some move_to
            && not
                 (String.equal
                    (Tool.resolve_path context path)
                    (Tool.resolve_path context destination))
          then (
            Sys_unix.remove (Tool.resolve_path context path);
            Option.iter
              seen
              ~f:(Seen_files.forget ~path:(Tool.resolve_path context path)));
          let shown = display context destination in
          ( "M " ^ shown
          , Some (Udiff.hunks ~path:shown ~before ~after)
            |> Option.filter ~f:(Fn.non String.is_empty) ))
      |> List.unzip
    in
    let diffs = List.filter_opt diffs in
    Tool.Result.ok
      (String.concat
         ~sep:"\n"
         ("Success. Updated the following files:" :: summaries)
       ^ (match List.rev !notes with
          | [] -> ""
          | notes -> "\n" ^ String.concat ~sep:"\n" notes)
       ^ if List.is_empty diffs then "" else "\n\n" ^ String.concat diffs)
;;

let tool = { Tool.spec; run }
