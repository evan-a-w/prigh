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

let run (context : Tool.Context.t) args =
  match
    Or_error.bind
      (Apply_patch.parse (Tool_args.string args "input"))
      ~f:(Apply_patch.apply ~read:(read context))
  with
  | Error e -> Tool.Result.error (Error.to_string_hum e)
  | Ok changes ->
    let summaries, diffs =
      List.map changes ~f:(function
        | Added { path; contents } ->
          write context path contents;
          "A " ^ display context path, None
        | Deleted { path; before = _ } ->
          Sys_unix.remove (Tool.resolve_path context path);
          "D " ^ display context path, None
        | Updated { path; move_to; before; after } ->
          let destination = Option.value move_to ~default:path in
          write context destination after;
          if
            Option.is_some move_to
            && not
                 (String.equal
                    (Tool.resolve_path context path)
                    (Tool.resolve_path context destination))
          then Sys_unix.remove (Tool.resolve_path context path);
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
       ^ if List.is_empty diffs then "" else "\n\n" ^ String.concat diffs)
;;

let tool = { Tool.spec; run }
