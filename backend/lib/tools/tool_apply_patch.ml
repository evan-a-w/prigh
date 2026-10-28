open! Core
open! Import

let spec =
  { Tool_spec.name = "apply_patch"
  ; parallel_safe = false
  ; on_host = true
  ; freeform = Some Apply_patch.grammar
  ; destructive = Always
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

let read (context : Tool.Context.t) path =
  let path = Tool.resolve_path context path in
  match Sys_unix.is_file path with
  | `Yes -> Some (File_access.read context.files path)
  | `No | `Unknown -> None
;;

let write (context : Tool.Context.t) path data =
  let path = Tool.resolve_path context path in
  Core_unix.mkdir_p (Filename.dirname path);
  File_access.write context.files path ~data
;;

(* The file a path names however it is written: [a.txt], [./a.txt],
   [x/../a.txt] or through a symlink. *)
let file context path =
  let resolved = Tool.resolve_path context path in
  match Filename_unix.realpath resolved with
  | real -> real
  | exception _ ->
    (match Filename_unix.realpath (Filename.dirname resolved) with
     | dir -> Filename.concat dir (Filename.basename resolved)
     | exception _ -> resolved)
;;

let display (context : Tool.Context.t) path =
  Tool_edit.relative_path ~cwd:context.cwd (Tool.resolve_path context path)
;;

(* [*** Add File:] over an existing file, or a move onto one, replaces it:
   only when the agent has seen its current contents. *)
let check_overwrites (context : Tool.Context.t) seen operations =
  let retry = "send the patch again (or change it with *** Update File)" in
  let check path ~what =
    let resolved = Tool.resolve_path context path in
    match Sys_unix.is_file resolved with
    | `Yes ->
      Seen_files.check_overwrite
        seen
        ~path:resolved
        ~current:(File_access.read context.files resolved)
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
          Apply_patch.apply operations ~file:(file context) ~read:(read context)))
  with
  | Error e -> Tool.Result.error (Error.to_string_hum e)
  | Ok changes ->
    let remember path contents =
      Option.iter
        seen
        ~f:(Seen_files.record ~path:(Tool.resolve_path context path) ~contents)
    in
    let notes = ref [] in
    let write_change : Apply_patch.Change.t -> string * string option = function
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
          |> Option.filter ~f:(Fn.non String.is_empty) )
    in
    let label : Apply_patch.Change.t -> string = function
      | Added { path; _ } -> "A " ^ display context path
      | Deleted { path; _ } -> "D " ^ display context path
      | Updated { path; move_to; _ } ->
        "M " ^ display context (Option.value move_to ~default:path)
    in
    (* Every change was checked before anything is written, but a write can
       still fail (permissions, a full disk): then say exactly what was
       changed and what was not. *)
    let rec write_all done_ = function
      | [] -> Ok (List.rev done_)
      | change :: rest ->
        (match write_change change with
         | written -> write_all (written :: done_) rest
         | exception exn -> Error (List.rev done_, change, exn, rest))
    in
    (match write_all [] changes with
     | Error (done_, failed, exn, rest) ->
       let list = function
         | [] -> " none"
         | labels -> "\n" ^ String.concat ~sep:"\n" labels
       in
       Tool.Result.error
         (sprintf
            "The patch was only partly applied: writing failed at %s: %s\n\
             Changed:%s\n\
             Not changed:%s\n\
             Fix the cause, then patch only the files not changed (the changed \
             ones have their edits already)."
            (label failed)
            (match exn with
             | Core_unix.Unix_error (error, _, _) ->
               Core_unix.Error.message error
             | Sys_error message -> message
             | exn -> Exn.to_string exn)
            (list (List.map done_ ~f:fst))
            (list (List.map (failed :: rest) ~f:label)))
     | Ok written ->
       let summaries, diffs = List.unzip written in
       let diffs = List.filter_opt diffs in
       Tool.Result.ok
         (String.concat
            ~sep:"\n"
            ("Success. Updated the following files:" :: summaries)
          ^ (match List.rev !notes with
             | [] -> ""
             | notes -> "\n" ^ String.concat ~sep:"\n" notes)
          ^ if List.is_empty diffs then "" else "\n\n" ^ String.concat diffs))
;;

let tool = { Tool.spec; run }
