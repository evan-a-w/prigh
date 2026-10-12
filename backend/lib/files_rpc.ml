open! Core
open! Import

let methods =
  [ "fs_list"
  ; "fs_preview"
  ; "fs_mkdir"
  ; "fs_rename"
  ; "fs_delete"
  ; "fs_download"
  ; "fs_upload"
  ]
;;

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string params name =
  match param params name with
  | Some (`String s) when not (String.is_empty s) -> Ok s
  | Some (`String _) -> Or_error.errorf "param %S is empty" name
  | Some _ -> Or_error.errorf "param %S must be a string" name
  | None -> Or_error.errorf "missing param %S" name
;;

let string_opt params name =
  match param params name with
  | None | Some `Null -> Ok None
  | Some _ -> Or_error.map (string params name) ~f:Option.some
;;

let bool params name =
  match param params name with
  | None | Some `Null | Some `False -> Ok false
  | Some `True -> Ok true
  | Some _ -> Or_error.errorf "param %S must be a boolean" name
;;

let int params name =
  match param params name with
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some n when n >= 0 -> Ok n
     | _ -> Or_error.errorf "param %S must be a size in bytes" name)
  | _ -> Or_error.errorf "missing param %S" name
;;

let with_path agent op params ~key =
  Or_error.bind (string params key) ~f:(fun path ->
    File_ops.call agent op [ "path", `String path ])
;;

let ticket agent kind =
  `String (File_tickets.url kind (File_tickets.issue agent kind))
;;

let download agent params =
  let open Or_error.Let_syntax in
  let%bind path = string params "path" in
  let%bind archive = bool params "archive" in
  let%bind stat =
    File_ops.call agent Host_files.stat_op [ "path", `String path ]
  in
  let path =
    match Json.member "path" stat with
    | Some (`String path) -> path
    | _ -> path
  in
  let%bind () =
    match Json.member "kind" stat, archive with
    | Some (`String "dir"), false ->
      Or_error.errorf "%s is a directory: download it as an archive" path
    | Some (`String "other"), false ->
      Or_error.errorf
        "%s is not a regular file: read it with a command instead"
        path
    | Some (`String "dir"), true | _, false -> Ok ()
    | _, true -> Or_error.errorf "not a directory: %s" path
  in
  let kind : File_tickets.Kind.t =
    if archive then Archive { path } else Download { path }
  in
  return
    (`Object
        [ "url", ticket agent kind
        ; ( "name"
          , `String (Filename.basename path ^ if archive then ".tar.gz" else "")
          )
        ])
;;

let upload agent params =
  let open Or_error.Let_syntax in
  let%bind name = string params "name" in
  let%bind () = Host_files.check_name name in
  let%bind size = int params "size" in
  let%bind dir = string_opt params "dir" in
  let%bind overwrite = bool params "overwrite" in
  let%bind () =
    if size > File_ops.max_upload
    then
      Or_error.errorf
        "%s is too large to upload (%d bytes; at most %d): copy it to the host \
         another way (scp, git)"
        name
        size
        File_ops.max_upload
    else Ok ()
  in
  let%bind kind =
    match dir with
    | None ->
      let cwd = (Agent.state agent).cwd in
      Ok
        (File_tickets.Kind.Upload
           { path =
               Filename.concat (Filename.concat cwd File_ops.uploads_dir) name
           ; conflict = Rename
           ; size
           ; make_dir = true
           })
    | Some dir ->
      let path = Filename.concat dir name in
      let%map () =
        if overwrite
        then Ok ()
        else (
          match
            File_ops.call agent Host_files.stat_op [ "path", `String path ]
          with
          | Ok _ ->
            Or_error.errorf
              "%s already exists in %s: overwrite it or rename the upload"
              name
              dir
          | Error _ -> Ok ())
      in
      File_tickets.Kind.Upload
        { path
        ; conflict = (if overwrite then Overwrite else Fail)
        ; size
        ; make_dir = false
        }
  in
  let path =
    match kind with
    | Upload { path; _ } -> path
    | Download _ | Inline _ | Archive _ -> ""
  in
  return (`Object [ "url", ticket agent kind; "path", `String path ])
;;

let dispatch agent ~meth ~params =
  let call op fields = File_ops.call agent op fields in
  match meth with
  | "fs_list" ->
    Some
      (let open Or_error.Let_syntax in
       let%bind path = string_opt params "path" in
       let%bind hidden = bool params "hidden" in
       call
         Host_files.list_op
         [ "path", `String (Option.value path ~default:(Agent.state agent).cwd)
         ; ("hidden", if hidden then `True else `False)
         ])
  | "fs_preview" ->
    Some
      (Or_error.bind (string params "path") ~f:(fun path ->
         File_ops.preview agent ~path ~image_url:(fun ~path ~mime ->
           match ticket agent (Inline { path; mime }) with
           | `String url -> url
           | _ -> "")))
  | "fs_mkdir" -> Some (with_path agent Host_files.mkdir_op params ~key:"path")
  | "fs_delete" ->
    Some (with_path agent Host_files.delete_op params ~key:"path")
  | "fs_rename" ->
    Some
      (let open Or_error.Let_syntax in
       let%bind from = string params "from" in
       let%bind to_ = string params "to" in
       call Host_files.rename_op [ "from", `String from; "to", `String to_ ])
  | "fs_download" -> Some (download agent params)
  | "fs_upload" -> Some (upload agent params)
  | _ -> None
;;
