open! Core
open! Import

module If_missing = struct
  type t =
    | Fail
    | Ask
    | Create
  [@@deriving sexp_of, equal]

  let of_param = function
    | None | Some "fail" -> Ok Fail
    | Some "ask" -> Ok Ask
    | Some "create" -> Ok Create
    | Some other ->
      Or_error.errorf "if_missing must be fail, ask or create, got %S" other
  ;;
end

module Outcome = struct
  type t =
    | Changed of
        { cwd : string
        ; created : bool
        }
    | Missing of string
  [@@deriving sexp_of, equal]

  let to_json = function
    | Changed { cwd; created } ->
      `Object
        [ "cwd", `String cwd; ("created", if created then `True else `False) ]
    | Missing path -> `Object [ "missing", `String path ]
  ;;
end

let op = "$make_dir"

let args ~path ~check =
  `Object [ "path", `String path; ("check", if check then `True else `False) ]
;;

(* "a/./b/../c" -> "a/c", without looking at the filesystem: the missing part
   cannot be resolved any other way. *)
let normalise path =
  let parts =
    List.fold (String.split path ~on:'/') ~init:[] ~f:(fun acc part ->
      match part, acc with
      | ("" | "."), _ -> acc
      | "..", _ :: rest -> rest
      | "..", [] -> []
      | part, _ -> part :: acc)
  in
  "/" ^ String.concat ~sep:"/" (List.rev parts)
;;

let rec nearest_existing path =
  match Sys_unix.file_exists path with
  | `Yes -> path
  | `No | `Unknown ->
    let parent = Filename.dirname path in
    if String.equal parent path then path else nearest_existing parent
;;

let unix_error path = function
  | Core_unix.Unix_error ((EACCES | EPERM | EROFS), _, _) ->
    sprintf
      "permission denied: cannot create %s; choose a directory you can write"
      path
  | Core_unix.Unix_error (err, _, _) ->
    sprintf "cannot create %s: %s" path (Core_unix.Error.message err)
  | exn -> sprintf "cannot create %s: %s" path (Exn.to_string exn)
;;

let run ~cwd ~check path =
  let path = normalise (Tool.resolve ~cwd path) in
  match Sys_unix.is_directory path with
  | `Yes -> Tool.Result.ok (Filename_unix.realpath path)
  | `No | `Unknown ->
    let existing = nearest_existing path in
    if String.equal existing path
    then
      Tool.Result.error
        (sprintf "%s is a file, not a directory: choose another path" path)
    else (
      match Sys_unix.is_directory existing with
      | `No | `Unknown ->
        Tool.Result.error
          (sprintf
             "%s is a file, so %s cannot be created: choose another path"
             existing
             path)
      | `Yes ->
        (match Core_unix.access existing [ `Write; `Exec ] with
         | Error _ ->
           Tool.Result.error
             (sprintf
                "permission denied: cannot create %s in %s; choose a directory \
                 you can write"
                path
                existing)
         | Ok () when check -> Tool.Result.ok path
         | Ok () ->
           (match Core_unix.mkdir_p path with
            | () -> Tool.Result.ok (Filename_unix.realpath path)
            | exception exn -> Tool.Result.error (unix_error path exn))))
;;

let older_host (result : Tool.Result.t) ~host ~path =
  if
    result.is_error
    && String.is_prefix result.text ~prefix:(sprintf "unknown host tool %S" op)
  then
    Some
      (sprintf
         "tool host %s runs an older prigh that cannot create directories: \
          update prigh there (and restart its TUI or tool-host), or create %s \
          yourself"
         host
         path)
  else None
;;
