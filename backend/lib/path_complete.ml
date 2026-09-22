open! Core
open! Import

let max_results = 200
let max_depth = 4
let skips = [ "_build"; ".git"; "node_modules"; "_opam" ]
let skipped name = List.mem skips name ~equal:String.equal

let matches ~prefix path =
  String.is_empty prefix
  || String.is_substring
       (String.lowercase path)
       ~substring:(String.lowercase prefix)
;;

let is_directory path =
  match Sys_unix.is_directory path with
  | `Yes -> true
  | `No | `Unknown -> false
;;

let normalize ~cwd path =
  let path = String.rstrip path ~drop:(Char.equal '/') in
  if String.is_empty path
  then path
  else if is_directory (Filename.concat cwd path)
  then path ^ "/"
  else path
;;

let finish ~cwd ~prefix paths =
  paths
  |> List.map ~f:(normalize ~cwd)
  |> List.filter ~f:(matches ~prefix)
  |> List.dedup_and_sort ~compare:String.compare
  |> fun paths -> List.take paths max_results
;;

let rec walk ~cwd ~dir ~depth acc =
  if depth > max_depth
  then acc
  else (
    let abs = if String.is_empty dir then cwd else Filename.concat cwd dir in
    match Sys_unix.readdir abs with
    | exception _ -> acc
    | entries ->
      Array.fold entries ~init:acc ~f:(fun acc name ->
        if skipped name
        then acc
        else (
          let rel =
            if String.is_empty dir then name else Filename.concat dir name
          in
          if is_directory (Filename.concat cwd rel)
          then walk ~cwd ~dir:rel ~depth:(depth + 1) ((rel ^ "/") :: acc)
          else rel :: acc)))
;;

let readdir ~cwd ~prefix = finish ~cwd ~prefix (walk ~cwd ~dir:"" ~depth:1 [])

let with_fd ~env ~cwd ~prefix =
  match
    Process.run_collect
      ~env
      ~cwd
      ~timeout:(Time_ns.Span.of_sec 5.)
      ~prog:"fd"
      ~args:
        [ "--type"
        ; "f"
        ; "--type"
        ; "d"
        ; "--max-depth"
        ; Int.to_string max_depth
        ; "--hidden"
        ]
      ()
  with
  | exception _ -> None
  | { exit; stdout; stderr = _ } ->
    if Process.Exit.is_success exit
    then
      Some
        (finish
           ~cwd
           ~prefix
           (List.filter (String.split_lines stdout) ~f:(Fn.non String.is_empty)))
    else None
;;

let list ?(use_fd = true) ~env ~cwd ~prefix () =
  match if use_fd then with_fd ~env ~cwd ~prefix else None with
  | Some paths -> paths
  | None -> readdir ~cwd ~prefix
;;
