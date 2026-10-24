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

let normalize ~root path =
  let path = String.rstrip path ~drop:(Char.equal '/') in
  if String.is_empty path
  then path
  else (
    match Sys_unix.is_directory (Filename.concat root path) with
    | `Yes -> path ^ "/"
    | `No | `Unknown -> path)
;;

let finish ~root ~prefix paths =
  paths
  |> List.map ~f:(normalize ~root)
  |> List.filter ~f:(matches ~prefix)
  |> List.dedup_and_sort ~compare:String.compare
  |> fun paths -> List.take paths max_results
;;

let rec walk ~root ~dir ~depth acc =
  if depth > max_depth
  then acc
  else (
    match
      Sys_unix.readdir
        (Filename.concat root (if String.is_empty dir then "." else dir))
    with
    | exception _ -> acc
    | entries ->
      Array.fold entries ~init:acc ~f:(fun acc name ->
        if skipped name
        then acc
        else (
          let rel =
            if String.is_empty dir then name else Filename.concat dir name
          in
          match Sys_unix.is_directory (Filename.concat root rel) with
          | `Yes -> walk ~root ~dir:rel ~depth:(depth + 1) ((rel ^ "/") :: acc)
          | `No | `Unknown -> rel :: acc)))
;;

let readdir ~root ~prefix =
  finish ~root ~prefix (walk ~root ~dir:"" ~depth:1 [])
;;

let fd ~env ~root ~prefix =
  match
    Process.run_collect
      ~env
      ~cwd:root
      ~timeout:(Time_ns.Span.of_sec 5.)
      ~prog:"fd"
      ~args:
        ([ "--type"
         ; "f"
         ; "--type"
         ; "d"
         ; "--max-depth"
         ; Int.to_string max_depth
         ; "--hidden"
         ]
         @ List.concat_map skips ~f:(fun name -> [ "--exclude"; name ]))
      ()
  with
  | exception _ -> None
  | { exit; stdout; _ } ->
    if Process.Exit.is_success exit
    then
      Some
        (finish
           ~root
           ~prefix
           (List.filter (String.split_lines stdout) ~f:(Fn.non String.is_empty)))
    else None
;;

let list ~env ~root ~prefix =
  match fd ~env ~root ~prefix with
  | Some paths -> paths
  | None -> readdir ~root ~prefix
;;
