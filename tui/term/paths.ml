open! Core
open! Async
module P = Prigh_protocol

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
  let paths =
    paths
    |> List.map ~f:(normalize ~root)
    |> List.filter ~f:(matches ~prefix)
    |> List.dedup_and_sort ~compare:String.compare
    |> fun paths -> List.take paths max_results
  in
  `Array (List.map paths ~f:(fun p -> `String p))
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

let fd ~root ~prefix =
  let%bind.Deferred result =
    Process.run
      ~working_dir:root
      ~prog:"fd"
      ~args:
        ([ "--type"; "f"; "--type"; "d"; "--max-depth"; "4"; "--hidden" ]
         @ List.concat_map skips ~f:(fun name -> [ "--exclude"; name ]))
      ()
  in
  match result with
  | Error _ -> Deferred.return (readdir ~root ~prefix)
  | Ok output ->
    let paths =
      List.filter (String.split_lines output) ~f:(Fn.non String.is_empty)
    in
    Deferred.return (finish ~root ~prefix paths)
;;

let list ~cwd ~prefix =
  let root = Option.value cwd ~default:(Core_unix.getcwd ()) in
  fd ~root ~prefix
;;
