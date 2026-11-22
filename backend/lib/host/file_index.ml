open! Core
open! Import

type t =
  { files : string list
  ; truncated : bool
  }
[@@deriving sexp_of]

let limit = 1_000_000
let skips = [ "_build"; "_opam"; "node_modules" ]

let skipped name =
  String.is_prefix name ~prefix:"." || List.mem skips name ~equal:String.equal
;;

let walk ?(limit = limit) ~root () =
  let count = ref 0 in
  let truncated = ref false in
  let acc = ref [] in
  let rec go dir =
    if not !truncated
    then (
      let entries =
        match
          Sys_unix.readdir
            (if String.is_empty dir then root else Filename.concat root dir)
        with
        | entries -> entries
        | exception _ -> [||]
      in
      Array.sort entries ~compare:String.compare;
      Array.iter entries ~f:(fun name ->
        if (not !truncated) && not (skipped name)
        then (
          let rel = if String.is_empty dir then name else dir ^ "/" ^ name in
          match Core_unix.lstat (Filename.concat root rel) with
          | { st_kind = S_DIR; _ } -> go rel
          | { st_kind = S_REG; _ } ->
            if !count >= limit
            then truncated := true
            else (
              incr count;
              acc := rel :: !acc)
          | _ | (exception _) -> ())))
  in
  go "";
  { files = List.sort !acc ~compare:String.compare; truncated = !truncated }
;;

let git ~env ~root args =
  match
    Process.run_collect
      ~env
      ~cwd:root
      ~timeout:(Time_ns.Span.of_sec 20.)
      ~prog:"git"
      ~args:("-c" :: "core.quotePath=false" :: args)
      ()
  with
  | exception _ -> None
  | { exit; stdout; _ } ->
    if Process.Exit.is_success exit
    then
      Some
        (String.split stdout ~on:'\000' |> List.filter ~f:(Fn.non String.is_empty))
    else None
;;

let list ?(limit = limit) ~env ~root () =
  match
    git ~env ~root [ "ls-files"; "-z"; "--cached"; "--others"; "--exclude-standard" ]
  with
  | None -> walk ~limit ~root ()
  | Some files ->
    let deleted =
      Option.value ~default:[] (git ~env ~root [ "ls-files"; "-z"; "--deleted" ])
      |> String.Set.of_list
    in
    let files =
      List.filter files ~f:(fun f -> not (Set.mem deleted f))
      |> List.dedup_and_sort ~compare:String.compare
    in
    let truncated = List.length files > limit in
    { files = (if truncated then List.take files limit else files); truncated }
;;

let to_json ~root t =
  `Object
    [ "root", `String root
    ; "files", `String (String.concat ~sep:"\n" t.files)
    ; "truncated", (if t.truncated then `True else `False)
    ]
;;
