open! Core
open! Import

type t =
  { id : string
  ; leftovers : Leftover.t list
  }
[@@deriving sexp_of, equal, jsonaf]

let dir ~sessions_dir = Filename.concat sessions_dir ".temporary"
let path ~dir ~id = Filename.concat dir (id ^ ".json")

let write ~dir t =
  Core_unix.mkdir_p dir ~perm:0o700;
  let file = path ~dir ~id:t.id in
  let tmp = file ^ ".tmp" in
  Out_channel.write_all tmp ~data:(Json.to_string (jsonaf_of_t t));
  Core_unix.rename ~src:tmp ~dst:file
;;

let remove ~dir ~id =
  try Sys_unix.remove (path ~dir ~id) with
  | _ -> ()
;;

let load_all ~dir =
  match Sys_unix.readdir dir with
  | exception _ -> []
  | names ->
    Array.to_list names
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun name ->
      let file = Filename.concat dir name in
      if not (String.is_suffix name ~suffix:".json")
      then (
        (try Sys_unix.remove file with
         | _ -> ());
        None)
      else (
        match t_of_jsonaf (Json.of_string (In_channel.read_all file)) with
        | t -> Some t
        | exception _ ->
          (try Sys_unix.remove file with
           | _ -> ());
          None))
;;
