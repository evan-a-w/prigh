open! Core
open! Import
module File = Prigh_ui.Diff_files.File

module Why = struct
  type t =
    | Generated
    | Renamed_to of string
    | Renamed_from of string
  [@@deriving sexp_of, equal]

  let label = function
    | Generated -> "generated"
    | Renamed_to _ | Renamed_from _ -> "renamed"
  ;;
end

let lock_names =
  [ "package-lock.json"
  ; "npm-shrinkwrap.json"
  ; "pnpm-lock.yaml"
  ; "yarn.lock"
  ; "flake.lock"
  ; "Cargo.lock"
  ; "Gemfile.lock"
  ; "poetry.lock"
  ; "composer.lock"
  ; "go.sum"
  ; "bun.lockb"
  ; "Pipfile.lock"
  ; "uv.lock"
  ]
;;

let generated_dirs = [ "_build"; "node_modules"; "dist"; "vendor"; "_opam" ]

let generated path =
  let parts = String.split path ~on:'/' in
  let name = Option.value (List.last parts) ~default:path in
  List.mem lock_names name ~equal:String.equal
  || List.exists
       [ ".lock"; ".opam.locked"; ".min.js"; ".min.css"; ".js.map"; ".css.map" ]
       ~f:(fun suffix -> String.is_suffix name ~suffix)
  || List.exists (List.drop_last_exn parts) ~f:(fun dir ->
    List.mem generated_dirs dir ~equal:String.equal)
;;

(* A whole-file diff's text: its lines without the sign. *)
let text (f : File.t) =
  List.concat_map f.hunks ~f:String.split_lines
  |> List.filter_map ~f:(fun line ->
    if String.is_prefix line ~prefix:"@@" || String.is_prefix line ~prefix:"\\"
    then None
    else Some (String.drop_prefix line 1))
;;

let classify (files : File.t list) =
  let deleted =
    List.filter files ~f:(fun f ->
      File.Status.equal f.status Deleted && not (List.is_empty f.hunks))
  in
  let added =
    List.filter files ~f:(fun f ->
      File.Status.equal f.status Added && not (List.is_empty f.hunks))
  in
  let renames =
    if List.is_empty deleted || List.is_empty added
    then []
    else (
      let texts = List.map added ~f:(fun f -> f, text f) in
      List.fold deleted ~init:([], String.Set.empty) ~f:(fun (acc, used) d ->
        let gone = text d in
        match
          List.find texts ~f:(fun ((a : File.t), lines) ->
            (not (Set.mem used a.path))
            && a.added = d.removed
            && List.equal String.equal lines gone)
        with
        | Some (a, _) -> (d.path, a.path) :: acc, Set.add used a.path
        | None -> acc, used)
      |> fst)
  in
  let renamed =
    List.concat_map renames ~f:(fun (from, to_) ->
      [ from, Why.Renamed_to to_; to_, Why.Renamed_from from ])
  in
  let generated =
    List.filter_map files ~f:(fun f ->
      Option.some_if (generated f.path) (f.path, Why.Generated))
  in
  String.Map.of_alist_reduce (renamed @ generated) ~f:(fun a _ -> a)
;;
