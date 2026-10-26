open! Core
open! Import

type t =
  { at : float
  ; by : string
  ; generations : Nix_generations.t option
  ; generations_deleted : int
  ; paths_deleted : int
  ; freed_bytes : int
  ; seconds : float
  }
[@@deriving sexp_of]

let int n = `Number (Int.to_string n)
let ms seconds = int (Float.iround_nearest_exn (seconds *. 1000.))

let to_json t =
  `Object
    [ "at_ms", ms t.at
    ; "by", `String t.by
    ; "generations", Nix_generations.to_json t.generations
    ; "generations_deleted", int t.generations_deleted
    ; "paths_deleted", int t.paths_deleted
    ; "freed_bytes", int t.freed_bytes
    ; "duration_ms", ms t.seconds
    ]
;;

let fields text =
  String.split_lines text
  |> List.filter_map ~f:(fun line ->
    match String.lsplit2 line ~on:'=' with
    | Some (key, value) when not (String.is_empty key) -> Some (key, value)
    | _ -> None)
;;

let of_fields text =
  let fields = fields text in
  let open Or_error.Let_syntax in
  let get key =
    match List.Assoc.find fields ~equal:String.equal key with
    | Some v -> Ok v
    | None -> Or_error.errorf "no %s" key
  in
  let int key =
    let%bind v = get key in
    match Int.of_string_opt v with
    | Some n when n >= 0 -> Ok n
    | _ -> Or_error.errorf "bad %s %S" key v
  in
  let float key =
    let%bind v = get key in
    match Float.of_string_opt v with
    | Some f -> Ok f
    | None -> Or_error.errorf "bad %s %S" key v
  in
  let%bind at = float "at" in
  let%bind by = get "by" in
  let%bind generations = get "generations" >>= Nix_generations.of_arg in
  let%bind generations_deleted = int "generations_deleted" in
  let%bind paths_deleted = int "paths_deleted" in
  let%bind freed_bytes = int "freed_bytes" in
  let%map seconds = float "seconds" in
  { at
  ; by
  ; generations
  ; generations_deleted
  ; paths_deleted
  ; freed_bytes
  ; seconds
  }
;;
