open! Core
open! Import

let path json names =
  List.fold names ~init:(Some json) ~f:(fun json name ->
    Option.bind json ~f:(Json.member name))
;;

let str_opt json names =
  match path json names with
  | Some (`String s) -> Some s
  | _ -> None
;;

let str json names = Option.value (str_opt json names) ~default:""
let int json names = Option.bind (path json names) ~f:Json.int

let bool json names =
  match path json names with
  | Some `True -> true
  | _ -> false
;;

let strings json names ~field =
  match path json names with
  | Some (`Array items) ->
    List.filter_map items ~f:(fun i -> str_opt i [ field ])
  | _ -> []
;;

let number n = `Number (Int.to_string n)
let strings_json l = `Array (List.map l ~f:(fun s -> `String s))
