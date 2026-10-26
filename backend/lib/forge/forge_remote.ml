open! Core

type t =
  { host : string
  ; owner : string
  ; name : string
  }
[@@deriving sexp_of, equal]

let of_path ~host path =
  let parts =
    String.split path ~on:'/' |> List.filter ~f:(Fn.non String.is_empty)
  in
  match List.rev parts with
  | name :: owner :: _ ->
    let name = String.chop_suffix_if_exists name ~suffix:".git" in
    if String.is_empty name || String.is_empty host
    then None
    else Some { host = String.lowercase host; owner; name }
  | _ -> None
;;

let without_user host =
  match String.rsplit2 host ~on:'@' with
  | Some (_, host) -> host
  | None -> host
;;

let parse url =
  let url = String.strip url in
  match String.substr_index url ~pattern:"://" with
  | Some i ->
    let scheme = String.prefix url i in
    let rest = String.drop_prefix url (i + 3) in
    let authority, path =
      match String.lsplit2 rest ~on:'/' with
      | Some (a, p) -> a, p
      | None -> rest, ""
    in
    let host = without_user authority in
    let host =
      match scheme with
      | "http" | "https" -> host
      | _ ->
        (match String.lsplit2 host ~on:':' with
         | Some (h, _) -> h
         | None -> host)
    in
    of_path ~host path
  | None ->
    (match String.lsplit2 url ~on:':' with
     | Some (authority, path) when not (String.mem authority '/') ->
       of_path ~host:(without_user authority) path
     | _ -> None)
;;
