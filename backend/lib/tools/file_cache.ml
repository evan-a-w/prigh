open! Core
open! Import

module Stamp = struct
  type t =
    { size : Int64.t
    ; mtime : float
    }
  [@@deriving equal]

  let of_path path =
    match Core_unix.stat path with
    | { st_size = size; st_mtime = mtime; _ } -> Some { size; mtime }
    | exception _ -> None
  ;;
end

type 'a entry =
  { stamp : Stamp.t
  ; data : 'a
  ; mutable used : int
  }

type 'a t =
  { entries : 'a entry String.Table.t
  ; max_bytes : int
  ; mutable clock : int
  }

let create ~max_bytes () =
  { entries = String.Table.create (); max_bytes; clock = 0 }
;;

let touch t entry =
  t.clock <- t.clock + 1;
  entry.used <- t.clock
;;

let remove t path = Hashtbl.remove t.entries path

let find_stamped t path ~stamp =
  match Hashtbl.find t.entries path, stamp with
  | Some entry, Some stamp when Stamp.equal entry.stamp stamp ->
    touch t entry;
    Some entry.data
  | Some _, _ ->
    remove t path;
    None
  | None, _ -> None
;;

let find t path = find_stamped t path ~stamp:(Stamp.of_path path)

let paths t =
  Hashtbl.to_alist t.entries
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare a.used b.used)
  |> List.map ~f:fst
;;

let bytes t =
  Hashtbl.fold t.entries ~init:0 ~f:(fun ~key:_ ~data acc ->
    acc + Int64.to_int_exn data.stamp.size)
;;

let rec shrink t =
  if bytes t > t.max_bytes && Hashtbl.length t.entries > 1
  then (
    match paths t with
    | oldest :: _ ->
      remove t oldest;
      shrink t
    | [] -> ())
;;

let add t path ~stamp data =
  let entry = { stamp; data; used = 0 } in
  touch t entry;
  Hashtbl.set t.entries ~key:path ~data:entry;
  shrink t
;;

let set t path data =
  match Stamp.of_path path with
  | Some stamp -> add t path ~stamp data
  | None -> remove t path
;;

let find_or_read t path ~read =
  let stamp = Stamp.of_path path in
  match find_stamped t path ~stamp with
  | Some data -> Ok data
  | None ->
    let result = read path in
    (match result, stamp with
     | Ok data, Some stamp -> add t path ~stamp data
     | Ok _, None | Error _, _ -> ());
    result
;;
