open! Core
open Prigh_protocol

type t =
  | Found of Model.t
  | Ambiguous of Model.t list
  | Not_found of Model.t list
[@@deriving sexp_of]

let names (m : Model.t) = [ m.key; m.id; m.name ]

let resolve (models : Model.t list) query =
  let q = String.lowercase (String.strip query) in
  let eq s = String.equal (String.lowercase s) q in
  match List.filter models ~f:(fun m -> eq m.key) with
  | m :: _ -> Found m
  | [] ->
    (match List.filter models ~f:(fun m -> List.exists (names m) ~f:eq) with
     | [ m ] -> Found m
     | _ :: _ as many -> Ambiguous many
     | [] ->
       let prefix =
         List.filter models ~f:(fun m ->
           List.exists (names m) ~f:(fun n ->
             String.is_prefix (String.lowercase n) ~prefix:q))
       in
       (match prefix with
        | [ m ] -> Found m
        | _ :: _ as many -> Ambiguous many
        | [] ->
          (match
             Fuzzy.rank ~query:q models ~key:(fun m -> m.name ^ " " ^ m.key)
           with
           | [ m ] -> Found m
           | _ :: _ as many -> Ambiguous many
           | [] ->
             let scored =
               List.map models ~f:(fun m ->
                 let d =
                   List.min_elt
                     (List.map (names m) ~f:(fun n ->
                        Edit_distance.distance n q))
                     ~compare:Int.compare
                   |> Option.value ~default:Int.max_value
                 in
                 d, m)
             in
             Not_found
               (List.stable_sort scored ~compare:(fun (a, _) (b, _) ->
                  Int.compare a b)
                |> List.map ~f:snd
                |> fun l -> List.take l 3))))
;;
