open! Core
open! Import

type t =
  { selecting : bool
  ; marked : String.Set.t
  ; anchor : string option
  }
[@@deriving sexp_of, equal]

let empty = { selecting = false; marked = String.Set.empty; anchor = None }
let start = { empty with selecting = true }

let index order path =
  List.findi order ~f:(fun _ p -> String.equal p path) |> Option.map ~f:fst
;;

let click t ~path ~range ~order =
  let between =
    if range
    then (
      match Option.bind t.anchor ~f:(index order), index order path with
      | Some a, Some b ->
        let lo = Int.min a b in
        Some (List.sub order ~pos:lo ~len:(Int.max a b - lo + 1))
      | _ -> None)
    else None
  in
  match between with
  | Some paths ->
    { t with
      selecting = true
    ; marked = Set.union t.marked (String.Set.of_list paths)
    }
  | None ->
    { selecting = true
    ; marked =
        (if Set.mem t.marked path
         then Set.remove t.marked path
         else Set.add t.marked path)
    ; anchor = Some path
    }
;;

let prune t ~paths =
  let paths = String.Set.of_list paths in
  { t with
    marked = Set.inter t.marked paths
  ; anchor = Option.filter t.anchor ~f:(Set.mem paths)
  }
;;

let marked t sessions =
  List.filter sessions ~f:(fun (s : Session_summary.t) -> Set.mem t.marked s.path)
;;
