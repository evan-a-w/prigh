open! Core
open! Import

module Place = struct
  type t =
    { cwd : string
    ; host : Session.Host.t option
    ; repo : Git_location.t option option
    }
  [@@deriving sexp_of]

  let home t =
    { Session.Home.cwd = t.cwd; host = t.host; repo = Option.join t.repo }
  ;;

  let of_home (h : Session.Home.t) =
    { cwd = h.cwd; host = h.host; repo = Option.map h.repo ~f:Option.some }
  ;;
end

module Change = struct
  type t =
    | Keep
    | Set of Session.Home.t
    | Clear
  [@@deriving sexp_of, equal]
end

let host_id (p : Place.t) =
  Option.map p.host ~f:(fun (h : Session.Host.t) -> h.id)
;;

let same_host a b = Option.equal String.equal (host_id a) (host_id b)

let worktree_top (p : Place.t) =
  match p.repo with
  | Some (Some l) -> Some l.worktree
  | Some None | None -> None
;;

(* Without git's answer for one of them, the other's checkout containing it;
   for neither, one cwd containing the other. *)
let same_worktree (a : Place.t) (b : Place.t) =
  same_host a b
  &&
  match worktree_top a, worktree_top b with
  | Some x, Some y -> String.equal x y
  | Some top, None -> Session_worktree.contains ~path:top ~dir:b.cwd
  | None, Some top -> Session_worktree.contains ~path:top ~dir:a.cwd
  | None, None ->
    Session_worktree.contains ~path:a.cwd ~dir:b.cwd
    || Session_worktree.contains ~path:b.cwd ~dir:a.cwd
;;

let switch home ~(from : Place.t) ~(to_ : Place.t) : Change.t =
  match home with
  | None -> if same_worktree from to_ then Keep else Set (Place.home from)
  | Some home -> if same_worktree (Place.of_home home) to_ then Clear else Keep
;;

let root (p : Place.t) =
  match p.repo with
  | Some (Some l) -> Some l.root
  | Some None | None -> None
;;

let cd home ~(from : Place.t) ~(to_ : Place.t) : Change.t =
  match home with
  | None -> Keep
  | Some (home : Session.Home.t) ->
    let place = Place.of_home home in
    if (not (same_host place to_)) || String.equal home.cwd to_.cwd
    then Clear
    else if same_worktree from to_
    then Keep
    else (
      match root to_ with
      | Some to_root ->
        let roots = List.filter_opt [ root place; root from ] in
        if List.mem roots to_root ~equal:String.equal then Keep else Clear
      | None -> Clear)
;;
