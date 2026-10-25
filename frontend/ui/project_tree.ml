open! Core
open! Import

module Worktree_node = struct
  type t =
    { path : string
    ; branch : string option
    ; sessions : P.Session_summary.t list
    ; agent : (P.Agent_worktree.t * P.Session_summary.t) option
    ; listed : P.Worktree.t option
    }
  [@@deriving sexp_of, equal]

  let label t =
    match t.branch with
    | Some branch -> branch
    | None -> Filename.basename t.path
  ;;
end

module Project = struct
  type t =
    { root : string
    ; git : bool
    ; branch : string option
    ; sessions : P.Session_summary.t list
    ; worktrees : Worktree_node.t list
    ; listed : P.Worktree.t option
    }
  [@@deriving sexp_of, equal]

  let name t =
    match Filename.basename t.root with
    | "" | "/" | "." -> t.root
    | name -> String.chop_suffix_if_exists name ~suffix:".git"
  ;;

  let all_sessions t =
    t.sessions
    @ List.concat_map t.worktrees ~f:(fun (w : Worktree_node.t) -> w.sessions)
  ;;
end

type t = Project.t list [@@deriving sexp_of, equal]

let place (s : P.Session_summary.t) =
  match s.repo with
  | Some r -> r.root, if r.main then None else Some r.worktree
  | None -> s.cwd, None
;;

(* Insertion-ordered tables: the tree's order is that of first mention. *)
module Ordered = struct
  type 'a t =
    { table : 'a String.Table.t
    ; mutable keys : string list (** newest first *)
    }

  let create () = { table = String.Table.create (); keys = [] }

  let find_or_add t key ~default =
    match Hashtbl.find t.table key with
    | Some v -> v
    | None ->
      let v = default () in
      Hashtbl.set t.table ~key ~data:v;
      t.keys <- key :: t.keys;
      v
  ;;

  let to_list t = List.rev_map t.keys ~f:(Hashtbl.find_exn t.table)
end

module Node_builder = struct
  type t =
    { path : string
    ; mutable branch : string option
    ; mutable sessions : P.Session_summary.t list (** newest first *)
    ; mutable agent : (P.Agent_worktree.t * P.Session_summary.t) option
    ; mutable listed : P.Worktree.t option
    }

  let create path =
    { path; branch = None; sessions = []; agent = None; listed = None }
  ;;

  let finish (b : t) =
    { Worktree_node.path = b.path
    ; branch = b.branch
    ; sessions = List.rev b.sessions
    ; agent = b.agent
    ; listed = b.listed
    }
  ;;
end

module Project_builder = struct
  type t =
    { root : string
    ; mutable git : bool
    ; mutable branch : string option
    ; mutable sessions : P.Session_summary.t list (** newest first *)
    ; worktrees : Node_builder.t Ordered.t
    ; mutable listed : P.Worktree.t option
    }

  let create root =
    { root
    ; git = false
    ; branch = None
    ; sessions = []
    ; worktrees = Ordered.create ()
    ; listed = None
    }
  ;;

  let node t path =
    Ordered.find_or_add t.worktrees path ~default:(fun () ->
      Node_builder.create path)
  ;;

  let finish (b : t) =
    { Project.root = b.root
    ; git = b.git
    ; branch = b.branch
    ; sessions = List.rev b.sessions
    ; worktrees = List.map (Ordered.to_list b.worktrees) ~f:Node_builder.finish
    ; listed = b.listed
    }
  ;;
end

let build ?listing ?(owners = []) sessions =
  let projects = Ordered.create () in
  let project root =
    Ordered.find_or_add projects root ~default:(fun () ->
      Project_builder.create root)
  in
  let agent_worktrees (owner : P.Session_summary.t) =
    List.iter owner.agent_worktrees ~f:(fun (w : P.Agent_worktree.t) ->
      let p = project w.root in
      p.git <- true;
      let node = Project_builder.node p w.path in
      if Option.is_none node.branch then node.branch <- Some w.branch;
      if Option.is_none node.agent then node.agent <- Some (w, owner))
  in
  List.iter sessions ~f:(fun (s : P.Session_summary.t) ->
    let root, worktree = place s in
    let p = project root in
    if Option.is_some s.repo then p.git <- true;
    let branch = Option.bind s.repo ~f:(fun r -> r.branch) in
    (match worktree with
     | None ->
       p.sessions <- s :: p.sessions;
       if Option.is_none p.branch then p.branch <- branch
     | Some path ->
       let node = Project_builder.node p path in
       node.sessions <- s :: node.sessions;
       if Option.is_none node.branch then node.branch <- branch);
    agent_worktrees s);
  List.iter owners ~f:agent_worktrees;
  let listed_first =
    match listing with
    | None -> false
    | Some (l : P.Worktree.Listing.t) ->
      let fresh = not (Hashtbl.mem projects.table l.root) in
      let p = project l.root in
      p.git <- true;
      List.iter l.worktrees ~f:(fun (w : P.Worktree.t) ->
        if w.main
        then (
          p.listed <- Some w;
          Option.iter w.branch ~f:(fun b -> p.branch <- Some b))
        else (
          let node = Project_builder.node p w.path in
          node.listed <- Some w;
          Option.iter w.branch ~f:(fun b -> node.branch <- Some b)));
      fresh
  in
  let all = List.map (Ordered.to_list projects) ~f:Project_builder.finish in
  match listing, listed_first with
  | Some l, true ->
    (* The current session's project, which none of the others is in. *)
    let mine, others =
      List.partition_tf all ~f:(fun (p : Project.t) ->
        String.equal p.root l.root)
    in
    mine @ others
  | _ -> all
;;

let sessions ?(collapsed = String.Set.empty) t =
  List.concat_map t ~f:(fun (p : Project.t) ->
    if Set.mem collapsed p.root
    then []
    else
      p.sessions
      @ List.concat_map p.worktrees ~f:(fun (w : Worktree_node.t) ->
        if Set.mem collapsed w.path then [] else w.sessions))
;;

let find_project t root =
  List.find t ~f:(fun (p : Project.t) -> String.equal p.root root)
;;

let find_worktree t path =
  List.find_map t ~f:(fun (p : Project.t) ->
    List.find p.worktrees ~f:(fun (w : Worktree_node.t) ->
      String.equal w.path path)
    |> Option.map ~f:(fun w -> p, w))
;;

let place_label t (s : P.Session_summary.t) =
  let root, worktree = place s in
  let name =
    match find_project t root with
    | Some p -> Project.name p
    | None -> Filename.basename root
  in
  match worktree with
  | None -> name
  | Some path ->
    let branch =
      match find_worktree t path with
      | Some (_, w) -> Worktree_node.label w
      | None -> Filename.basename path
    in
    sprintf "%s ⎇ %s" name branch
;;
