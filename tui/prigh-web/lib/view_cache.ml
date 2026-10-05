open! Core
open! Import
module Weak_map = Jsoo_weak_collections.Weak_map

type ('key, 'deps) t = ('key, 'deps * Node.t) Weak_map.t

let create () = Weak_map.create ()

let find t key ~deps ~equal ~f =
  match Weak_map.get t key with
  | Some (deps', node) when equal deps deps' -> node
  | _ ->
    let node = Node.lazy_ (lazy (f ())) in
    Weak_map.set t key (deps, node);
    node
;;
