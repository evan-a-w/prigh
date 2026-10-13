open! Core
open! Import
module Weak_map = Jsoo_weak_collections.Weak_map

type ('key, 'deps) t = ('key, 'deps * Obj.t option * Node.t) Weak_map.t

let create () = Weak_map.create ()

let find ?inject t key ~deps ~equal ~f =
  let inject = Option.map inject ~f:Obj.repr in
  match Weak_map.get t key with
  | Some (deps', inject', node)
    when equal deps deps' && Option.equal phys_equal inject inject' -> node
  | _ ->
    let node = Node.lazy_ (lazy (f ())) in
    Weak_map.set t key (deps, inject, node);
    node
;;
