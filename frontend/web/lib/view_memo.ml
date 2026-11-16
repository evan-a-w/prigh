open! Core
open! Import

type 'deps t =
  { name : string
  ; mutable last : ('deps * Node.t) option
  }

let create name = { name; last = None }
let made_since = Queue.create ()

let made () =
  let names = Queue.to_list made_since in
  Queue.clear made_since;
  names
;;

let find t ~deps ~equal ~f =
  match t.last with
  | Some (deps', node) when equal deps deps' -> node
  | _ ->
    if Queue.length made_since < 1000 then Queue.enqueue made_since t.name;
    (* Made at once: whether it is an element decides how it can be kept. A
       lazy node is one DOM node, so nothing and fragments stay as they are
       (the same value, converted again: cheap, and their parts may be lazy
       themselves). *)
    let node =
      match (f () : Node.t) with
      | (None | Fragment _ | Lazy _) as node -> node
      | (Element _ | Text _ | Widget _) as node ->
        Node.lazy_ (Lazy.from_val node)
    in
    t.last <- Some (deps, node);
    node
;;
