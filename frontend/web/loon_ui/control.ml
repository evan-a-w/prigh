open! Import

type 'a t =
  { value : 'a
  ; set : 'a -> unit Effect.t
  ; view : Node.t
  }

let make_named ?equal default ~view (local_ graph) =
  let value, set = Bonsai.state ?equal default graph in
  let name = Bonsai.path_id graph in
  let open Bonsai.Let_syntax in
  let%arr value
  and set
  and name in
  { value; set; view = view ~name:("loon" ^ name) value set }
;;

let make ?equal default ~view (local_ graph) =
  make_named
    ?equal
    default
    ~view:(fun ~name:_ value set -> view value set)
    graph
;;
