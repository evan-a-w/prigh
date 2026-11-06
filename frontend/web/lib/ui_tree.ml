open! Core

module Row = struct
  type 'a t =
    { depth : int
    ; name : string
    ; path : string
    ; dir : bool
    ; info : 'a option
    }
  [@@deriving sexp_of]
end

module Node = struct
  type 'a t =
    { name : string
    ; path : string
    ; mutable dir : bool
    ; mutable info : 'a option
    ; mutable children : 'a t list (** newest first *)
    }
end

let rows entries =
  let root = { Node.name = ""; path = ""; dir = true; info = None; children = [] } in
  let child (parent : _ Node.t) name ~path =
    match List.find parent.children ~f:(fun c -> String.equal c.name name) with
    | Some c -> c
    | None ->
      let c = { Node.name; path; dir = true; info = None; children = [] } in
      parent.children <- c :: parent.children;
      c
  in
  List.iter entries ~f:(fun (path, info) ->
    let dir = String.is_suffix path ~suffix:"/" in
    let absolute = String.is_prefix path ~prefix:"/" in
    let parts =
      String.split path ~on:'/' |> List.filter ~f:(fun s -> not (String.is_empty s))
    in
    let last = List.length parts - 1 in
    let node, _ =
      List.foldi parts ~init:(root, if absolute then "/" else "") ~f:(fun i (n, prefix) part ->
        let path = if i = last then path else prefix ^ part ^ "/" in
        child n part ~path, prefix ^ part ^ "/")
    in
    if not (phys_equal node root)
    then (
      node.dir <- dir;
      node.info <- Some info));
  let rec flatten depth (n : _ Node.t) =
    List.concat_map (List.rev n.children) ~f:(fun (c : _ Node.t) ->
      let dir = c.dir || not (List.is_empty c.children) in
      { Row.depth; name = (if dir then c.name ^ "/" else c.name); path = c.path; dir; info = c.info }
      :: flatten (depth + 1) c)
  in
  flatten 0 root
;;
