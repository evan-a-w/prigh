open! Core
open! Import

module Compare = struct
  type t =
    { base : string option
    ; direct : bool
    }
  [@@deriving sexp_of, equal]
end

type t =
  { compares : (string * Compare.t) list (** newest first *)
  ; lasts : (string * Git_diff.Target.t) list
  }
[@@deriving sexp_of, equal]

let empty = { compares = []; lasts = [] }
let max = 50

let target_to_string : Git_diff.Target.t -> string = function
  | Dir None -> "own"
  | Dir (Some dir) -> "dir:" ^ dir
  | Rev rev -> "rev:" ^ rev
;;

let target_of_string s : Git_diff.Target.t option =
  if String.equal s "own"
  then Some (Dir None)
  else (
    match String.lsplit2 s ~on:':' with
    | Some ("dir", dir) -> Some (Dir (Some dir))
    | Some ("rev", rev) -> Some (Rev rev)
    | _ -> None)
;;

let fields = function
  | Some (`Object fields) -> fields
  | _ -> []
;;

let of_json json =
  let section name =
    fields (List.Assoc.find (fields json) name ~equal:String.equal)
  in
  { compares =
      List.map (section "compare") ~f:(fun (key, v) ->
        ( key
        , { Compare.base =
              (match Json.field v "base" with
               | Some (`String b) -> Some b
               | _ -> None)
          ; direct = Option.exists (Json.field v "direct") ~f:(Json.equal `True)
          } ))
  ; lasts =
      List.filter_map (section "last") ~f:(fun (own, v) ->
        match v with
        | `String s -> Option.map (target_of_string s) ~f:(fun t -> own, t)
        | _ -> None)
  }
;;

let to_json t =
  Json.obj
    [ ( "compare"
      , Json.obj
          (List.map t.compares ~f:(fun (key, (c : Compare.t)) ->
             ( key
             , Json.obj
                 (Option.to_list
                    (Option.map c.base ~f:(fun b -> "base", Json.str b))
                  @ [ "direct", Json.bool c.direct ]) ))) )
    ; ( "last"
      , Json.obj
          (List.map t.lasts ~f:(fun (own, target) ->
             own, Json.str (target_to_string target))) )
    ]
;;

let key ~own : Git_diff.Target.t -> string option = function
  | Dir None -> own
  | Dir (Some dir) -> Some dir
  | Rev rev -> Some ("branch:" ^ rev)
;;

let compare t ~key = List.Assoc.find t.compares key ~equal:String.equal
let last t ~own = List.Assoc.find t.lasts own ~equal:String.equal

let front list ~key data =
  List.take
    ((key, data) :: List.filter list ~f:(fun (k, _) -> not (String.equal k key)))
    max
;;

let without list ~key =
  List.filter list ~f:(fun (k, _) -> not (String.equal k key))
;;

(* Only what differs from the defaults is kept. *)
let remember t ~own reviewed (compare : Compare.t) =
  let t =
    match key ~own reviewed, compare with
    | Some key, { base = None; direct = false } ->
      { t with compares = without t.compares ~key }
    | Some key, _ -> { t with compares = front t.compares ~key compare }
    | None, _ -> t
  in
  match own, reviewed with
  | Some own, Dir None -> { t with lasts = without t.lasts ~key:own }
  | Some own, _ -> { t with lasts = front t.lasts ~key:own reviewed }
  | None, _ -> t
;;
