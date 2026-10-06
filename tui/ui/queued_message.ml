open! Core

module Kind = struct
  type t =
    | Steer
    | Follow_up
  [@@deriving sexp_of, equal]

  let name = function
    | Steer -> "steer"
    | Follow_up -> "follow-up"
  ;;
end

type t =
  { kind : Kind.t
  ; text : string
  }
[@@deriving sexp_of, equal]

let remove_first ts ~text =
  match List.findi ts ~f:(fun _ q -> String.equal q.text text) with
  | Some (i, _) -> List.filteri ts ~f:(fun j _ -> j <> i)
  | None -> ts
;;

let remove_last ts ~text = List.rev (remove_first (List.rev ts) ~text)
