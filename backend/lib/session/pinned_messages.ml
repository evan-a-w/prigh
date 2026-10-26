open! Core
open! Import

module Pin = struct
  type t =
    { id : string
    ; shortened : bool [@jsonaf.default false] [@jsonaf_drop_default.equal]
    }
  [@@deriving sexp, jsonaf, equal]
end

type t = Pin.t list [@@deriving sexp, jsonaf_of, equal]

let t_of_jsonaf : Jsonaf.t -> t = function
  | `String id -> [ { id; shortened = false } ]
  | json -> [%of_jsonaf: Pin.t list] json
;;

let max_chars = 2_000

(* A text this much longer than [max_chars] is cut: more than the note, so
   that cutting again leaves a cut text as it is. *)
let slack = 64

let shorten (m : Message.t) =
  match m with
  | User u when String.length u.text > max_chars + slack ->
    let head, _ =
      Utf8.split_incomplete_suffix (String.prefix u.text max_chars)
    in
    Message.User
      { u with
        text =
          sprintf
            "%s\n[... %d characters omitted]"
            head
            (String.length u.text - String.length head)
      }
  | User _ | Assistant _ | Tool_result _ -> m
;;
