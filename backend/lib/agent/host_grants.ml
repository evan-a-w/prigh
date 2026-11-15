open! Core
open! Import

type t = String.Hash_set.t

let create () = String.Hash_set.create ()
let clear t = Hash_set.clear t
let grant t ~host = Hash_set.add t host

let names text ~word =
  let text = String.lowercase text in
  let word = String.lowercase word in
  let part c = Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' in
  let edge i = i < 0 || i >= String.length text || not (part text.[i]) in
  (not (String.is_empty word))
  && List.exists (String.substr_index_all text ~may_overlap:true ~pattern:word)
       ~f:(fun i -> edge (i - 1) && edge (i + String.length word))
;;

let allows t ~host ~name ~requests =
  Hash_set.mem t host
  || List.exists requests ~f:(fun text ->
    names text ~word:name || names text ~word:host)
;;
