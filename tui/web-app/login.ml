open! Core

module Storage = struct
  type t =
    { get : string -> string option
    ; set : string -> string -> unit
    ; remove : string -> unit
    }

  let browser =
    { get = Browser.get_item
    ; set = Browser.set_item
    ; remove = Browser.remove_item
    }
  ;;
end

type t =
  { user : string option
  ; password : string option
  }
[@@deriving sexp_of]

let user_key = "prigh.user"
let password_key = "prigh.token"

let load (storage : Storage.t) =
  let get key = Option.filter (storage.get key) ~f:(Fn.non String.is_empty) in
  { user = get user_key; password = get password_key }
;;

let save (storage : Storage.t) ~user ~password =
  let store key value =
    match String.strip value with
    | "" -> storage.remove key
    | value -> storage.set key value
  in
  store user_key user;
  store password_key password
;;

let signed_out_key = "prigh.signed_out"

let forget (storage : Storage.t) =
  storage.remove user_key;
  storage.remove password_key;
  storage.set signed_out_key "1"
;;

let take_signed_out (storage : Storage.t) =
  let signed_out = Option.is_some (storage.get signed_out_key) in
  storage.remove signed_out_key;
  signed_out
;;

let hello_fields t =
  List.filter_opt
    [ Option.map t.user ~f:(fun user -> "user", `String user)
    ; Option.map t.password ~f:(fun token -> "token", `String token)
    ]
;;
