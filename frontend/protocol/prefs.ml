open! Core

module Change = struct
  type t =
    { key : string
    ; value : Json.t option
    }
  [@@deriving sexp_of, equal]
end

let changes_of_json = function
  | `Object fields ->
    Ok
      (List.map fields ~f:(fun (key, value) ->
         { Change.key
         ; value =
             (match value with
              | `Null -> None
              | value -> Some value)
         }))
  | _ -> Or_error.error_string "prefs: expected an object"
;;

let of_reply json =
  Or_error.bind (Json.object_field json "prefs") ~f:changes_of_json
;;

let get_params ~keys =
  [ "keys", `Array (List.map keys ~f:(fun key -> `String key)) ]
;;

let set_params changes =
  [ ( "prefs"
    , `Object
        (List.map changes ~f:(fun { Change.key; value } ->
           key, Option.value value ~default:`Null)) )
  ]
;;

let push_params ~key value ~max =
  [ "key", `String key; "value", value; "max", Json.int max ]
;;
