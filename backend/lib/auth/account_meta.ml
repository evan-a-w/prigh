open! Core
open! Import

module Account = struct
  type t =
    { id : Account_id.t
    ; label : string
    ; fallback : bool
    ; created_ms : int option
    }
  [@@deriving sexp, equal]

  let to_json t =
    `Object
      ([ "id", `String (Account_id.to_string t.id)
       ; "label", `String t.label
       ; ("fallback", if t.fallback then `True else `False)
       ]
       @ Option.value_map t.created_ms ~default:[] ~f:(fun ms ->
         [ "created_ms", `Number (Int.to_string ms) ]))
  ;;

  let of_json = function
    | `Object fields ->
      let open Or_error.Let_syntax in
      let find name = List.Assoc.find fields ~equal:String.equal name in
      let%bind id =
        match find "id" with
        | Some (`String s) -> Account_id.of_string s
        | _ -> Or_error.error_string "an account has no \"id\""
      in
      let label =
        match find "label" with
        | Some (`String s) -> s
        | _ -> Account_id.to_string id
      in
      let fallback =
        match find "fallback" with
        | Some `False -> false
        | _ -> true
      in
      let created_ms =
        match find "created_ms" with
        | Some (`Number n) -> Int.of_string_opt n
        | _ -> None
      in
      Ok { id; label; fallback; created_ms }
    | _ -> Or_error.error_string "an account is not an object"
  ;;
end

module Provider_meta = struct
  type t =
    { default : Account_id.t option
    ; accounts : Account.t list
    }
  [@@deriving sexp, equal]

  let to_json t =
    `Object
      (Option.value_map t.default ~default:[] ~f:(fun id ->
         [ "default", `String (Account_id.to_string id) ])
       @ [ "accounts", `Array (List.map t.accounts ~f:Account.to_json) ])
  ;;

  let of_json = function
    | `Object fields ->
      let open Or_error.Let_syntax in
      let%bind default =
        match List.Assoc.find fields ~equal:String.equal "default" with
        | Some (`String s) -> Account_id.of_string s >>| Option.some
        | _ -> Ok None
      in
      let%map accounts =
        match List.Assoc.find fields ~equal:String.equal "accounts" with
        | Some (`Array items) ->
          Or_error.all (List.map items ~f:Account.of_json)
        | _ -> Ok []
      in
      { default; accounts }
    | _ -> Or_error.error_string "a provider's entry is not an object"
  ;;
end

type t = (string * Provider_meta.t) list [@@deriving sexp_of, equal]

let key = "prigh:accounts"
let empty = []
let is_empty = List.is_empty

let find t provider =
  List.Assoc.find t ~equal:String.equal (Provider_id.to_string provider)
;;

let set t provider (meta : Provider_meta.t) =
  let name = Provider_id.to_string provider in
  if List.is_empty meta.accounts
  then List.Assoc.remove t ~equal:String.equal name
  else if List.Assoc.mem t ~equal:String.equal name
  then
    List.map t ~f:(fun (p, m) -> if String.equal p name then p, meta else p, m)
  else t @ [ name, meta ]
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; ( "providers"
      , `Object (List.map t ~f:(fun (p, m) -> p, Provider_meta.to_json m)) )
    ]
;;

let of_json json =
  let parsed =
    match json with
    | `Object fields ->
      (match List.Assoc.find fields ~equal:String.equal "providers" with
       | None -> Ok []
       | Some (`Object providers) ->
         Or_error.all
           (List.map providers ~f:(fun (p, m) ->
              Or_error.map (Provider_meta.of_json m) ~f:(fun m -> p, m)))
       | Some _ -> Or_error.error_string "\"providers\" is not an object")
    | _ -> Or_error.error_string "it is not an object"
  in
  Or_error.tag
    parsed
    ~tag:
      (sprintf
         "auth.json's %S entry is unreadable: fix it, or remove it (the \
          accounts' labels and order are lost, not their logins)"
         key)
;;
