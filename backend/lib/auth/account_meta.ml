open! Core
open! Import

let field fields name = List.Assoc.find fields ~equal:String.equal name

let bool_field fields name ~default =
  match field fields name with
  | Some `True -> true
  | Some `False -> false
  | _ -> default
;;

let json_bool b = if b then `True else `False

module Grant = struct
  type t =
    { user : string
    ; since_ms : int
    }
  [@@deriving sexp, equal]

  let to_json t =
    `Object
      [ "user", `String t.user; "since_ms", `Number (Int.to_string t.since_ms) ]
  ;;

  let of_json = function
    | `Object fields ->
      (match field fields "user" with
       | Some (`String user) ->
         let since_ms =
           match field fields "since_ms" with
           | Some (`Number n) -> Option.value (Int.of_string_opt n) ~default:0
           | _ -> 0
         in
         Ok { user; since_ms }
       | _ -> Or_error.error_string "a grant has no \"user\"")
    | _ -> Or_error.error_string "a grant is not an object"
  ;;
end

module Account = struct
  type t =
    { id : Account_id.t
    ; label : string
    ; fallback : bool
    ; created_ms : int option
    ; shared_with : Grant.t list
    }
  [@@deriving sexp, equal]

  let to_json t =
    `Object
      ([ "id", `String (Account_id.to_string t.id)
       ; "label", `String t.label
       ; "fallback", json_bool t.fallback
       ]
       @ Option.value_map t.created_ms ~default:[] ~f:(fun ms ->
         [ "created_ms", `Number (Int.to_string ms) ])
       @
       match t.shared_with with
       | [] -> []
       | grants -> [ "shared_with", `Array (List.map grants ~f:Grant.to_json) ]
      )
  ;;

  let of_json = function
    | `Object fields ->
      let open Or_error.Let_syntax in
      let%bind id =
        match field fields "id" with
        | Some (`String s) -> Account_id.of_string s
        | _ -> Or_error.error_string "an account has no \"id\""
      in
      let label =
        match field fields "label" with
        | Some (`String s) -> s
        | _ -> Account_id.to_string id
      in
      let fallback = bool_field fields "fallback" ~default:true in
      let created_ms =
        match field fields "created_ms" with
        | Some (`Number n) -> Int.of_string_opt n
        | _ -> None
      in
      let%map shared_with =
        match field fields "shared_with" with
        | Some (`Array grants) ->
          Or_error.all (List.map grants ~f:Grant.of_json)
        | _ -> Ok []
      in
      { id; label; fallback; created_ms; shared_with }
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

module Shared_pref = struct
  type t =
    { hidden : bool
    ; fallback : bool
    }
  [@@deriving sexp, equal]

  let default = { hidden = false; fallback = false }

  let to_json t =
    `Object [ "hidden", json_bool t.hidden; "fallback", json_bool t.fallback ]
  ;;

  let of_json = function
    | `Object fields ->
      Ok
        { hidden = bool_field fields "hidden" ~default:false
        ; fallback = bool_field fields "fallback" ~default:false
        }
    | _ -> Or_error.error_string "a shared login's entry is not an object"
  ;;
end

type t =
  { providers : (string * Provider_meta.t) list
  ; shared : (string * Shared_pref.t) list
  }
[@@deriving sexp_of, equal]

let key = "prigh:accounts"
let empty = { providers = []; shared = [] }
let is_empty t = List.is_empty t.providers && List.is_empty t.shared

let find t provider =
  List.Assoc.find
    t.providers
    ~equal:String.equal
    (Provider_id.to_string provider)
;;

let set t provider (meta : Provider_meta.t) =
  let name = Provider_id.to_string provider in
  let providers =
    if List.is_empty meta.accounts
    then List.Assoc.remove t.providers ~equal:String.equal name
    else if List.Assoc.mem t.providers ~equal:String.equal name
    then
      List.map t.providers ~f:(fun (p, m) ->
        if String.equal p name then p, meta else p, m)
    else t.providers @ [ name, meta ]
  in
  { t with providers }
;;

let shared_pref t ref_ =
  Option.value
    (List.Assoc.find t.shared ~equal:String.equal ref_)
    ~default:Shared_pref.default
;;

let set_shared_pref t ref_ pref =
  let others = List.Assoc.remove t.shared ~equal:String.equal ref_ in
  { t with
    shared =
      (if Shared_pref.equal pref Shared_pref.default
       then others
       else others @ [ ref_, pref ])
  }
;;

let to_json t =
  `Object
    ([ "version", `Number "1"
     ; ( "providers"
       , `Object
           (List.map t.providers ~f:(fun (p, m) -> p, Provider_meta.to_json m))
       )
     ]
     @
     match t.shared with
     | [] -> []
     | shared ->
       [ ( "shared"
         , `Object (List.map shared ~f:(fun (r, p) -> r, Shared_pref.to_json p))
         )
       ])
;;

let of_json json =
  let parsed =
    match json with
    | `Object fields ->
      let open Or_error.Let_syntax in
      let%bind providers =
        match field fields "providers" with
        | None -> Ok []
        | Some (`Object providers) ->
          Or_error.all
            (List.map providers ~f:(fun (p, m) ->
               Or_error.map (Provider_meta.of_json m) ~f:(fun m -> p, m)))
        | Some _ -> Or_error.error_string "\"providers\" is not an object"
      in
      let%map shared =
        match field fields "shared" with
        | None -> Ok []
        | Some (`Object shared) ->
          Or_error.all
            (List.map shared ~f:(fun (r, p) ->
               Or_error.map (Shared_pref.of_json p) ~f:(fun p -> r, p)))
        | Some _ -> Or_error.error_string "\"shared\" is not an object"
      in
      { providers; shared }
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
