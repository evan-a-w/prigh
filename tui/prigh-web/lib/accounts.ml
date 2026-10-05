open! Core
open! Import

module Storage = struct
  type t =
    { get : string -> string option
    ; set : string -> string -> unit
    ; remove : string -> unit
    }

  let in_memory () =
    let table = String.Table.create () in
    { get = Hashtbl.find table
    ; set = (fun key data -> Hashtbl.set table ~key ~data)
    ; remove = Hashtbl.remove table
    }
  ;;
end

module Account = struct
  type t =
    { backend : string
    ; user : string option
    ; token : string option
    ; session : string option
    }
  [@@deriving sexp_of, equal]

  let same a b =
    String.equal a.backend b.backend
    &&
    match a.user, b.user with
    | Some x, Some y -> String.equal x y
    | None, None -> Option.equal String.equal a.token b.token
    | Some _, None | None, Some _ -> false
  ;;

  let name t =
    match t.user, t.token with
    | Some user, _ -> user
    | None, Some _ -> "token"
    | None, None -> "anonymous"
  ;;

  let host t =
    let rest =
      match String.lsplit2 t.backend ~on:':' with
      | Some (_, rest) when String.is_prefix rest ~prefix:"//" ->
        String.drop_prefix rest 2
      | _ -> t.backend
    in
    match String.lsplit2 rest ~on:'/' with
    | Some (host, _) -> host
    | None -> rest
  ;;

  let to_json t =
    let opt name = Option.value_map ~default:[] ~f:(fun v -> [ name, `String v ]) in
    `Object
      ([ "backend", `String t.backend ]
       @ opt "user" t.user
       @ opt "token" t.token
       @ opt "session" t.session)
  ;;

  let of_json json =
    let open Option.Let_syntax in
    let str name =
      match Json.field json name with
      | Some (`String s) when not (String.is_empty s) -> Some s
      | _ -> None
    in
    let%map backend = str "backend" in
    { backend
    ; user = str "user"
    ; token = str "token"
    ; session = str "session"
    }
  ;;
end

let key = "prigh-web.accounts"
let user_key = "prigh.user"
let token_key = "prigh.token"

let saved (storage : Storage.t) =
  match Option.map (storage.get key) ~f:Json.parse with
  | Some (Ok (`Array items)) -> List.filter_map items ~f:Account.of_json
  | _ -> []
;;

let save (storage : Storage.t) accounts =
  storage.set key (Json.to_string (`Array (List.map accounts ~f:Account.to_json)))
;;

let signed_in (storage : Storage.t) ~backend =
  let get key =
    Option.bind (storage.get key) ~f:(fun v ->
      Option.some_if (not (String.is_empty (String.strip v))) (String.strip v))
  in
  match get user_key, get token_key with
  | None, None -> None
  | user, token -> Some { Account.backend; user; token; session = None }
;;

let load = saved

let remember storage ~backend =
  let accounts = saved storage in
  match signed_in storage ~backend with
  | None -> accounts
  | Some account ->
    let accounts =
      if List.exists accounts ~f:(Account.same account)
      then
        (* The same user may have signed in with a new token. *)
        List.map accounts ~f:(fun a ->
          if Account.same a account then { a with token = account.token } else a)
      else accounts @ [ account ]
    in
    save storage accounts;
    accounts
;;

let current storage ~backend =
  Option.map (signed_in storage ~backend) ~f:(fun account ->
    match List.find (saved storage) ~f:(Account.same account) with
    | Some saved -> { account with session = saved.session }
    | None -> account)
;;

let activate (storage : Storage.t) (account : Account.t) =
  let store key = function
    | Some value -> storage.set key value
    | None -> storage.remove key
  in
  store user_key account.user;
  store token_key account.token
;;

let remove (storage : Storage.t) account =
  save storage (List.filter (saved storage) ~f:(Fn.non (Account.same account)));
  match signed_in storage ~backend:account.backend with
  | Some active when Account.same active account ->
    storage.remove user_key;
    storage.remove token_key
  | _ -> ()
;;

let set_session storage account session =
  let accounts = saved storage in
  if List.exists accounts ~f:(Account.same account)
  then
    save
      storage
      (List.map accounts ~f:(fun (a : Account.t) ->
         if Account.same a account then { a with session = Some session } else a))
;;
