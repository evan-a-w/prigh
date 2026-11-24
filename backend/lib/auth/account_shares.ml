open! Core
open! Import

module User = struct
  type t =
    { store : Auth_store.t
    ; ledger : Account_ledger.t
    ; mutable subscribers : (unit -> unit) list
    ; mutable recipients : String.Set.t
      (** whom the user's logins were shared with when last looked at *)
    }
end

type t =
  { enabled : bool
  ; users : User.t String.Table.t
  }

let create ?(enabled = true) () = { enabled; users = String.Table.create () }
let enabled t = t.enabled

module Member = struct
  type nonrec t =
    { shares : t
    ; user : string
    }
end

let off = "sharing is off on this server (prigh serve -no-sharing)"
let check_enabled t = if t.enabled then Ok () else Or_error.error_string off

let grants_of store =
  match Provider_accounts.list_all store with
  | Error _ -> String.Set.empty
  | Ok providers ->
    List.concat_map providers ~f:(fun (_, accounts) ->
      List.concat_map accounts ~f:(fun (a : Provider_accounts.Account.t) ->
        List.map a.shared_with ~f:(fun g -> g.user)))
    |> String.Set.of_list
;;

let register t ~user ~store ~home =
  Hashtbl.set
    t.users
    ~key:user
    ~data:
      { User.store
      ; ledger = Account_ledger.create ~home
      ; subscribers = []
      ; recipients = grants_of store
      }
;;

let subscribe t ~user ~f =
  Option.iter (Hashtbl.find t.users user) ~f:(fun u ->
    u.subscribers <- u.subscribers @ [ f ])
;;

let notify t users =
  Set.iter users ~f:(fun user ->
    Option.iter (Hashtbl.find t.users user) ~f:(fun (u : User.t) ->
      List.iter u.subscribers ~f:(fun f -> f ())))
;;

let changed t ~owner =
  Option.iter (Hashtbl.find t.users owner) ~f:(fun (u : User.t) ->
    let before = u.recipients in
    u.recipients <- grants_of u.store;
    notify t (Set.union before u.recipients))
;;

let others t ~user =
  Hashtbl.keys t.users
  |> List.filter ~f:(Fn.non (String.equal user))
  |> List.sort ~compare:String.compare
;;

let targets t ~user =
  Or_error.map (check_enabled t) ~f:(fun () -> others t ~user)
;;

let find_user t user =
  match Hashtbl.find t.users user with
  | Some u -> Ok u
  | None -> Or_error.errorf "no user %S" user
;;

let share t ~owner provider id ~users =
  let open Or_error.Let_syntax in
  let%bind () = check_enabled t in
  let%bind u = find_user t owner in
  let%bind () =
    if Provider_id.is_custom provider
    then
      Or_error.errorf
        "%s is a custom provider: only the built-in providers' logins can be \
         shared"
        (Provider_id.to_string provider)
    else Ok ()
  in
  let%bind () =
    match
      List.find users ~f:(fun user ->
        String.equal user owner || not (Hashtbl.mem t.users user))
    with
    | None -> Ok ()
    | Some user ->
      let choices =
        match others t ~user:owner with
        | [] -> "there is no other user"
        | others -> "share with " ^ String.concat ~sep:", " others
      in
      if String.equal user owner
      then Or_error.errorf "%s is you: %s" user choices
      else Or_error.errorf "no user %S: %s" user choices
  in
  let%map grants = Provider_accounts.share u.store provider id ~users in
  changed t ~owner;
  grants
;;

let key provider ~owner id =
  sprintf
    "~%s/%s/%s"
    owner
    (Provider_id.to_string provider)
    (Account_id.to_string id)
;;

module Shared = struct
  type t =
    { owner : string
    ; provider : Provider_id.t
    ; id : Account_id.t
    ; label : string
    ; method_ : Login_method.t
    ; pref : Account_meta.Shared_pref.t
    }
  [@@deriving sexp_of]

  let ref_ t = Account_ref.Shared { owner = t.owner; id = t.id }
  let key t = key t.provider ~owner:t.owner t.id
end

let shared_with t ~user =
  if not t.enabled
  then []
  else (
    let prefs =
      match Hashtbl.find t.users user with
      | None -> Account_meta.empty
      | Some u ->
        (match Auth_store.read_entry u.store Account_meta.key with
         | Ok (Some json) ->
           Result.ok (Account_meta.of_json json)
           |> Option.value ~default:Account_meta.empty
         | Ok None | Error _ -> Account_meta.empty)
    in
    List.concat_map (others t ~user) ~f:(fun owner ->
      let u = Hashtbl.find_exn t.users owner in
      match Provider_accounts.list_all u.store with
      | Error _ -> []
      | Ok providers ->
        List.concat_map providers ~f:(fun (provider, accounts) ->
          List.filter_map accounts ~f:(fun (a : Provider_accounts.Account.t) ->
            if List.exists a.shared_with ~f:(fun g -> String.equal g.user user)
            then
              Some
                { Shared.owner
                ; provider
                ; id = a.id
                ; label = a.label
                ; method_ = Login_method.of_credential a.credential
                ; pref =
                    Account_meta.shared_pref prefs (key provider ~owner a.id)
                }
            else None))))
;;

let find_account t provider ~owner id =
  let open Or_error.Let_syntax in
  let%bind u = find_user t owner in
  let%map accounts = Provider_accounts.list u.store provider in
  ( u
  , List.find accounts ~f:(fun (a : Provider_accounts.Account.t) ->
      Account_id.equal a.id id) )
;;

let granted t ~recipient provider ~owner id =
  t.enabled
  &&
  match find_account t provider ~owner id with
  | Ok (_, Some a) ->
    List.exists a.shared_with ~f:(fun g -> String.equal g.user recipient)
  | Ok (_, None) | Error _ -> false
;;

let resolve t ~env ?cancel ?refresh ~recipient provider ~owner id =
  let open Or_error.Let_syntax in
  let%bind () = check_enabled t in
  let%bind owner_user, account = find_account t provider ~owner id in
  match account with
  | None ->
    Or_error.errorf
      "%s no longer has the %s login %S it shared with you: /accounts picks \
       another"
      owner
      (Provider_id.display_name provider)
      (Account_id.to_string id)
  | Some a
    when not
           (List.exists a.shared_with ~f:(fun g ->
              String.equal g.user recipient)) ->
    Or_error.errorf
      "%s does not share the %s login %S with you (any more): /accounts picks \
       another"
      owner
      (Provider_id.display_name provider)
      a.label
  | Some _ ->
    (match%bind
       Provider_auth.resolve
         ~env
         ?cancel
         ~getenv:(fun _ -> None)
         ?refresh
         ~account:id
         owner_user.store
         provider
     with
     | Some resolved -> Ok resolved
     | None ->
       Or_error.errorf
         "%s's %s login is not usable: ask %s to log in again"
         owner
         (Provider_id.display_name provider)
         owner)
;;

let set_pref t ~user provider ~owner id ~f =
  let open Or_error.Let_syntax in
  let%bind u = find_user t user in
  Provider_accounts.set_shared_pref u.store (key provider ~owner id) ~f
;;

let login_key = Account_ledger.login

let record ?now t ~recipient provider ~owner id usage ~cost_usd =
  Option.iter (Hashtbl.find t.users owner) ~f:(fun (u : User.t) ->
    Account_ledger.record
      ?now
      u.ledger
      ~login:(login_key provider id)
      ~user:recipient
      usage
      ~cost_usd)
;;

let record_request
      ?now
      ?member
      ?ledger
      provider
      (account : Account_ref.t)
      usage
      ~cost_usd
  =
  match account, member, ledger with
  | Own _, _, None -> ()
  | Own id, _, Some ledger ->
    Account_ledger.record
      ?now
      ledger
      ~login:(login_key provider id)
      ~user:
        (Option.value_map
           member
           ~default:Account_ledger.single_user
           ~f:(fun m -> m.Member.user))
      usage
      ~cost_usd
  | Shared { owner; id }, Some { Member.shares; user }, _ ->
    record ?now shares ~recipient:user provider ~owner id usage ~cost_usd
  | Shared _, None, _ -> ()
;;

let usage ?now t ~user provider ~owner id =
  let open Or_error.Let_syntax in
  let%map u = find_user t owner in
  Account_ledger.rows
    ?now
    ?user:(Option.some_if (not (String.equal user owner)) user)
    u.ledger
    ~login:(login_key provider id)
;;
