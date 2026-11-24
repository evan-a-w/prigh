open! Core
open! Import

module Account = struct
  type t =
    { id : Account_id.t
    ; label : string
    ; credential : Credential.t
    ; default : bool
    ; fallback : bool
    ; shared_with : Account_meta.Grant.t list
    }
  [@@deriving sexp_of]
end

let prefix provider = sprintf "account:%s:" (Provider_id.to_string provider)

let entry_key provider id =
  if Account_id.is_main id
  then Provider_id.to_string provider
  else prefix provider ^ Account_id.to_string id
;;

let assoc fields key = List.Assoc.find fields ~equal:String.equal key

let meta_of_fields fields =
  match assoc fields Account_meta.key with
  | None -> Ok Account_meta.empty
  | Some json -> Account_meta.of_json json
;;

let credentials ~file provider fields =
  let main_key = Provider_id.to_string provider in
  let prefix = prefix provider in
  List.filter_map fields ~f:(fun (key, json) ->
    let id =
      if String.equal key main_key
      then Some (Ok Account_id.main)
      else Option.map (String.chop_prefix key ~prefix) ~f:Account_id.of_string
    in
    Option.map id ~f:(fun id ->
      let open Or_error.Let_syntax in
      let%bind id = id in
      match Credential.of_json json with
      | Ok c -> Ok (id, c)
      | Error e ->
        Or_error.errorf
          "%s: the %s credential is unreadable (%s): log in to %s again"
          file
          key
          (Error.to_string_hum e)
          (Provider_id.to_string provider)))
  |> Or_error.all
  |> Or_error.map
       ~f:
         (List.stable_sort ~compare:(fun (a, _) (b, _) ->
            Bool.compare
              (not (Account_id.is_main a))
              (not (Account_id.is_main b))))
;;

let default_fallback : Credential.t -> bool = function
  | Oauth _ -> true
  | Api_key _ -> false
;;

(* The metadata, the provider's own part of it and its accounts. *)
let view ~file provider fields =
  let open Or_error.Let_syntax in
  let%bind meta = meta_of_fields fields in
  let%map credentials = credentials ~file provider fields in
  let provider_meta = Account_meta.find meta provider in
  let described =
    Option.value_map provider_meta ~default:[] ~f:(fun m -> m.accounts)
  in
  let ordered =
    List.filter_map described ~f:(fun (a : Account_meta.Account.t) ->
      Option.map
        (List.Assoc.find credentials ~equal:Account_id.equal a.id)
        ~f:(fun c -> a.id, Some a, c))
    @ List.filter_map credentials ~f:(fun (id, c) ->
      if
        List.exists described ~f:(fun (a : Account_meta.Account.t) ->
          Account_id.equal a.id id)
      then None
      else Some (id, None, c))
  in
  let default_id =
    match Option.bind provider_meta ~f:(fun m -> m.default) with
    | Some d
      when List.exists ordered ~f:(fun (id, _, _) -> Account_id.equal id d) ->
      Some d
    | _ -> Option.map (List.hd ordered) ~f:(fun (id, _, _) -> id)
  in
  let accounts =
    List.map
      ordered
      ~f:(fun (id, (described : Account_meta.Account.t option), credential) ->
        { Account.id
        ; label =
            (match described with
             | Some d -> d.label
             | None ->
               if Account_id.is_main id
               then
                 Login_method.label
                   provider
                   (Login_method.of_credential credential)
               else Account_id.to_string id)
        ; credential
        ; default = Option.equal Account_id.equal (Some id) default_id
        ; fallback =
            (match described with
             | Some d -> d.fallback
             | None -> default_fallback credential)
        ; shared_with =
            Option.value_map described ~default:[] ~f:(fun d -> d.shared_with)
        })
  in
  meta, provider_meta, accounts
;;

let list store provider =
  Or_error.bind (Auth_store.entries store ~prefix:"") ~f:(fun fields ->
    Or_error.map
      (view ~file:(Auth_store.path store) provider fields)
      ~f:(fun (_, _, accounts) -> accounts))
;;

let default store provider =
  Or_error.map
    (list store provider)
    ~f:(List.find ~f:(fun a -> a.Account.default))
;;

let describe accounts =
  match accounts with
  | [] -> "it has none"
  | accounts ->
    "it has "
    ^ String.concat
        ~sep:", "
        (List.map accounts ~f:(fun (a : Account.t) ->
           sprintf "%s (%s)" a.label (Account_id.to_string a.id)))
;;

let find_in provider accounts name =
  match
    List.find accounts ~f:(fun (a : Account.t) ->
      String.equal (Account_id.to_string a.id) name)
  with
  | Some a -> Ok a
  | None ->
    (match
       List.find accounts ~f:(fun (a : Account.t) ->
         String.Caseless.equal a.label name)
     with
     | Some a -> Ok a
     | None ->
       Or_error.errorf
         "%s has no account %S: %s"
         (Provider_id.display_name provider)
         name
         (describe accounts))
;;

let find store provider name =
  Or_error.bind (list store provider) ~f:(fun accounts ->
    find_in provider accounts name)
;;

let modify store provider id ~f =
  let current = ref None in
  let%map.Or_error _ =
    Auth_store.modify_entry store (entry_key provider id) ~f:(fun json ->
      let open Or_error.Let_syntax in
      let%bind credential =
        match json with
        | None -> Ok None
        | Some json -> Or_error.map (Credential.of_json json) ~f:Option.some
      in
      let%map next = f credential in
      current := next;
      if [%equal: Credential.t option] credential next
      then json
      else Option.map next ~f:Credential.to_json)
  in
  !current
;;

let materialise ?provider_meta (accounts : Account.t list) =
  let created id =
    Option.bind provider_meta ~f:(fun (m : Account_meta.Provider_meta.t) ->
      List.find_map m.accounts ~f:(fun a ->
        if Account_id.equal a.id id then a.created_ms else None))
  in
  { Account_meta.Provider_meta.default =
      List.find_map accounts ~f:(fun a -> Option.some_if a.default a.id)
  ; accounts =
      List.map accounts ~f:(fun a ->
        { Account_meta.Account.id = a.id
        ; label = a.label
        ; fallback = a.fallback
        ; created_ms = created a.id
        ; shared_with = a.shared_with
        })
  }
;;

(* [f] gets the provider's accounts and returns the entries to change
   ([None]: remove), the provider's new metadata if it changes, and a
   result. *)
let rewrite store provider ~f =
  let result = ref None in
  let%map.Or_error () =
    Auth_store.modify_all store ~f:(fun fields ->
      let open Or_error.Let_syntax in
      let%bind meta, provider_meta, accounts =
        view ~file:(Auth_store.path store) provider fields
      in
      let%map changes, new_meta, r = f ~provider_meta accounts in
      result := Some r;
      let fields =
        List.fold changes ~init:fields ~f:(fun fields (key, json) ->
          match json with
          | None -> List.Assoc.remove fields ~equal:String.equal key
          | Some json ->
            if List.Assoc.mem fields ~equal:String.equal key
            then
              List.map fields ~f:(fun (k, v) ->
                if String.equal k key then k, json else k, v)
            else fields @ [ key, json ])
      in
      match new_meta with
      | None -> fields
      | Some m ->
        let meta = Account_meta.set meta provider m in
        let json = Account_meta.to_json meta in
        if Account_meta.is_empty meta
        then List.Assoc.remove fields ~equal:String.equal Account_meta.key
        else if List.Assoc.mem fields ~equal:String.equal Account_meta.key
        then
          List.map fields ~f:(fun (k, v) ->
            if String.equal k Account_meta.key then k, json else k, v)
        else fields @ [ Account_meta.key, json ])
  in
  Option.value_exn !result
;;

let max_label = 40

let check_label label =
  let label = String.strip label in
  if String.is_empty label
  then Or_error.error_string "an account's label cannot be empty"
  else if String.length label > max_label
  then Or_error.errorf "an account's label has at most %d characters" max_label
  else Ok label
;;

let same_login (credential : Credential.t) (a : Account.t) =
  match credential, a.credential with
  | Oauth { account_id = Some x; _ }, Oauth { account_id = Some y; _ } ->
    String.equal x y
  | _ -> false
;;

let save_login ?account ?label store provider credential =
  rewrite store provider ~f:(fun ~provider_meta accounts ->
    let open Or_error.Let_syntax in
    let store_as id meta =
      Ok
        ( [ entry_key provider id, Some (Credential.to_json credential) ]
        , meta
        , id )
    in
    match account, label with
    | Some id, _ ->
      if
        Account_id.is_main id
        || List.exists accounts ~f:(fun a -> Account_id.equal a.id id)
      then store_as id None
      else
        Or_error.errorf
          "%s has no account %S: %s"
          (Provider_id.display_name provider)
          (Account_id.to_string id)
          (describe accounts)
    | None, Some label ->
      let%bind label = check_label label in
      (match
         List.find accounts ~f:(fun a -> String.Caseless.equal a.label label)
       with
       | Some a -> store_as a.id None
       | None ->
         (match List.find accounts ~f:(same_login credential) with
          | Some a -> store_as a.id None
          | None ->
            let id =
              if List.is_empty accounts
              then Account_id.main
              else
                Account_id.of_label
                  label
                  ~taken:(List.map accounts ~f:(fun a -> a.id))
            in
            let meta = materialise ?provider_meta accounts in
            let meta =
              { Account_meta.Provider_meta.default =
                  Some (Option.value meta.default ~default:id)
              ; accounts =
                  meta.accounts
                  @ [ { id
                      ; label
                      ; fallback = default_fallback credential
                      ; created_ms = Some (Credential.now_ms ())
                      ; shared_with = []
                      }
                    ]
              }
            in
            store_as id (Some meta)))
    | None, None ->
      store_as
        (match List.find accounts ~f:(fun a -> a.default) with
         | Some a -> a.id
         | None -> Account_id.main)
        None)
;;

let remove store provider id =
  rewrite store provider ~f:(fun ~provider_meta accounts ->
    let meta =
      Option.map provider_meta ~f:(fun (m : Account_meta.Provider_meta.t) ->
        { Account_meta.Provider_meta.default =
            Option.filter m.default ~f:(Fn.non (Account_id.equal id))
        ; accounts =
            List.filter m.accounts ~f:(fun a -> not (Account_id.equal a.id id))
        })
    in
    ignore (accounts : Account.t list);
    Ok ([ entry_key provider id, None ], meta, ()))
;;

module Update = struct
  type t =
    { label : string option
    ; make_default : bool
    ; fallback : bool option
    ; move_before : Account_id.t option option
    }

  let empty =
    { label = None; make_default = false; fallback = None; move_before = None }
  ;;
end

let update store provider id (u : Update.t) =
  rewrite store provider ~f:(fun ~provider_meta accounts ->
    let open Or_error.Let_syntax in
    let%bind account = find_in provider accounts (Account_id.to_string id) in
    let others =
      List.filter accounts ~f:(fun a -> not (Account_id.equal a.id id))
    in
    let%bind label =
      match u.label with
      | None -> Ok account.label
      | Some label ->
        let%bind label = check_label label in
        if List.exists others ~f:(fun a -> String.Caseless.equal a.label label)
        then
          Or_error.errorf
            "%s already has an account labelled %S: pick another label"
            (Provider_id.display_name provider)
            label
        else Ok label
    in
    let account =
      { account with
        label
      ; fallback = Option.value u.fallback ~default:account.fallback
      }
    in
    let%map accounts =
      match u.move_before with
      | None ->
        Ok
          (List.map accounts ~f:(fun a ->
             if Account_id.equal a.id id then account else a))
      | Some None -> Ok (others @ [ account ])
      | Some (Some before) ->
        if Account_id.equal before id
        then Ok accounts
        else (
          match
            List.findi others ~f:(fun _ a -> Account_id.equal a.id before)
          with
          | None ->
            Or_error.errorf
              "%s has no account %S to move before: %s"
              (Provider_id.display_name provider)
              (Account_id.to_string before)
              (describe accounts)
          | Some (i, _) ->
            Ok (List.take others i @ [ account ] @ List.drop others i))
    in
    let accounts =
      if u.make_default
      then
        List.map accounts ~f:(fun a ->
          { a with default = Account_id.equal a.id id })
      else accounts
    in
    [], Some (materialise ?provider_meta accounts), ())
;;

let share store provider id ~users =
  rewrite store provider ~f:(fun ~provider_meta accounts ->
    let open Or_error.Let_syntax in
    let%map account = find_in provider accounts (Account_id.to_string id) in
    let users = List.dedup_and_sort users ~compare:String.compare in
    let grants =
      List.map users ~f:(fun user ->
        match
          List.find account.shared_with ~f:(fun g -> String.equal g.user user)
        with
        | Some g -> g
        | None -> { Account_meta.Grant.user; since_ms = Credential.now_ms () })
    in
    let accounts =
      List.map accounts ~f:(fun a ->
        if Account_id.equal a.id id then { a with shared_with = grants } else a)
    in
    [], Some (materialise ?provider_meta accounts), grants)
;;

let meta store =
  Or_error.bind (Auth_store.read_entry store Account_meta.key) ~f:(function
    | None -> Ok Account_meta.empty
    | Some json -> Account_meta.of_json json)
;;

let shared_pref store ref_ =
  Or_error.map (meta store) ~f:(fun meta -> Account_meta.shared_pref meta ref_)
;;

let set_shared_pref store ref_ ~f =
  Or_error.ignore_m
    (Auth_store.modify_entry store Account_meta.key ~f:(fun json ->
       let open Or_error.Let_syntax in
       let%map meta =
         match json with
         | None -> Ok Account_meta.empty
         | Some json -> Account_meta.of_json json
       in
       let meta =
         Account_meta.set_shared_pref
           meta
           ref_
           (f (Account_meta.shared_pref meta ref_))
       in
       if Account_meta.is_empty meta
       then None
       else Some (Account_meta.to_json meta)))
;;

let list_all store =
  Or_error.bind (Auth_store.entries store ~prefix:"") ~f:(fun fields ->
    Or_error.all
      (List.map Provider_id.builtins ~f:(fun provider ->
         Or_error.map
           (view ~file:(Auth_store.path store) provider fields)
           ~f:(fun (_, _, accounts) -> provider, accounts))))
;;
