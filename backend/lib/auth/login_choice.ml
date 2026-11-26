open! Core
open! Import

type t =
  { ref_ : Account_ref.t
  ; label : string
  ; default : bool
  ; fallback : bool
  }
[@@deriving sexp_of]

let list ?member store provider =
  let open Or_error.Let_syntax in
  let%map own = Provider_accounts.list store provider in
  let own =
    List.map own ~f:(fun (a : Provider_accounts.Account.t) ->
      { ref_ = Own a.id
      ; label = a.label
      ; default = a.default
      ; fallback = a.fallback
      })
  in
  let shared =
    match member with
    | None -> []
    | Some { Account_shares.Member.shares; user } ->
      List.filter_map (Account_shares.shared_with shares ~user) ~f:(fun s ->
        Option.some_if
          (Provider_id.equal s.provider provider && not s.pref.hidden)
          { ref_ = Account_shares.Shared.ref_ s
          ; label = sprintf "%s (from %s)" s.label s.owner
          ; default = false
          ; fallback = s.pref.fallback
          })
  in
  match own, shared with
  | [], first :: rest -> { first with default = true } :: rest
  | _ -> own @ shared
;;

let find ?member store provider name =
  let open Or_error.Let_syntax in
  let%bind choices = list ?member store provider in
  let by f = List.find choices ~f in
  match
    Option.first_some
      (by (fun c -> String.equal (Account_ref.to_string c.ref_) name))
      (by (fun c ->
         match c.ref_ with
         | Own id -> String.equal (Account_id.to_string id) name
         | Shared _ -> false))
  with
  | Some c -> Ok c
  | None ->
    (match by (fun c -> String.Caseless.equal c.label name) with
     | Some c -> Ok c
     | None ->
       Or_error.errorf
         "%s has no account %S: %s"
         (Provider_id.display_name provider)
         name
         (match choices with
          | [] -> "it has none"
          | choices ->
            "it has "
            ^ String.concat
                ~sep:", "
                (List.map choices ~f:(fun c ->
                   sprintf "%s (%s)" c.label (Account_ref.to_string c.ref_)))))
;;
