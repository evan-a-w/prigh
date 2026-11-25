open! Core
open! Import

type t =
  { provider : Provider_id.t
  ; account : Account_ref.t
  ; label : string
  ; fetch :
      ?refused:string
      -> ?valid_for_ms:int
      -> unit
      -> Provider_auth.Resolved.t Or_error.t
  ; record : Usage.t -> cost_usd:float -> unit
  ; mutable last : Provider_auth.Resolved.t Or_error.t option
  }

let sexp_of_t t =
  [%sexp
    { provider : Provider_id.t = t.provider
    ; account : string = Account_ref.to_string t.account
    ; label : string = t.label
    }]
;;

let create
      ~provider
      ~account
      ~label
      ~fetch
      ?(record = fun _ ~cost_usd:_ -> ())
      ()
  =
  { provider; account; label; fetch; record; last = None }
;;

let of_store ~env ?member ?ledger store provider account =
  let open Or_error.Let_syntax in
  let%bind choice =
    Login_choice.find ?member store provider (Account_ref.to_string account)
  in
  let fetch ?refused ?valid_for_ms () =
    match choice.ref_ with
    | Own id ->
      (match%bind
         Provider_auth.resolve
           ~env
           ~getenv:(fun _ -> None)
           ?refused
           ?valid_for_ms
           ~account:id
           store
           provider
       with
       | Some resolved -> Ok resolved
       | None ->
         Or_error.errorf
           "%s's login %S is gone: /login %s logs in again"
           (Provider_id.display_name provider)
           choice.label
           (Provider_id.to_string provider))
    | Shared { owner; id } ->
      (match member with
       | None ->
         Or_error.errorf
           "%s's login %S is shared, and this server has no users to share with"
           owner
           (Account_id.to_string id)
       | Some { Account_shares.Member.shares; user } ->
         Account_shares.resolve
           shares
           ~env
           ?refused
           ?valid_for_ms
           ~recipient:user
           provider
           ~owner
           id)
  in
  let record usage ~cost_usd =
    Account_shares.record_request
      ?member
      ?ledger
      provider
      choice.ref_
      usage
      ~cost_usd
  in
  Ok
    { provider
    ; account = choice.ref_
    ; label = choice.label
    ; fetch
    ; record
    ; last = None
    }
;;

let provider t = t.provider
let account t = t.account
let label t = t.label

let fetch t ?refused ?valid_for_ms () =
  let result = t.fetch ?refused ?valid_for_ms () in
  t.last <- Some result;
  result
;;

let last t = t.last
let record t usage ~cost_usd = t.record usage ~cost_usd
