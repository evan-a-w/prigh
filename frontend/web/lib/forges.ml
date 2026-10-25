open! Core
open! Import

module Accounts = struct
  type t =
    | Loading
    | Failed of string
    | Loaded of Forge_account.t list
  [@@deriving sexp_of, equal]
end

type t =
  { accounts : Accounts.t
  ; signing_out : String.Set.t
  }
[@@deriving sexp_of, equal]

let empty = { accounts = Loading; signing_out = String.Set.empty }

let list t =
  match t.accounts with
  | Loaded accounts -> accounts
  | Loading | Failed _ -> []
;;

let find t host =
  List.find (list t) ~f:(fun (a : Forge_account.t) ->
    String.Caseless.equal a.host host)
;;

let name (a : Forge_account.t) = sprintf "%s (%s)" a.label a.host

let describe (a : Forge_account.t) =
  match a.user, a.method_ with
  | Some user, Some m -> sprintf "signed in as %s (%s)" user m
  | None, Some m -> sprintf "signed in (%s)" m
  | _, None -> "not signed in"
;;

module Login = struct
  type t =
    | Provider of
        { provider : string
        ; method_ : string option
        }
    | Forge of
        { host : string
        ; kind : string option
        ; method_ : string option
        ; again : string
        }
    | Ask_host
    | Bad of string
  [@@deriving sexp_of, equal]
end

let normalize_host host =
  let host = String.strip host in
  let host =
    List.fold [ "https://"; "http://" ] ~init:host ~f:(fun host prefix ->
      String.chop_prefix_if_exists host ~prefix)
  in
  String.lowercase (String.rstrip host ~drop:(Char.equal '/'))
;;

let is_host word = String.mem word '.'

let method_of = function
  | [] -> Ok None
  | [ (("token" | "oauth") as m) ] -> Ok (Some m)
  | _ -> Error ()
;;

let parse_login t words : Login.t =
  let forge ~host ~kind ~rest ~usage =
    match method_of rest with
    | Ok method_ ->
      let host = normalize_host host in
      Login.Forge
        { host
        ; kind
        ; method_
        ; again =
            (match kind with
             | Some "github" when String.equal host "github.com" -> "github"
             | Some kind -> kind ^ " " ^ host
             | None -> host)
        }
    | Error () -> Bad usage
  in
  match words with
  | "github" :: host :: rest when is_host host ->
    forge
      ~host
      ~kind:(Some "github")
      ~rest
      ~usage:"Usage: /login github [HOST] [token]"
  | "github" :: rest ->
    forge
      ~host:"github.com"
      ~kind:(Some "github")
      ~rest
      ~usage:"Usage: /login github [HOST] [token]"
  | [ "forgejo" ] -> Ask_host
  | "forgejo" :: host :: rest ->
    forge
      ~host
      ~kind:(Some "forgejo")
      ~rest
      ~usage:"Usage: /login forgejo HOST [token]"
  | host :: rest when is_host host ->
    (match find t (normalize_host host) with
     | Some a ->
       forge
         ~host:a.host
         ~kind:(Some a.kind)
         ~rest
         ~usage:(sprintf "Usage: /login %s [token]" a.host)
     | None ->
       let host = normalize_host host in
       Bad
         (sprintf
            "No git host %s is set up: /login forgejo %s signs in to a Forgejo \
             or Gitea there, /login github %s to GitHub Enterprise."
            host
            host
            host))
  | provider :: method_ :: _ -> Provider { provider; method_ = Some method_ }
  | [ provider ] -> Provider { provider; method_ = None }
  | [] -> Bad "Usage: /login [provider|github|forgejo HOST]"
;;

let logout_host t arg =
  match String.strip arg with
  | "github" -> Some "github.com"
  | arg when is_host arg ->
    Some
      (Option.value_map
         (find t (normalize_host arg))
         ~default:(normalize_host arg)
         ~f:(fun a -> a.host))
  | _ -> None
;;

let login_items t =
  List.map (list t) ~f:(fun (a : Forge_account.t) ->
    let detail =
      match a.oauth, a.oauth_note with
      | false, Some note when not (Forge_account.signed_in a) ->
        describe a ^ "; with a token: " ^ note
      | _ -> describe a
    in
    Picker.Item.create
      ~id:("forge " ^ a.host)
      ~detail
      ~search:(String.concat ~sep:" " [ a.label; a.host; a.kind; "git" ])
      ~marked:(Forge_account.signed_in a)
      (name a))
  @ [ Picker.Item.create
        ~id:"forge-host"
        ~detail:"a self-hosted Forgejo or Gitea: type its host"
        ~search:"forgejo gitea another host git"
        "Forgejo: another host…"
    ]
;;

let logout_items t =
  List.filter_map (list t) ~f:(fun (a : Forge_account.t) ->
    Option.some_if
      (Forge_account.signed_in a)
      (Picker.Item.create
         ~id:("forge " ^ a.host)
         ~detail:(describe a)
         ~search:(String.concat ~sep:" " [ a.label; a.host; a.kind; "git" ])
         (name a)))
;;
