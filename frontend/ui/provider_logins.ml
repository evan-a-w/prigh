open! Core
open! Import

module Login = struct
  type t =
    { provider : P.Auth_status.t
    ; account : P.Auth_status.Account.t
    }
  [@@deriving sexp_of]
end

let str = P.Json.str

let all (auth : P.Auth_status.t list) =
  List.concat_map auth ~f:(fun provider ->
    List.map provider.accounts ~f:(fun account -> { Login.provider; account }))
;;

let title (l : Login.t) = sprintf "%s: %s" l.provider.name l.account.label

let kind (a : P.Auth_status.Account.t) =
  match a.method_ with
  | "oauth" -> "subscription"
  | "api_key" -> "API key"
  | other -> other
;;

let detail (l : Login.t) =
  String.concat
    ~sep:", "
    (List.concat
       [ [ kind l.account ]
       ; (if l.account.default then [ "default" ] else [])
       ; (if l.account.fallback then [ "takes over at usage limits" ] else [])
       ])
;;

let short (a : P.Auth_status.Account.t) =
  if String.length a.label <= 16 then a.label else a.id
;;

let names (l : Login.t) =
  let p = l.provider.provider in
  [ l.account.ref_; p ^ "@" ^ l.account.label; p ^ "@" ^ l.account.id; title l ]
;;

let find ?provider auth arg =
  let arg = String.strip arg in
  let logins =
    List.filter (all auth) ~f:(fun l ->
      Option.for_all provider ~f:(String.equal l.provider.provider))
  in
  let is s = String.Caseless.equal s arg in
  let one = function
    | [ l ] -> Some (Ok l)
    | [] -> None
    | many ->
      Some
        (Error
           (sprintf
              "several logins are called %S: name one by its ref (%s)"
              arg
              (String.concat
                 ~sep:", "
                 (List.map many ~f:(fun (l : Login.t) -> l.account.ref_)))))
  in
  let by f = one (List.filter logins ~f:(fun l -> f l)) in
  match
    List.find_map
      [ (fun l -> List.exists (names l) ~f:is)
      ; (fun (l : Login.t) -> is l.account.label)
      ; (fun (l : Login.t) -> is l.account.id)
      ]
      ~f:by
  with
  | Some result -> result
  | None when List.is_empty logins ->
    Error
      (match provider with
       | Some p -> sprintf "%s has no logins: /login %s signs in" p p
       | None -> "no logins yet: /login signs in to a provider")
  | None when Option.is_some provider ->
    Error
      (sprintf
         "no %s login %S: it has %s (/accounts lists them)"
         (Option.value_exn provider)
         arg
         (String.concat
            ~sep:", "
            (List.map logins ~f:(fun (l : Login.t) -> l.account.label))))
  | None ->
    let close =
      Fuzzy.rank ~query:arg logins ~key:(fun l ->
        title l ^ " " ^ l.account.ref_)
      |> Fn.flip List.take 3
    in
    Error
      (sprintf
         "no login %S%s; /accounts lists them"
         arg
         (match close with
          | [] -> ""
          | close ->
            sprintf
              " (did you mean %s?)"
              (String.concat
                 ~sep:", "
                 (List.map close ~f:(fun (l : Login.t) -> l.account.ref_)))))
;;

let names_login arg = String.mem arg '@' || String.mem (String.strip arg) '/'

let of_session auth (state : P.State.t) =
  let logins =
    List.filter (all auth) ~f:(fun l ->
      String.equal l.provider.provider state.model.provider)
  in
  match state.account with
  | Some id -> List.find logins ~f:(fun l -> String.equal l.account.id id)
  | None ->
    (match logins with
     | [] | [ _ ] -> None
     | logins -> List.find logins ~f:(fun l -> l.account.default))
;;

let split_as words =
  match List.findi words ~f:(fun _ w -> String.equal w "as") with
  | None -> words, None
  | Some (i, _) ->
    let label = String.concat ~sep:" " (List.drop words (i + 1)) in
    ( List.take words i
    , if String.is_empty (String.strip label) then None else Some label )
;;

let login_params auth ~provider ~method_ ~label =
  [ "provider", str provider ]
  @ Option.value_map method_ ~default:[] ~f:(fun m -> [ "method", str m ])
  @
  match label with
  | None -> []
  | Some label ->
    (match find ~provider auth label with
     | Ok l -> [ "account", str l.account.id ]
     | Error _ -> [ "label", str label ])
;;

module Request = struct
  type t =
    { method_ : string
    ; params : (string * P.Json.t) list
    ; done_ : string
    }
  [@@deriving sexp_of]
end

let logout (l : Login.t) =
  { Request.method_ = "logout"
  ; params =
      [ "provider", str l.provider.provider; "account", str l.account.id ]
  ; done_ = sprintf "logged out of %s" (title l)
  }
;;

let set_model_params ~model_key login =
  ("model", str model_key)
  :: Option.value_map login ~default:[] ~f:(fun (l : Login.t) ->
    [ "account", str l.account.id ])
;;

let keep_pin (state : P.State.t option) ~provider =
  match state with
  | Some { account = Some id; model; _ }
    when String.equal model.provider provider -> [ "account", str id ]
  | _ -> []
;;

module Command = struct
  type t =
    | Pick of string
    | Menu of string
    | Default of string
    | Rename of string
    | Fallback of
        { account : string
        ; on : bool
        }
    | Up of string
    | Down of string
    | Remove of string
    | Error of string
  [@@deriving sexp_of, equal]

  let usage =
    "/accounts [QUERY] picks a login; /accounts default|up|down|remove \
     ACCOUNT, /accounts rename ACCOUNT to LABEL, /accounts fallback ACCOUNT \
     on|off"
  ;;

  let parse rest =
    let rest = String.strip rest in
    let word, arg =
      match String.lsplit2 rest ~on:' ' with
      | Some (w, a) -> w, String.strip a
      | None -> rest, ""
    in
    let needs f =
      if String.is_empty arg
      then Error (sprintf "/accounts %s needs a login: %s" word usage)
      else f arg
    in
    match word with
    | "menu" -> needs (fun a -> Menu a)
    | "default" -> needs (fun a -> Default a)
    | "rename" -> needs (fun a -> Rename a)
    | "up" -> needs (fun a -> Up a)
    | "down" -> needs (fun a -> Down a)
    | "remove" | "logout" -> needs (fun a -> Remove a)
    | "fallback" ->
      needs (fun a ->
        match String.rsplit2 a ~on:' ' with
        | Some (account, (("on" | "off") as on)) ->
          Fallback { account = String.strip account; on = String.equal on "on" }
        | _ -> Error ("/accounts fallback ACCOUNT on|off: " ^ usage))
    | _ -> Pick rest
  ;;
end

let update (l : Login.t) params ~done_ =
  Ok
    { Request.method_ = "account_update"
    ; params = ("ref", str l.account.ref_) :: params
    ; done_
    }
;;

let siblings auth (l : Login.t) =
  List.find auth ~f:(fun (p : P.Auth_status.t) ->
    String.equal p.provider l.provider.provider)
  |> Option.value_map ~default:[] ~f:(fun (p : P.Auth_status.t) -> p.accounts)
;;

let neighbour auth (l : Login.t) ~step =
  let accounts = siblings auth l in
  match
    List.findi accounts ~f:(fun _ a -> String.equal a.ref_ l.account.ref_)
  with
  | None -> None
  | Some (i, _) ->
    Option.map
      (List.nth accounts (i + step))
      ~f:(fun account -> { l with account })
;;

let request auth (command : Command.t) =
  let ( let* ) r f = Result.bind r ~f in
  match command with
  | Pick _ | Menu _ -> Error "not a change"
  | Error e -> Error e
  | Default arg ->
    let* l = find auth arg in
    update
      l
      [ "default", P.Json.bool true ]
      ~done_:
        (sprintf "%s is now %s's default login" l.account.label l.provider.name)
  | Rename arg ->
    let account, label =
      match String.substr_index arg ~pattern:" to " with
      | Some i -> String.prefix arg i, String.drop_prefix arg (i + 4)
      | None ->
        (match String.lsplit2 arg ~on:' ' with
         | Some (a, l) -> a, l
         | None -> arg, "")
    in
    let label = String.strip label in
    if String.is_empty label
    then Error "/accounts rename ACCOUNT to LABEL: type the new label"
    else
      let* l = find auth account in
      update
        l
        [ "label", str label ]
        ~done_:(sprintf "renamed %s to %s" (title l) label)
  | Fallback { account; on } ->
    let* l = find auth account in
    update
      l
      [ "fallback", P.Json.bool on ]
      ~done_:
        (if on
         then
           sprintf
             "%s takes over when another %s login hits its usage limit"
             l.account.label
             l.provider.name
         else sprintf "%s no longer takes over at usage limits" l.account.label)
  | Up arg ->
    let* l = find auth arg in
    (match neighbour auth l ~step:(-1) with
     | None -> Error (sprintf "%s is first already" (title l))
     | Some before ->
       update
         l
         [ "before", str before.account.ref_ ]
         ~done_:(sprintf "moved %s up" (title l)))
  | Down arg ->
    let* l = find auth arg in
    (match neighbour auth l ~step:1 with
     | None -> Error (sprintf "%s is last already" (title l))
     | Some after ->
       update
         after
         [ "before", str l.account.ref_ ]
         ~done_:(sprintf "moved %s down" (title l)))
  | Remove arg ->
    let* l = find auth arg in
    Ok (logout l)
;;

module Row = struct
  type t =
    { id : string
    ; label : string
    ; detail : string
    ; marked : bool
    }
  [@@deriving sexp_of]
end

let asks id = String.is_suffix id ~suffix:" "

let in_use (session : P.State.t option) auth (l : Login.t) =
  match session with
  | None -> false
  | Some s ->
    Option.exists (of_session auth s) ~f:(fun u ->
      String.equal u.account.ref_ l.account.ref_)
;;

let add_rows (p : P.Auth_status.t) =
  match p.methods with
  | [ _ ] | [] ->
    [ { Row.id = sprintf "/login %s as " p.provider
      ; label = sprintf "Add a %s login" p.name
      ; detail = "another account, under a label you choose"
      ; marked = false
      }
    ]
  | methods ->
    List.map methods ~f:(fun (m : P.Auth_status.Method.t) ->
      { Row.id = sprintf "/login %s %s as " p.provider m.method_
      ; label = sprintf "Add a %s login (%s)" p.name m.label
      ; detail = "another account, under a label you choose"
      ; marked = false
      })
;;

let rows auth ~session =
  List.map (all auth) ~f:(fun l ->
    { Row.id = "/accounts menu " ^ l.account.ref_
    ; label = title l
    ; detail = detail l ^ "  " ^ l.account.ref_
    ; marked = in_use session auth l
    })
  @ List.concat_map auth ~f:(fun (p : P.Auth_status.t) ->
    if Option.is_some p.custom || List.is_empty p.accounts
    then []
    else add_rows p)
;;

let menu (l : Login.t) ~session =
  let r = l.account.ref_ in
  let row ?(detail = "") id label = { Row.id; label; detail; marked = false } in
  let same_provider =
    Option.exists session ~f:(fun (s : P.State.t) ->
      String.equal s.model.provider l.provider.provider)
  in
  let used =
    Option.exists session ~f:(fun (s : P.State.t) ->
      Option.exists s.account ~f:(String.equal l.account.id))
  in
  let accounts = l.provider.accounts in
  let position =
    List.findi accounts ~f:(fun _ a -> String.equal a.ref_ r)
    |> Option.value_map ~default:0 ~f:fst
  in
  List.concat
    [ (if same_provider && not used
       then
         [ row
             ("/account " ^ r)
             "Use in this session"
             ~detail:"this session's requests go to this login"
         ]
       else [])
    ; (if l.account.default
       then []
       else
         [ row
             ("/accounts default " ^ r)
             "Make default"
             ~detail:"sessions that pick no login use it"
         ])
    ; [ row (sprintf "/accounts rename %s to " r) "Rename…" ]
    ; [ (if l.account.fallback
         then
           row
             (sprintf "/accounts fallback %s off" r)
             "Stop taking over at usage limits"
         else
           row
             (sprintf "/accounts fallback %s on" r)
             "Take over at usage limits"
             ~detail:"when another login of the provider hits its limit")
      ]
    ; (if position > 0 then [ row ("/accounts up " ^ r) "Move up" ] else [])
    ; (if position < List.length accounts - 1
       then [ row ("/accounts down " ^ r) "Move down" ]
       else [])
    ; [ row
          (sprintf "/login %s %s as %s" l.provider.provider l.account.method_ r)
          "Log in again"
      ; row
          ("/logout " ^ r)
          "Log out"
          ~detail:
            (if String.equal l.account.id "main" && List.length accounts > 1
             then "pi uses this login: it loses it"
             else "deletes the stored credential")
      ]
    ]
;;

let provider_name auth provider =
  List.find_map auth ~f:(fun (p : P.Auth_status.t) ->
    Option.some_if (String.equal p.provider provider) p.name)
  |> Option.value ~default:provider
;;

let session_request auth (state : P.State.t) arg =
  let model_key = state.model.key in
  match String.strip arg with
  | "default" ->
    Ok
      { Request.method_ = "set_model"
      ; params = set_model_params ~model_key None
      ; done_ =
          sprintf
            "this session uses %s's default login"
            (provider_name auth state.model.provider)
      }
  | arg ->
    Result.map (find ~provider:state.model.provider auth arg) ~f:(fun l ->
      { Request.method_ = "set_model"
      ; params = set_model_params ~model_key (Some l)
      ; done_ = sprintf "this session uses %s" (title l)
      })
;;

let session_rows auth (state : P.State.t) =
  let logins =
    List.filter (all auth) ~f:(fun l ->
      String.equal l.provider.provider state.model.provider)
  in
  List.map logins ~f:(fun l ->
    { Row.id = "/account " ^ l.account.ref_
    ; label = l.account.label
    ; detail = detail l
    ; marked = Option.exists state.account ~f:(String.equal l.account.id)
    })
  @ [ { Row.id = "/account default"
      ; label = "The provider's default"
      ; detail = "follows /accounts default"
      ; marked = Option.is_none state.account
      }
    ]
;;

let no_choice auth (state : P.State.t) =
  let provider = state.model.provider in
  let n =
    List.count (all auth) ~f:(fun l ->
      String.equal l.provider.provider provider)
  in
  if n >= 2 || Option.is_some state.account
  then None
  else
    Some
      (sprintf
         "%s has %s login: /login %s as LABEL adds another"
         (provider_name auth provider)
         (if n = 0 then "no stored" else "one")
         provider)
;;
