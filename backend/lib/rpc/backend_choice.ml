open! Core
open! Import
module Source = Web_connect.Check.Source

type t =
  { remote : Backend_address.t
  ; credentials : Web_connect.Credentials.t
  ; source : Source.t
  ; token_source : Source.t option
  ; save : Saved_connection.t option
  }
[@@deriving sexp_of]

module Error = struct
  type t =
    | Unreadable_saved of
        { file : string
        ; error : Core.Error.t
        }
    | No_backend
    | Bad_address of
        { error : Core.Error.t
        ; source : Source.t
        }
  [@@deriving sexp_of]

  let to_string_hum t ~no_backend =
    match t with
    | Unreadable_saved { file; error } ->
      sprintf
        "cannot read the saved connection: %s\n\
         Fix or delete %s, or pass -connect HOST:PORT."
        (Core.Error.to_string_hum error)
        file
    | No_backend -> no_backend
    | Bad_address { error; source } ->
      sprintf
        "-connect: %s%s"
        (Core.Error.to_string_hum error)
        (match source with
         | Flag | Form -> ""
         | Env -> " (from $PRIGH_CONNECT)"
         | Saved file -> sprintf " (saved in %s)" file)
  ;;
end

let env () : Saved_connection.Settings.t =
  let getenv var = Option.filter (Sys.getenv var) ~f:(Fn.non String.is_empty) in
  { connect = getenv "PRIGH_CONNECT"
  ; token = getenv "PRIGH_TOKEN"
  ; user = getenv "PRIGH_USER"
  }
;;

let choose ~home ~(flags : Saved_connection.Settings.t) ~env
  : (t, Error.t) Result.t
  =
  let open Result.Let_syntax in
  let file = Saved_connection.file ~home in
  let%bind saved =
    Saved_connection.load ~home
    |> Result.map_error ~f:(fun error -> Error.Unreadable_saved { file; error })
  in
  let normalize (s : Saved_connection.Settings.t) =
    { s with connect = Option.map s.connect ~f:Backend_address.normalize }
  in
  let flags = normalize flags in
  let env = normalize env in
  let { Saved_connection.Resolved.settings; save } =
    Saved_connection.resolve ~saved ~flags ~env ~local:false
  in
  let%bind address =
    Result.of_option settings.connect ~error:Error.No_backend
  in
  let source : Source.t =
    if Option.is_some flags.connect
    then Flag
    else if Option.is_some env.connect
    then Env
    else Saved file
  in
  let%map remote =
    Backend_address.parse address
    |> Result.map_error ~f:(fun error -> Error.Bad_address { error; source })
  in
  let token_source : Source.t option =
    if Option.is_some flags.token
    then Some Flag
    else if Option.is_some env.token
    then Some Env
    else Option.map settings.token ~f:(fun _ : Source.t -> Saved file)
  in
  { remote =
      Backend_address.with_proxy_cookie
        remote
        (Saved_connection.proxy_cookie saved ~connect:settings.connect)
  ; credentials = { token = settings.token; user = settings.user }
  ; source
  ; token_source
  ; save
  }
;;

let refused_hint t =
  let address = Backend_address.to_string t.remote in
  match t.token_source, t.credentials.user with
  | None, _ ->
    sprintf
      "No token was given, and none is saved for %s: pass the backend's with \
       -token SECRET (or $PRIGH_TOKEN), or sign in once with prigh-tui \
       -connect %s -token SECRET (or prigh web -connect %s -token SECRET), \
       which saves it."
      address
      address
      address
  | Some (Saved file), _ ->
    sprintf
      "The credentials are the ones saved for %s in %s: sign in again with \
       /signin in prigh-tui (or prigh-tui -connect %s -token SECRET [-user \
       NAME]), or pass -token SECRET (and -user NAME)."
      address
      file
      address
  | Some (Flag | Env | Form), None ->
    "Check -token (or $PRIGH_TOKEN) against the backend's; if it serves \
     several users (-tokens), also give -user NAME (or $PRIGH_USER)."
  | Some (Flag | Env | Form), Some _ ->
    "Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against the \
     backend's -tokens NAME=TOKEN (or -host-tokens) entries."
;;

let remember_signin ~home remote (grant : Browser_signin.Grant.t) =
  let open Or_error.Let_syntax in
  let connect = Backend_address.to_string remote in
  match%bind Saved_connection.load ~home with
  | Some saved when String.equal saved.connect connect ->
    let saved =
      match saved.token with
      | Some _ -> { saved with proxy_cookie = Some grant.proxy_cookie }
      | None ->
        { saved with
          proxy_cookie = Some grant.proxy_cookie
        ; token = grant.token
        ; user = grant.user
        }
    in
    Saved_connection.save ~home saved
  | Some _ | None -> Ok ()
;;
