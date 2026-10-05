open! Core
open! Import

type t =
  { name : string
  ; token : string
  }
[@@deriving sexp_of]

let valid_name name =
  (not (String.is_empty name))
  && String.for_all name ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '_' || Char.equal c '-')
;;

let parse_entry ~flag entry =
  match String.lsplit2 entry ~on:'=' with
  | None -> Or_error.errorf "%s entry %S must be NAME=TOKEN" flag entry
  | Some (name, token) ->
    let name = String.strip name in
    let token = String.strip token in
    if not (valid_name name)
    then
      Or_error.errorf
        "%s: bad namespace name %S (use letters, digits, _ and -)"
        flag
        name
    else if String.is_empty token
    then Or_error.errorf "%s: empty token for namespace %S" flag name
    else Ok { name; token }
;;

let first_duplicate l ~f =
  List.find_a_dup l ~compare:(fun a b -> String.compare (f a) (f b))
;;

let parse_spec ?(flag = "-tokens") spec =
  let entries =
    String.split spec ~on:','
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  Or_error.bind
    (Or_error.all (List.map entries ~f:(parse_entry ~flag)))
    ~f:(fun ts ->
      if List.is_empty ts
      then Or_error.errorf "%s: no NAME=TOKEN entries" flag
      else (
        match
          ( first_duplicate ts ~f:(fun t -> t.name)
          , first_duplicate ts ~f:(fun t -> t.token) )
        with
        | Some t, _ -> Or_error.errorf "%s: duplicate namespace %S" flag t.name
        | None, Some t ->
          Or_error.errorf
            "%s: namespace %S reuses another namespace's token"
            flag
            t.name
        | None, None -> Ok ts))
;;

module World = struct
  type t =
    { home : string
    ; sessions_dir : string
    ; store : Auth_store.t
    ; getenv : string -> string option
    }

  let legacy ~home ~auth_file =
    { home
    ; sessions_dir = Session.default_dir ~home
    ; store = Auth_store.create ~path:auth_file
    ; getenv = Sys.getenv
    }
  ;;
end

let provider_key_vars =
  List.concat_map Provider_id.all ~f:Provider_auth.env_vars
;;

let without_provider_keys getenv name =
  if List.mem provider_key_vars name ~equal:String.equal
  then None
  else getenv name
;;

let world t ~home ~legacy_auth_file =
  let world =
    if String.equal t.name "default"
    then World.legacy ~home ~auth_file:legacy_auth_file
    else (
      let home = home ^/ ".prigh/namespaces" ^/ t.name in
      Core_unix.mkdir_p home;
      World.legacy ~home ~auth_file:(home ^/ ".config/prigh/auth.json"))
  in
  { world with getenv = without_provider_keys Sys.getenv }
;;
